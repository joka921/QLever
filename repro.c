// Copyright 2026 The QLever Authors, in particular:
//
// 2026 Johannes Kalmbach <kalmbach@cs.uni-freiburg.de>, UFR
//
// UFR = University of Freiburg, Chair of Algorithms and Data Structures
//
// You may not use this file except in compliance with the Apache 2.0 License,
// which can be found in the `LICENSE` file at the root of the QLever project.

// A minimal workload that mimics the threads of the QLever unit tests that
// stalled (and corrupted memory) on the arm64 GitHub runners with the kernel
// `7.0.0-1017-azure`: a short-lived process starts a few threads, each of which
// copies between freshly allocated buffers via `memmove` (which is glibc's
// `__memmove_sve` on these CPUs) and/or compresses them via `ZSTD_compress`.
// Every copy is verified, so a lost write is reported as a corruption.
//
// Usage: repro <memmove|zstd|mixed|pool|madvfree|madvdontneed> <numThreads>
//              <numIterationsPerThread>
//        repro ptzap <numTouchers> <seconds>
// The `ptzap` mode targets a stale page-walk cache after page tables are
// freed, see `runPtZap`.
// In the `pool` mode, the copies and compressions run as tasks on a thread
// pool, see `runPool`. In the `madv...` modes, the threads share a pool of
// chunks of one big mapping and hand them back to the kernel via `madvise`
// after each use (like `jemalloc` does with freed memory), see `runMadvise`.
// Exit status: 0 on success, 3 on a corruption, 2 on a usage error.

#define _GNU_SOURCE
#include <pthread.h>
#include <sched.h>
#include <unistd.h>
#include <sys/mman.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <zstd.h>

static int useMemmove = 0;
static int useZstd = 0;
static long numIterations = 0;

// Call `memmove` through a `volatile` pointer, so that the compiler can't
// replace or inline it.
static void* (*volatile memmovePtr)(void*, const void*, size_t) = memmove;

// Return a pseudo-random number (xorshift).
static uint64_t nextRandom(uint64_t* state) {
  uint64_t x = *state;
  x ^= x << 13;
  x ^= x >> 7;
  x ^= x << 17;
  *state = x;
  return x;
}

// Return a size between 64 bytes and 4 MiB with a roughly log-uniform
// distribution, so that both small copies (inside the heap) and large ones
// (into freshly `mmap`ed memory, which page faults) occur.
static size_t randomSize(uint64_t* state) {
  size_t shift = 6 + nextRandom(state) % 17;
  size_t size = (size_t)1 << shift;
  return size + nextRandom(state) % size;
}

// Copy and/or compress a buffer of a random size once, and verify the result.
static void doOneIteration(uint64_t* stateArg) {
  uint64_t state = *stateArg;
  {
    size_t size = randomSize(&state);
    unsigned char* source = malloc(size);
    unsigned char* target = malloc(size);
    if (source == NULL || target == NULL) {
      fprintf(stderr, "allocation of %zu bytes failed\n", size);
      exit(4);
    }
    // Fill the source with a pattern that depends on the position, so that a
    // copy that is lost or lands at the wrong offset is detected.
    uint64_t seed = nextRandom(&state);
    for (size_t j = 0; j + 8 <= size; j += 8) {
      uint64_t value = seed + j;
      memcpy(source + j, &value, 8);
    }
    for (size_t j = size & ~(size_t)7; j < size; ++j) {
      source[j] = (unsigned char)(seed + j);
    }
    if (useMemmove) {
      memmovePtr(target, source, size);
      if (memcmp(target, source, size) != 0) {
        size_t j = 0;
        while (target[j] == source[j]) {
          ++j;
        }
        fprintf(stderr,
                "CORRUPTION: memmove of %zu bytes from %p to %p differs at "
                "offset %zu\n",
                size, (void*)source, (void*)target, j);
        exit(3);
      }
    }
    if (useZstd) {
      size_t bound = ZSTD_compressBound(size);
      void* compressed = malloc(bound);
      size_t compressedSize = ZSTD_compress(compressed, bound, source, size, 1);
      if (ZSTD_isError(compressedSize)) {
        fprintf(stderr, "ZSTD_compress: %s\n",
                ZSTD_getErrorName(compressedSize));
        exit(4);
      }
      size_t decompressedSize =
          ZSTD_decompress(target, size, compressed, compressedSize);
      if (decompressedSize != size || memcmp(target, source, size) != 0) {
        fprintf(stderr, "CORRUPTION: zstd round trip of %zu bytes differs\n",
                size);
        exit(3);
      }
      free(compressed);
    }
    free(source);
    free(target);
  }
  *stateArg = state;
}

// The work of a single thread, see the comment at the top of the file.
static void* worker(void* arg) {
  uint64_t state = (uint64_t)(uintptr_t)arg * 0x9E3779B97F4A7C15ull ^
                   (uint64_t)time(NULL) ^ 0x1234567ull;
  for (long i = 0; i < numIterations; ++i) {
    doOneIteration(&state);
  }
  return NULL;
}

// A minimal thread pool like the one of Boost.Asio: the workers sleep on a
// condition variable until a task is posted, run it, and sleep again. The main
// thread posts the tasks one by one, so the workers are constantly woken up and
// put to sleep (which saves and restores their FP/SIMD/SVE state).
static pthread_mutex_t poolMutex = PTHREAD_MUTEX_INITIALIZER;
static pthread_cond_t poolHasTask = PTHREAD_COND_INITIALIZER;
static pthread_cond_t poolTaskDone = PTHREAD_COND_INITIALIZER;
static long numPostedTasks = 0;
static long numStartedTasks = 0;
static long numFinishedTasks = 0;
static int poolDone = 0;

static void* poolWorker(void* arg) {
  uint64_t state = (uint64_t)(uintptr_t)arg * 0x9E3779B97F4A7C15ull ^
                   (uint64_t)time(NULL) ^ 0x7654321ull;
  pthread_mutex_lock(&poolMutex);
  for (;;) {
    while (!poolDone && numStartedTasks == numPostedTasks) {
      pthread_cond_wait(&poolHasTask, &poolMutex);
    }
    if (numStartedTasks == numPostedTasks) {
      break;
    }
    ++numStartedTasks;
    pthread_mutex_unlock(&poolMutex);
    doOneIteration(&state);
    pthread_mutex_lock(&poolMutex);
    ++numFinishedTasks;
    pthread_cond_signal(&poolTaskDone);
  }
  pthread_mutex_unlock(&poolMutex);
  return NULL;
}

// Run `numTasks` tasks on a pool of `numThreads` workers, with at most
// `numThreads / 2` tasks in flight, so that some workers are always idle.
static void runPool(long numThreads, long numTasks) {
  pthread_t* threads = calloc((size_t)numThreads, sizeof(pthread_t));
  for (long t = 0; t < numThreads; ++t) {
    pthread_create(&threads[t], NULL, poolWorker, (void*)(uintptr_t)(t + 1));
  }
  long maxInFlight = numThreads / 2 > 0 ? numThreads / 2 : 1;
  pthread_mutex_lock(&poolMutex);
  for (long i = 0; i < numTasks; ++i) {
    while (numPostedTasks - numFinishedTasks >= maxInFlight) {
      pthread_cond_wait(&poolTaskDone, &poolMutex);
    }
    ++numPostedTasks;
    pthread_cond_signal(&poolHasTask);
  }
  poolDone = 1;
  pthread_cond_broadcast(&poolHasTask);
  pthread_mutex_unlock(&poolMutex);
  for (long t = 0; t < numThreads; ++t) {
    pthread_join(threads[t], NULL);
  }
  free(threads);
}

// The `madv...` modes: `numChunks` chunks of `CHUNK_SIZE` bytes in one big
// anonymous mapping, a stack of the currently unused chunks, and the advice
// with which a chunk is returned to the kernel after each use.
// `CHUNK_SIZE` can be set via the environment variable `REPRO_CHUNK_KB`
// (default 1024). The chunks are aligned to 2 MiB if their size is a multiple
// of 2 MiB, so that `madvise` then empties whole page tables (which the kernel
// may free).
static size_t CHUNK_SIZE = (size_t)1 << 20;
static unsigned char* chunkBase = NULL;
static long* freeChunks = NULL;
static long numFreeChunks = 0;
static pthread_mutex_t chunkMutex = PTHREAD_MUTEX_INITIALIZER;
static int madviseAdvice = 0;

// Take an unused chunk, fill a random part of it from a thread-local source
// buffer via `memmove` (so that it write faults on a page that the kernel may
// have freed or not), verify it, return the whole chunk to the kernel via
// `madvise`, and put the chunk back, so that it is reused by an arbitrary
// thread (on an arbitrary CPU).
static void* madviseWorker(void* arg) {
  uint64_t state = (uint64_t)(uintptr_t)arg * 0x9E3779B97F4A7C15ull ^
                   (uint64_t)time(NULL) ^ 0xabcdefull;
  unsigned char* source = malloc(CHUNK_SIZE);
  for (long i = 0; i < numIterations; ++i) {
    pthread_mutex_lock(&chunkMutex);
    long chunk = freeChunks[--numFreeChunks];
    pthread_mutex_unlock(&chunkMutex);
    unsigned char* target = chunkBase + (size_t)chunk * CHUNK_SIZE;
    size_t size = 4096 + nextRandom(&state) % (CHUNK_SIZE - 4096);
    uint64_t seed = nextRandom(&state);
    for (size_t j = 0; j + 8 <= size; j += 8) {
      uint64_t value = seed + j;
      memcpy(source + j, &value, 8);
    }
    memmovePtr(target, source, size);
    if (memcmp(target, source, size) != 0) {
      size_t j = 0;
      while (target[j] == source[j]) {
        ++j;
      }
      fprintf(stderr,
              "CORRUPTION: memmove of %zu bytes into chunk %ld differs at "
              "offset %zu\n",
              size, chunk, j);
      exit(3);
    }
    if (madvise(target, CHUNK_SIZE, madviseAdvice) != 0) {
      perror("madvise");
      exit(4);
    }
    pthread_mutex_lock(&chunkMutex);
    freeChunks[numFreeChunks++] = chunk;
    pthread_mutex_unlock(&chunkMutex);
  }
  free(source);
  return NULL;
}

static void runMadvise(long numThreads) {
  const char* chunkKb = getenv("REPRO_CHUNK_KB");
  if (chunkKb != NULL) {
    CHUNK_SIZE = (size_t)atol(chunkKb) << 10;
  }
  long numChunks = 2 * numThreads;
  size_t alignment = (size_t)2 << 20;
  unsigned char* mapping =
      mmap(NULL, (size_t)numChunks * CHUNK_SIZE + alignment,
           PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
  if (mapping == MAP_FAILED) {
    perror("mmap");
    exit(4);
  }
  chunkBase = (unsigned char*)(((uintptr_t)mapping + alignment - 1) &
                               ~(uintptr_t)(alignment - 1));
  freeChunks = calloc((size_t)numChunks, sizeof(long));
  for (long c = 0; c < numChunks; ++c) {
    freeChunks[numFreeChunks++] = c;
  }
  pthread_t* threads = calloc((size_t)numThreads, sizeof(pthread_t));
  for (long t = 0; t < numThreads; ++t) {
    pthread_create(&threads[t], NULL, madviseWorker, (void*)(uintptr_t)(t + 1));
  }
  for (long t = 0; t < numThreads; ++t) {
    pthread_join(threads[t], NULL);
  }
  free(threads);
}

// The `ptzap` mode. `numTouchers` threads, each pinned to its own CPU, touch
// (write and read back) one word of their own in each of `PTZAP_CHUNKS`
// chunks of 2 MiB (each covered by exactly one page table), round robin. One
// zapper thread, pinned to the last CPU, repeatedly empties a random chunk
// via `madvise(MADV_DONTNEED)`, which may free its page table, while the
// touchers keep running on their CPUs (and thus keep their caches of the page
// table walks). Each chunk has a read-write lock, so that a touch and a zap
// never overlap. A word must then always read back as written, and before a
// write, it must contain either 0 (zapped) or the value that this thread
// wrote last. Anything else is a corruption. A toucher that makes no progress
// for 5 seconds is a stall (the storms of page faults on the arm64 runners).
// Exit status: 0 on success, 3 on a corruption, 5 on a stall.
#define PTZAP_CHUNK ((size_t)2 << 20)
#define PTZAP_CHUNKS 64
static unsigned char* ptzapBase = NULL;
static pthread_rwlock_t ptzapLocks[PTZAP_CHUNKS];
static volatile long ptzapProgress[64];
static volatile int ptzapDone = 0;

// Return the word of toucher `t` in chunk `c` (each toucher has its own 4 KiB
// page in each chunk).
static volatile uint64_t* ptzapWord(long t, long c) {
  return (volatile uint64_t*)(ptzapBase + (size_t)c * PTZAP_CHUNK +
                              ((size_t)t * 4096 + 8 * (size_t)c) %
                                  PTZAP_CHUNK);
}

static void pinToCpu(long cpu) {
  cpu_set_t set;
  CPU_ZERO(&set);
  CPU_SET((int)(cpu % sysconf(_SC_NPROCESSORS_ONLN)), &set);
  pthread_setaffinity_np(pthread_self(), sizeof(set), &set);
}

static void* ptzapToucher(void* arg) {
  long t = (long)(uintptr_t)arg;
  if (getenv("PTZAP_NO_PIN") == NULL) {
    pinToCpu(t);
  }
  uint64_t lastWritten[PTZAP_CHUNKS] = {0};
  uint64_t counter = (uint64_t)(t + 1) << 48;
  while (!ptzapDone) {
    for (long c = 0; c < PTZAP_CHUNKS && !ptzapDone; ++c) {
      pthread_rwlock_rdlock(&ptzapLocks[c]);
      volatile uint64_t* word = ptzapWord(t, c);
      uint64_t before = *word;
      if (before != 0 && before != lastWritten[c]) {
        fprintf(stderr,
                "CORRUPTION: toucher %ld chunk %ld at %p: found %#llx, "
                "expected 0 or %#llx\n",
                t, c, (void*)word, (unsigned long long)before,
                (unsigned long long)lastWritten[c]);
        exit(3);
      }
      uint64_t value = ++counter;
      *word = value;
      uint64_t after = *word;
      if (after != value) {
        fprintf(stderr,
                "CORRUPTION: toucher %ld chunk %ld at %p: wrote %#llx, read "
                "%#llx\n",
                t, c, (void*)word, (unsigned long long)value,
                (unsigned long long)after);
        exit(3);
      }
      lastWritten[c] = value;
      pthread_rwlock_unlock(&ptzapLocks[c]);
      ptzapProgress[t]++;
    }
  }
  return NULL;
}

static void* ptzapZapper(void* arg) {
  long cpu = (long)(uintptr_t)arg;
  if (getenv("PTZAP_NO_PIN") == NULL) {
    pinToCpu(cpu);
  }
  uint64_t state = (uint64_t)time(NULL) ^ 0x5a5a5aull;
  while (!ptzapDone) {
    long c = (long)(nextRandom(&state) % PTZAP_CHUNKS);
    pthread_rwlock_wrlock(&ptzapLocks[c]);
    if (madvise(ptzapBase + (size_t)c * PTZAP_CHUNK, PTZAP_CHUNK,
                MADV_DONTNEED) != 0) {
      perror("madvise");
      exit(4);
    }
    pthread_rwlock_unlock(&ptzapLocks[c]);
  }
  return NULL;
}

static void runPtZap(long numTouchers, long seconds) {
  if (numTouchers > 64) {
    numTouchers = 64;
  }
  size_t size = PTZAP_CHUNKS * PTZAP_CHUNK;
  unsigned char* mapping = mmap(NULL, size + PTZAP_CHUNK, PROT_READ | PROT_WRITE,
                                MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
  if (mapping == MAP_FAILED) {
    perror("mmap");
    exit(4);
  }
  ptzapBase = (unsigned char*)(((uintptr_t)mapping + PTZAP_CHUNK - 1) &
                               ~(uintptr_t)(PTZAP_CHUNK - 1));
  for (long c = 0; c < PTZAP_CHUNKS; ++c) {
    pthread_rwlock_init(&ptzapLocks[c], NULL);
  }
  pthread_t touchers[64];
  pthread_t zapper;
  for (long t = 0; t < numTouchers; ++t) {
    pthread_create(&touchers[t], NULL, ptzapToucher, (void*)(uintptr_t)t);
  }
  pthread_create(&zapper, NULL, ptzapZapper, (void*)(uintptr_t)numTouchers);
  long lastProgress[64] = {0};
  long stalledSeconds[64] = {0};
  for (long second = 0; second < seconds; ++second) {
    sleep(1);
    for (long t = 0; t < numTouchers; ++t) {
      long progress = ptzapProgress[t];
      stalledSeconds[t] = progress == lastProgress[t] ? stalledSeconds[t] + 1 : 0;
      lastProgress[t] = progress;
      if (stalledSeconds[t] >= 5) {
        fprintf(stderr, "STALL: toucher %ld made no progress for 5 s\n", t);
        char path[64];
        snprintf(path, sizeof(path), "/proc/%d/task", getpid());
        char command[256];
        snprintf(command, sizeof(command),
                 "for T in %s/*; do echo \"$T $(cut -d ' ' -f 3,10,14,15,39 "
                 "$T/stat)\"; done >&2",
                 path);
        if (system(command) != 0) {
          perror("system");
        }
        exit(5);
      }
    }
  }
  ptzapDone = 1;
  for (long t = 0; t < numTouchers; ++t) {
    pthread_join(touchers[t], NULL);
  }
  pthread_join(zapper, NULL);
}

int main(int argc, char** argv) {
  if (argc != 4) {
    fprintf(stderr, "Usage: %s <memmove|zstd|mixed|pool|madvfree|madvdontneed> <numThreads> <numIterations>\n",
            argv[0]);
    return 2;
  }
  if (strcmp(argv[1], "memmove") == 0) {
    useMemmove = 1;
  } else if (strcmp(argv[1], "zstd") == 0) {
    useZstd = 1;
  } else if (strcmp(argv[1], "mixed") == 0) {
    useMemmove = 1;
    useZstd = 1;
  } else if (strcmp(argv[1], "pool") == 0) {
    useMemmove = 1;
    useZstd = 1;
    runPool(atol(argv[2]), atol(argv[2]) * atol(argv[3]));
    return 0;
  } else if (strcmp(argv[1], "ptzap") == 0) {
    runPtZap(atol(argv[2]), atol(argv[3]));
    return 0;
  } else if (strcmp(argv[1], "madvfree") == 0 ||
             strcmp(argv[1], "madvdontneed") == 0) {
    madviseAdvice = strcmp(argv[1], "madvfree") == 0 ? MADV_FREE : MADV_DONTNEED;
    numIterations = atol(argv[3]);
    runMadvise(atol(argv[2]));
    return 0;
  } else {
    fprintf(stderr, "Unknown mode %s\n", argv[1]);
    return 2;
  }
  long numThreads = atol(argv[2]);
  numIterations = atol(argv[3]);
  pthread_t* threads = calloc((size_t)numThreads, sizeof(pthread_t));
  for (long t = 0; t < numThreads; ++t) {
    pthread_create(&threads[t], NULL, worker, (void*)(uintptr_t)(t + 1));
  }
  for (long t = 0; t < numThreads; ++t) {
    pthread_join(threads[t], NULL);
  }
  free(threads);
  return 0;
}
