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
// Usage: repro <memmove|zstd|mixed|pool> <numThreads> <numIterationsPerThread>
// In the `pool` mode, the copies and compressions run as tasks on a thread
// pool, see `runPool`.
// Exit status: 0 on success, 3 on a corruption, 2 on a usage error.

#include <pthread.h>
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

int main(int argc, char** argv) {
  if (argc != 4) {
    fprintf(stderr, "Usage: %s <memmove|zstd|mixed|pool> <numThreads> <numIterations>\n",
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
