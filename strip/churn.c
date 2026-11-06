// Copyright 2026 The QLever Authors, in particular:
//
// 2026 Johannes Kalmbach <kalmbach@cs.uni-freiburg.de>, UFR
//
// UFR = University of Freiburg, Chair of Algorithms and Data Structures
//
// You may not use this file except in compliance with the Apache 2.0 License,
// which can be found in the `LICENSE` file at the root of the QLever project.

// A synthetic imitation of the memory behavior of one `StripLoopTest` process
// (see `strace` in `REPORT.md`, appendix E): about 160 short-lived threads
// (each with its own 8 MiB stack, which glibc unmaps again when its stack
// cache of 40 MiB is full), in rounds of a few threads at a time, each of
// which allocates, fills, copies, checks, and frees blocks of various sizes
// via `malloc` (run with `jemalloc` via `LD_PRELOAD`, so that freed memory
// is purged with `madvise(MADV_DONTNEED)`).
//
// With a fourth argument `<huge MiB>`, each thread also allocates a block of
// that size, of which it only uses the first 64 KiB (like the first block of a
// `CompressedExternalIdTableSorter` with a memory limit of 4 GB, which is
// reserved but hardly used by a tiny index), and frees it again (`jemalloc`
// returns such huge blocks to the kernel immediately via `madvise`).
//
// Usage: churn <rounds> <threads per round> <max block KiB> [<huge MiB>]
// Exit status 3 and `CORRUPTION` on stderr if a check fails.

#include <pthread.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static size_t maxBlockBytes;
static size_t hugeBytes = 0;
static volatile int corrupted = 0;

// A simple deterministic pseudo-random generator (xorshift).
static uint64_t next(uint64_t* state) {
  uint64_t x = *state;
  x ^= x << 13;
  x ^= x >> 7;
  x ^= x << 17;
  *state = x;
  return x;
}

// Fill `n` words with a pattern derived from `seed`.
static void fill(uint64_t* p, size_t n, uint64_t seed) {
  for (size_t i = 0; i < n; ++i) {
    p[i] = seed * 0x9E3779B97F4A7C15ULL + i;
  }
}

// Check the pattern of `fill`, report the first mismatch.
static void check(const uint64_t* p, size_t n, uint64_t seed,
                  const char* what) {
  for (size_t i = 0; i < n; ++i) {
    if (p[i] != seed * 0x9E3779B97F4A7C15ULL + i) {
      fprintf(stderr,
              "CORRUPTION in %s at word %zu of %zu (address %p): 0x%llx "
              "instead of 0x%llx\n",
              what, i, n, (void*)(p + i), (unsigned long long)p[i],
              (unsigned long long)(seed * 0x9E3779B97F4A7C15ULL + i));
      corrupted = 1;
      return;
    }
  }
}

// The work of one short-lived thread.
static void* work(void* arg) {
  uint64_t state = (uint64_t)(uintptr_t)arg * 2654435761ULL + 1;
  enum { NUM_BLOCKS = 16 };
  uint64_t* blocks[NUM_BLOCKS];
  size_t sizes[NUM_BLOCKS];
  uint64_t seeds[NUM_BLOCKS];
  for (int i = 0; i < NUM_BLOCKS; ++i) {
    // Mostly small blocks, some large ones (like the buffers of the index
    // build).
    size_t bytes = (next(&state) % 8 == 0)
                       ? 64 * 1024 + next(&state) % maxBlockBytes
                       : 16 + next(&state) % 16384;
    sizes[i] = bytes / 8;
    seeds[i] = next(&state);
    blocks[i] = malloc(sizes[i] * 8 + 8);
    fill(blocks[i], sizes[i], seeds[i]);
  }
  // Copy each block into a fresh one, check it, and free the original.
  for (int i = 0; i < NUM_BLOCKS; ++i) {
    uint64_t* copy = malloc(sizes[i] * 8 + 8);
    memcpy(copy, blocks[i], sizes[i] * 8);
    free(blocks[i]);
    check(copy, sizes[i], seeds[i], "copy");
    blocks[i] = copy;
  }
  for (int i = 0; i < NUM_BLOCKS; ++i) {
    check(blocks[i], sizes[i], seeds[i], "block");
    free(blocks[i]);
  }
  if (hugeBytes > 0) {
    enum { HUGE_USED_WORDS = 64 * 1024 / 8 };
    uint64_t seed = next(&state);
    uint64_t* huge = malloc(hugeBytes);
    // Copy into it (like `pushBlock`), then check it.
    uint64_t* source = malloc(HUGE_USED_WORDS * 8);
    fill(source, HUGE_USED_WORDS, seed);
    memcpy(huge, source, HUGE_USED_WORDS * 8);
    free(source);
    check(huge, HUGE_USED_WORDS, seed, "huge");
    free(huge);
  }
  return NULL;
}

int main(int argc, char** argv) {
  if (argc != 4 && argc != 5) {
    fprintf(stderr,
            "Usage: %s <rounds> <threads per round> <max block KiB> "
            "[<huge MiB>]\n",
            argv[0]);
    return 1;
  }
  int rounds = atoi(argv[1]);
  int threadsPerRound = atoi(argv[2]);
  maxBlockBytes = (size_t)atoi(argv[3]) * 1024;
  if (argc == 5) {
    hugeBytes = (size_t)atoi(argv[4]) * 1024 * 1024;
  }
  uintptr_t id = 0;
  for (int r = 0; r < rounds; ++r) {
    pthread_t threads[256];
    for (int t = 0; t < threadsPerRound; ++t) {
      pthread_create(&threads[t], NULL, work, (void*)++id);
    }
    // Some work in the main thread, too.
    work((void*)++id);
    for (int t = 0; t < threadsPerRound; ++t) {
      pthread_join(threads[t], NULL);
    }
  }
  return corrupted ? 3 : 0;
}
