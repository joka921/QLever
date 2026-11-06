// Copyright 2026 The QLever Authors, in particular:
//
// 2026 Johannes Kalmbach <kalmbach@cs.uni-freiburg.de>, UFR
//
// UFR = University of Freiburg, Chair of Algorithms and Data Structures
//
// You may not use this file except in compliance with the Apache 2.0 License,
// which can be found in the `LICENSE` file at the root of the QLever project.

// A self-contained reproducer for the suspected kernel bug behind the fault
// storms on the arm64 GitHub runners (see `REPORT.md` on the
// `arm64-stall-repro` branch): with `CONFIG_PT_RECLAIM`, `MADV_DONTNEED` of a
// whole 2 MiB block frees its (empty) PTE table. `zap_pte_range` then calls
// `pte_free_tlb(tlb, ..., addr)` with `addr == end`, i.e. it adds only the
// first page of the *next* 2 MiB block to the range of the TLB flush. If no
// present PTE of the block was zapped in the same flush (because the table
// was already empty), no address of the block is in the flush range, and on
// arm64 (where `TLBI VAE1IS` only invalidates the walk-cache entries for the
// given address) other CPUs keep a cached PMD entry that points to the freed
// PTE table. An access from such a CPU then walks the freed (and maybe reused)
// page: an endless fault (the kernel sees a valid PTE in the new table, and the
// spurious-fault path only invalidates the last level), or an access to a
// wrong physical page.
//
// Each step: a thread pinned to CPU `TOUCH_CPU` touches a page of a fresh 2 MiB
// block B (which caches the walk to B's PTE table on that CPU), the main thread
// (on another CPU) zaps that page (B's table is now empty, the flush is
// last-level only), then zaps all of B (the table is freed), then populates
// many other blocks (new PTE tables, which reuse freed pages). Then the pinned
// thread touches B again and checks the value. If an access takes more than
// `STALL_SECONDS`, report a STALL with the minor faults of the thread; if a
// value is wrong, report CORRUPTION.
//
// Usage: ptreclaim [<steps> [<skip the first zap: 0 or 1>]]

#define _GNU_SOURCE
#include <errno.h>
#include <pthread.h>
#include <sched.h>
#include <stdatomic.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>
#include <sys/syscall.h>
#include <time.h>
#include <unistd.h>

#define BLOCK (2UL << 20)
#define PAGE 4096UL
#define NUM_BLOCKS 256UL
#define NUM_OTHER_BLOCKS 256UL
#define STALL_SECONDS 2

static char* region;
static char* other;
static _Atomic int command = 0;  // 0 idle, 1 touch, 2 check, 3 exit.
static _Atomic int done = 0;
static _Atomic uint64_t touchValue;
static _Atomic(char*) touchAddress;
static _Atomic int touchTid = 0;
static _Atomic int corrupted = 0;

static void pin(int cpu) {
  cpu_set_t set;
  CPU_ZERO(&set);
  CPU_SET(cpu, &set);
  sched_setaffinity(0, sizeof(set), &set);
}

// The pinned thread: write or check `touchValue` at `touchAddress` on command.
static void* toucher(void* arg) {
  pin((int)(intptr_t)arg);
  touchTid = (int)syscall(SYS_gettid);
  for (;;) {
    int c;
    while ((c = atomic_load(&command)) == 0) {
    }
    volatile uint64_t* p = (volatile uint64_t*)atomic_load(&touchAddress);
    if (c == 1) {
      *p = touchValue;
    } else if (c == 2) {
      uint64_t v = *p;
      if (v != touchValue) {
        fprintf(stderr, "CORRUPTION at %p: 0x%llx instead of 0x%llx\n",
                (void*)p, (unsigned long long)v,
                (unsigned long long)touchValue);
        corrupted = 1;
      }
      // Also write it again (a write fault, as in the storms).
      *p = touchValue + 1;
    } else {
      return NULL;
    }
    atomic_store(&command, 0);
    atomic_fetch_add(&done, 1);
  }
}

// Print the minor faults of the thread `tid`.
static long minorFaults(int tid) {
  char path[64], buf[1024];
  snprintf(path, sizeof(path), "/proc/self/task/%d/stat", tid);
  FILE* f = fopen(path, "r");
  if (f == NULL) {
    return -1;
  }
  size_t n = fread(buf, 1, sizeof(buf) - 1, f);
  fclose(f);
  buf[n] = 0;
  char* s = strrchr(buf, ')');
  long faults = -1;
  // After ") ": state ppid pgrp session tty tpgid flags minflt.
  sscanf(s + 2, "%*c %*d %*d %*d %*d %*d %*u %ld", &faults);
  return faults;
}

// Give a command to the toucher and wait for it, report a stall.
static int run(int c, char* address, uint64_t value) {
  touchAddress = address;
  touchValue = value;
  int before = done;
  atomic_store(&command, c);
  struct timespec start, now;
  clock_gettime(CLOCK_MONOTONIC, &start);
  long faultsBefore = minorFaults(touchTid);
  while (done == before) {
    clock_gettime(CLOCK_MONOTONIC, &now);
    if (now.tv_sec - start.tv_sec >= STALL_SECONDS) {
      long faults = minorFaults(touchTid);
      fprintf(stderr,
              "STALL: the access of the pinned thread to %p takes more than "
              "%d s, %ld minor faults meanwhile\n",
              (void*)address, STALL_SECONDS, faults - faultsBefore);
      // Exit without waiting (the thread may be stuck forever).
      _exit(4);
    }
  }
  return 0;
}

int main(int argc, char** argv) {
  long steps = argc > 1 ? atol(argv[1]) : 200;
  int skipFirstZap = argc > 2 ? atoi(argv[2]) : 0;
  long numCpus = sysconf(_SC_NPROCESSORS_ONLN);
  int touchCpu = numCpus > 1 ? 1 : 0;
  pin(0);
  // Aligned regions without huge pages.
  char* raw = mmap(NULL, (NUM_BLOCKS + 1) * BLOCK, PROT_READ | PROT_WRITE,
                   MAP_PRIVATE | MAP_ANONYMOUS | MAP_NORESERVE, -1, 0);
  char* rawOther =
      mmap(NULL, (NUM_OTHER_BLOCKS + 1) * BLOCK, PROT_READ | PROT_WRITE,
           MAP_PRIVATE | MAP_ANONYMOUS | MAP_NORESERVE, -1, 0);
  if (raw == MAP_FAILED || rawOther == MAP_FAILED) {
    perror("mmap");
    return 1;
  }
  region = (char*)(((uintptr_t)raw + BLOCK - 1) & ~(BLOCK - 1));
  other = (char*)(((uintptr_t)rawOther + BLOCK - 1) & ~(BLOCK - 1));
  madvise(region, NUM_BLOCKS * BLOCK, MADV_NOHUGEPAGE);
  madvise(other, NUM_OTHER_BLOCKS * BLOCK, MADV_NOHUGEPAGE);
  pthread_t thread;
  pthread_create(&thread, NULL, toucher, (void*)(intptr_t)touchCpu);
  while (touchTid == 0) {
  }
  uint64_t value = 0x1234567800000000ULL ^ (uint64_t)getpid();
  for (long step = 0; step < steps; ++step) {
    char* block = region + (step % NUM_BLOCKS) * BLOCK;
    // An offset in the middle of the block, so that the page is neither the
    // first nor the last one.
    char* address = block + ((step * 7919) % 400 + 50) * PAGE + 64;
    ++value;
    // 1. Touch the page from the pinned CPU (caches the walk).
    run(1, address, value);
    // 2. Zap only that page: B's PTE table becomes empty, last-level flush.
    if (!skipFirstZap) {
      madvise((void*)((uintptr_t)address & ~(PAGE - 1)), PAGE, MADV_DONTNEED);
    }
    // 3. Zap all of B: with `PT_RECLAIM`, the empty table is freed.
    madvise(block, BLOCK, MADV_DONTNEED);
    // 4. Populate other blocks (one page each, i.e. one new PTE table each),
    // so that the freed table page is reused, then zap them again (which
    // frees their tables, too, but with present PTEs, i.e. a correct flush).
    for (unsigned long b = 0; b < NUM_OTHER_BLOCKS; ++b) {
      volatile uint64_t* p = (volatile uint64_t*)(other + b * BLOCK + 8 * PAGE);
      *p = value + b;
    }
    // Give RCU time to free the old table (`MMU_GATHER_RCU_TABLE_FREE`).
    usleep(2000);
    for (unsigned long b = 0; b < NUM_OTHER_BLOCKS; ++b) {
      volatile uint64_t* p = (volatile uint64_t*)(other + b * BLOCK + 8 * PAGE);
      if (*p != value + b) {
        fprintf(stderr, "CORRUPTION in other block %lu: 0x%llx\n", b,
                (unsigned long long)*p);
        corrupted = 1;
      }
    }
    // 5. Touch B again from the pinned CPU: write a new value, check it.
    ++value;
    run(1, address, value);
    run(2, address, value);
    madvise(other, NUM_OTHER_BLOCKS * BLOCK, MADV_DONTNEED);
    if (corrupted) {
      return 3;
    }
  }
  atomic_store(&command, 3);
  pthread_join(thread, NULL);
  return 0;
}
