# Spurious stalls and corruptions of the QLever tests on the arm64 runners

Status: 2026-11-06, 02:15 UTC. Related: ad-freiburg/qlever PR #3892 (diagnostics
launcher), this branch (`arm64-stall-repro` on `joka921/QLever`) for the
reproducers.

## Summary

1. **The trigger is the runner kernel, not a QLever change.** All failures
   (POS corruption, Bus error, `unreachable` in `Datatype.h`) and all long
   stalls of the arm64 Docker build ran on `7.0.0-1017-azure`. On
   `6.17.0-1022-azure`, 0 of 148 master builds had any test taking more than
   10 s; on `7.0.0-1017-azure`, 17 of 48 did. For the same code (master
   2026-10-19 to 10-28): 0/56 vs. 6/16, Fisher p ≈ 3e-12.
2. **A stall is a storm of spurious page faults.** One thread runs a plain
   `memmove` (`__memmove_sve`) or `ZSTD_compress` and takes about 4 million
   minor page faults per second. 85 % of its CPU time is system time, its RSS
   doesn't change, and the stall ends as soon as gdb attaches. The kernel
   apparently "handles" the same fault over and over without making progress,
   which on arm64 means a stale TLB entry that the spurious-fault fixup
   doesn't remove.
3. **Stalls and corruptions have the same cause.** Both happen in the same
   tests, in the same code path (feeding the POS permutation via the external
   sorter and `CompressedRelationWriter`), in the same jobs, and one of the
   corruptions below occurred during a stall of the same test. The corrupt
   values are leftover heap bytes or zeros in exactly one column, i.e. writes
   that went to the wrong physical page (stale TLB entry), not a logic error.
4. **Undefined behavior in QLever is very unlikely.** A `memmove` with bad
   arguments crashes or copies garbage, but it can't produce millions of page
   faults on a flat RSS; unrelated code (`ZSTD_compress`) stalls the same way;
   the copies are small; and the same code never stalls on 6.17.
5. **Prime suspect:** Huang Ying, "arm64, tlbflush: don't TLBI broadcast if
   page reused in write fault" (arm64 tree November 2025, so in 7.0, not in
   6.17). It replaces broadcast TLB invalidations by local ones plus a fixup
   in the spurious-fault path, which is exactly the path that loops, possibly
   in combination with Hyper-V (the runners are Hyper-V guests). Second
   candidate: the TLBI level-hint commit `752a0d1d483e9` (known to cause
   spurious-fault loops, fixed March 2026, but for huge pages, which aren't
   used here). Neither is confirmed.
6. **Reproducer:** building only five QLever test binaries (about 20 minutes)
   and looping them via `ctest` reproduces both the stalls (about one per two
   minutes) and the **corruption (after 0.5 to 12 minutes of testing)**.
   Purely synthetic workloads with glibc `malloc` (about 274,000 processes of
   `memmove`/`zstd` stress, thread pools, memory pressure) reproduced nothing.
7. **What the trigger is not:** swap (round 3, no swap, still corrupt) and
   memory compaction / page migration (round 4: with
   `vm.compaction_proactiveness=0` and only 1k page migrations, stall plus
   corruption in the first iteration; with forced compaction and 30 million
   migrations, about the default rate).
8. **Transparent huge pages (THP) matter, although none are used.** With
   THP set to `never` (instead of the runners' default `madvise`) and its
   defragmentation off, two jobs had 1 stall in 341 iterations (3 hours) and
   no corruption, whereas with the default settings, every job had a stall
   about every 4 iterations and a corruption after 1 to 27 iterations.
   `thp_fault_alloc` and `thp_collapse_alloc` stay 0 in all jobs, so the
   setting must change some other kernel path. A guess: the freeing of empty
   page tables after `madvise(MADV_DONTNEED)`, where a stale entry in the
   page-walk cache would explain both symptoms. Which of the three THP
   settings matters is still to be checked.
9. **`jemalloc`'s purging (returning memory via `madvise`) is the trigger:**
   with `MALLOC_CONF=dirty_decay_ms:-1,muzzy_decay_ms:-1` (never purge),
   0 stalls in 188 iterations (90 min) and no corruption; QLever built
   without `jemalloc` (glibc `malloc`): 0 stalls in 202 iterations. With
   immediate purging (`MALLOC_CONF=dirty_decay_ms:0,muzzy_decay_ms:0`),
   corruption came in iteration 3. (`jemalloc` is not at fault: it only uses
   legal system calls, mostly `madvise`; but its pattern of returning memory
   and reusing it from other threads is what triggers the kernel bug.)
10. **A stall doesn't end by itself, and `SIGSTOP`/`SIGCONT` ends it.** The stalls of all failing tests ended
   exactly when `run-test-with-diagnostics.sh` started its diagnostics: 3002
   ms and 3045 ms with a threshold of 3 s, 10004 ms with 10 s (PR #3892 run
   42101074942 attempt 4, `Datatype.h:115` unreachable in
   `GroupByOptimizations...NamedGraphDuplicates`), and in CI without the
   launcher, stalls lasted up to 9844 s. The corruption shows up right after
   the stall ends. So something the launcher does from outside (reading
   `/proc/<pid>/...`, `ps`, `SIGSTOP`) ends the fault loop, possibly by
   completing the stuck write in the wrong place. Round 9 caught a storm
   live: reading any `/proc/<pid>` file (even its memory) leaves the 4
   million faults/s unchanged, `SIGSTOP` + `SIGCONT` ends it at once, and
   the test then fails with the corruption. **Round 10: merely moving the
   thread to another CPU (via its affinity) ends every storm at once, while
   pinning it to its current CPU doesn't.** So the page table is fine (the
   other CPU uses it without faulting), but one CPU keeps faulting on it:
   state local to that CPU (most likely a stale TLB entry) that the
   kernel's spurious-fault handling (a local TLB invalidation) doesn't
   remove. This is a kernel or hypervisor bug, not a bug of QLever.
11. **Short-lived processes are needed** (round 8 vs. round 6): building the
   3-triple index over and over in long-running processes never stalled or
   corrupted (96,000 builds), the same with a fresh process per build did
   both. Together with 10., a candidate mechanism is the ASID rollover of
   arm64 (each new address space needs a new ASID; after a rollover, each
   CPU flushes its TLB only at its next context switch): a CPU that keeps
   running one thread without a context switch could keep entries that a
   by-ASID invalidation doesn't hit. Round 12 checks this.

## Mitigations

| Mitigation | Status |
|---|---|
| Rerun failed arm64 jobs; classify via the launcher of #3892 (minor faults/s, system time) | works, but costs time and leaves a small chance of silently corrupt test results |
| Wait for a fixed `linux-azure` kernel / runner image (the 6.17 image was fine) | not under our control; report upstream (Ubuntu `linux-azure` Launchpad, GitHub `runner-images`) once a small reproducer exists |
| `vm.compaction_proactiveness=0` on the host | **does not help** (round 4) |
| **Disable THP on the host** (`echo never > /sys/kernel/mm/transparent_hugepage/enabled`, `.../defrag`, `.../khugepaged/defrag` = 0) before the Docker build | **promising**: 1 stall, 0 corruptions in 341 iterations (2 jobs); costs nothing for QLever's tests, since no THP were used anyway |
| Build the arm64 test binaries without `jemalloc` (e.g. `apt-get remove libjemalloc-dev` before `cmake` in the Docker build, only for the tests) | **works** in round 5 (0 stalls in 202 iterations), but then the tests don't use the allocator of the release binaries |
| **`MALLOC_CONF=dirty_decay_ms:-1,muzzy_decay_ms:-1` for the test step of the Docker build** (`jemalloc` never returns memory to the kernel; the tests are short-lived, so the memory cost is irrelevant) | **works** in round 5 (0 stalls in 188 iterations); **recommended**: one environment variable, keeps `jemalloc`, no host changes |
| Run the arm64 tests outside the Docker build or on amd64 only | avoids the symptom only if the trigger is specific to the build; not tested |

## Next steps

- Narrow the reproducer to one test binary and one test filter, then to a
  standalone program that mimics that test's memory access pattern (see
  appendix D), to report the bug upstream (Ubuntu `linux-azure`, arm64
  maintainers) with a reproducer.

---

## Appendix A: Occurrences in CI

Job logs via `gh api repos/ad-freiburg/qlever/actions/jobs/<id>/logs`.

| Date  | Where     | Test                                                      | Symptom                                  |
|-------|-----------|-----------------------------------------------------------|------------------------------------------|
| 10-22 | master    | TransitivePath `graphVariableEmptyPathWithRestrictedGraphs` | Bus error after 787 s                  |
| 10-28 | master    | OptionalJoin `rightIsScanWithoutVariables`                | POS subject = heap ASCII, 2392 s         |
| 10-31 | master    | same TransitivePath test                                  | POS subject garbage, 1011 s              |
| 11-04 | master    | JoinTestParametrized `joinTwoScansWithDifferentGraphs/false` | POS pattern column garbage, 0.14 s    |
| 11-04 | master    | GroupByOptimizations ...`NamedGraphDuplicates`            | `Datatype.h:115` unreachable, 356 s      |
| 11-05 | PR #3878  | `joinTwoScansWithDifferentGraphs/false`                   | Bus error after 37 s                     |

The same jobs contain stalls of 387 to 4340 s in tests that pass; healthy
tests take at most 5 s. All affected tests build small test indexes from NQuads
with the same triple in several graphs, and only POS (the "twin" permutation
written via `twinRelationSorter_`) is corrupt, always in a single column.

## Appendix B: Runner and kernel

- `lscpu`: ARM Neoverse-N2 r0p0, 4 CPUs, SVE with 128-bit vectors, Hyper-V
  guest (build 10.0.26102.3704, `hv_balloon`), 16 GB RAM, 3 GB swap.
- `dmesg` only reports errata 2067961/2054223; THP `enabled=madvise`, all
  mTHP sizes `never`.
- Kernel correlation (arm64 job of the Docker workflow on master):
  `6.17.0-1022-azure` 0/148 builds with a test > 10 s,
  `7.0.0-1017-azure` 17/48 (stalls up to 9844 s).
- Considered and less likely: Cobalt 100 TLBI erratum 4193789 (workaround is
  in Ubuntu 7.0.0-1013+), rust-lang/rust#135867 (a Hyper-V bug on Cobalt 100
  with similar symptoms, fixed in 2025), fpsimd/SVE state CVEs, contpte
  CVE-2026-43486.

## Appendix C: Diagnostics tooling (PR #3892)

- `misc/run-test-with-diagnostics.sh` is the `CMAKE_TEST_LAUNCHER`. When a test
  runs longer than `QLEVER_TEST_STALL_SECONDS` (default 10 s), it stops it,
  takes `QLEVER_TEST_NUM_SAMPLES` (2) samples `QLEVER_TEST_SAMPLE_INTERVAL`
  (0.5 s) apart of: per thread state, user/system ticks, minor/major faults,
  schedstat, wchan, syscall, kernel stack; loadavg, `/proc/stat`, PSI,
  `/proc/vmstat` (faults, migration, compaction, THP, swap), IPIs, cgroup
  `cpu.stat`/`memory.events`. Then it `exec`s gdb (needed for
  `ptrace_scope=1`), dumps all stacks, and stays attached to report fatal
  signals. On a crash without a stall, it analyzes the core dump
  (`kernel.core_pattern=core.%p` is set on the host). The exit status of the
  test is never changed.
- The workflow prints `uname`, `lscpu`, relevant `dmesg` lines, THP settings,
  and the SVE vector length, and disables BuildKit's 2 MiB log limit.
- A rerun watchdog reran the arm64 job of #3892 throughout 2026-11-05; it
  caught stalls (no corruption so far) with the signature above.

## Appendix D: Reproducer rounds (this branch)

Workflow `.github/workflows/repro.yml`, all on `ubuntu-24.04-arm` with
`7.0.0-1017-azure`.

| Round / job (run)                   | Workload                                                   | Result |
|-------------------------------------|------------------------------------------------------------|--------|
| 1 `memmove`, `zstd`, `mixed`, `mixed` in docker (42080052104) | `repro.c` via `stress.sh`, short-lived multithreaded processes, 100 min each | nothing in ~218,000 processes |
| 2 `pool` (42096592152)              | thread-pool variant of `repro.c`                           | nothing in 11,400 processes |
| 2 `pressure`                        | `mixed` while memory is filled                             | nothing in 20,400 processes |
| 2 `buildkit`                        | `mixed` inside `docker buildx build`                       | nothing in 23,800 processes |
| 2 `qlever`                          | `Dockerfile.qlever`: build 5 test binaries, loop `ctest`   | stall in iteration 1 |
| 3 `qlever` (42101451126)            | as round 2, continue after stalls                          | 4 stalls in 27 iterations, **corruption in iteration 27** |
| 3 `qlever-noswap`                   | as above, `swapoff -a` (pswpout stays 0)                   | 7 stalls in 19 iterations, **corruption in iteration 19** |
| 3 `qlever-nocompact`                | no proactive compaction, THP/khugepaged defrag off          | **0 stalls in 169 iterations (90 min)**, no failure |
| 4 `qlever-proactiveness0` (42119334732) | only `vm.compaction_proactiveness=0`                    | stall + **corruption in iteration 1** (1,073 migrations) |
| 4 `qlever-thp-never`                | only THP and its defragmentation off                       | **1 stall in 172 iterations (90 min), no corruption** |
| 4 `qlever-forcecompact`             | `vm.compact_memory` every second                           | 5 stalls in 9 iterations, **corruption in iteration 9** despite 30 million page migrations, i.e. about the default rate |
| 5 `qlever-purge0` (42123152400)     | `MALLOC_CONF=dirty_decay_ms:0,muzzy_decay_ms:0` (`jemalloc` returns freed memory immediately) | 9 slow tests in 3 iterations (some only slowdowns by the `madvise` calls, e.g. `CompressedRelationWriter.MixedSizes`), **corruption in iteration 3** |
| 5 `qlever-nopurge`                  | `MALLOC_CONF=dirty_decay_ms:-1,muzzy_decay_ms:-1` (never returns memory) | **0 stalls in 188 iterations (90 min), no corruption** |
| 5 `qlever-nojemalloc`               | built without `jemalloc`                                   | **0 stalls in 202 iterations (90 min), no corruption** |
| 5 `synth-madvfree`/`-madvdontneed`/`-jemalloc` | `repro.c` with chunks returned via `madvise` and reused by other threads / with `jemalloc` | running |
| 7 `dontneed-2m`/`-4m`/`free-2m`/`dontneed-2m-16threads` (42134538876) | `repro.c` `madv...` modes with 2 MiB aligned chunks of 2 or 4 MiB, so that `MADV_DONTNEED` empties whole page tables | `free-2m`: only false positives (processes too long for the 15 s threshold, user time, ~1k faults); the others running |
| 6 `loop-*` (42127430005)            | `IndexBuildLoopTest`: only the build of the 3-triple NQuads test index in a loop **in long-running processes**, 4 or 1 instances, `jemalloc` default / immediate / no purging | **nothing**: about 96,000 builds in 4 jobs × 1 h, 0 corruptions, no fault storms (a few ~1 s builds, simultaneously in all instances and also without purging, i.e. machine hiccups) |
| 8 `fresh-*` (42138234910)           | as round 6, but **a new process for each build** (or every 10 builds), like `ctest` | **reproduces**: `fresh-10` 8 fault storms (0.7 to 2.5 M faults, stime ≈ 5 × utime) in 3,170 processes / 1 h; `fresh-1-purge0` **the POS corruption of the 3-triple index** (`row V:7 V:792637804536924896 I:0 I:2147483647`) in 1 of 24,841 processes. So the stripped QLever reproducer works, but needs many short-lived processes, and is slower than the five-test `ctest` loop |
| 11 `qlever-trace-*` (42144842511)   | as round 10, but the watcher runs as root on the host: kernel stack, `bpftrace` of `handle_mm_fault`, `pagemap` | attaching the `bpftrace` kprobes **ended the storm** before anything was traced (kprobe insertion briefly stops all CPUs); the kernel stack of a running thread is not available |
| 12 `longrun-spawn`, `fresh-1-(no)spawn` (42147934519), synthetic `*-spawn` (42147934730) | a host process that starts `/bin/true` as fast as possible (many ASID rollovers) next to the long-running index loop (nothing in round 6) and next to the synthetic workloads; ASID generation logged on the host | running |
| 9 `qlever-watch-*` (42138288151)    | the QLever tests (as round 3) with `fault-storm-watch.sh` | `qlever-watch-1`: **storm caught live** in iteration 2 (details below), then the corruption in exactly that test. `qlever-watch-2`: 5 storms (3.6 to 4.0 M faults/s) in 17 iterations, **all 5 unaffected by reading `/proc` and ended by `SIGSTOP`/`SIGCONT`**; corruption in iteration 17 in `CompressedRelationReader.getFirstAndLastTripleIgnoringGraph` (1.49 s, no storm detected, i.e. corruption also happens without a long storm) |
| 10 `qlever-cpu-*` (42141114522)     | as round 9, with probes that pin the thread to its CPU and then move it to another CPU | **every storm that lasted until the CPU probes (≈ 12 in 2 jobs) continued at 3.6 to 4.0 M faults/s when pinned to its CPU and ended immediately when moved to another CPU** (a few storms ended by themselves earlier); corruption in both jobs |
| 4 `synth-compact-mixed`/`-pool`     | `repro.c` + `vm.compact_memory` every second               | nothing in 110 minutes (cancelled) |

Details of round 3:

- One `ctest` iteration runs 445 tests of `TransitivePathTest`, `JoinTest`,
  `CompressedRelationsTest`, `GroupByTest`, `OptionalJoinTest` in about 27 s.
  A stall (threshold 3 s) is a test that normally takes 0.05 to 0.1 s taking
  3.5 to 4.5 s. 10 of 11 stalls were in `TransitivePathTest` tests with a
  graph variable and an empty path, 1 in `GroupByOptimizations
  ...NamedGraphDuplicates`.
- Stalled samples: system-wide `pgfault` grows by 2.3 to 2.6 million in
  0.65 s (about 4 million/s); the busy threads have millions of minor faults
  (e.g. 5.3 M and 6.6 M) and 5 to 6 times more system than user ticks; cgroup
  `system_usec` is 2.8 times `user_usec`. Swap, compaction, migration, and
  THP counters don't change during a stall. A stalled thread was in
  `ZSTD_compress` called from
  `CompressedRelationWriter::compressAndWriteColumn` on the global asio pool.
- `qlever` corruption: `TransitivePathTest.graphVariableConstrainedByTwoIrisEmptyPath/TransitivePathHashMapFullyMaterialized`
  (48 ms, no stall): `IndexTestHelpers.cpp:124`, permutation POS, row
  `V:2 U:0 I:0 I:2147483647`, pattern index 0 instead of `NoPattern`.
- `qlever-noswap` corruption:
  `TransitivePathTest.graphVariableBoundToGraphOperationGraphVariableBothSidesEmptyPath/TransitivePathBinSearchFullyMaterialized`,
  which **also stalled** (3045 ms): POS rows `V:2 U:18906446041392 ...`, i.e.
  garbage in the subject column, same check failing.
- So swap is not needed. The common factor of the reproducing runs vs. the
  synthetic ones is the QLever index-building code itself (many short-lived
  asio pool tasks that allocate, fill, compress, and free blocks, with
  `memmove` into freshly reused heap memory), not the machine state.

Round 9 `qlever-watch-1`, the first storm caught live (`fault-storm-watch.sh`):
`TransitivePathTest.unboundGraphVariableEmptyPath/TransitivePathHashMapLazy`,
one thread in state R on CPU 1 with 10.5 million minor faults, 231 system vs.
38 user ticks. Fault rate after each probe: nothing 3.96 M/s, again 3.96 M/s,
reading `/proc/<pid>/stat`, `status`, `maps` 3.95 M/s, `smaps_rollup`,
`numa_maps`, `cmdline`, `environ`, reading one byte of its memory via
`/proc/<pid>/mem` 3.64 M/s, **`SIGSTOP` + `SIGCONT`: storm ended** (the test
then finished). The test then failed with the POS corruption (5.93 s:
`row V:2 S:81176270803333129 I:0 I:2147483647`, garbage in the subject
column). So the stuck write is the corrupting one, and something about
being descheduled (or the signal delivery) ends the loop; reading `/proc`
(including `access_remote_vm`) doesn't.

Round 4 `qlever-proactiveness0` corruption:
`GroupByOptimizations.computeGroupByForSingleIndexScanWithNamedGraphDuplicates`
(3002 ms, also stalled): POS row `V:7 V:9 U:4294967295 I:2147483647`, the
pattern of a row is `4294967295` (garbage) instead of 0.

Next variants (planned, before round 4):

1. Build only `TransitivePathTest` and loop only the graph-variable
   empty-path tests (more stalls per minute, shorter build).
2. Cache the built image, so that each attempt costs minutes instead of 20.
3. From the stalled path, derive a standalone program: a thread pool that
   repeatedly allocates (via glibc `malloc`, i.e. `mmap`/`munmap` and heap
   trimming with `madvise`) blocks of the sizes used there, writes them from a
   different thread than the one that allocated them, compresses, and frees.
   Candidates for the missing ingredient: `munmap`/`madvise(DONTNEED)` on one
   CPU while another CPU writes to recycled pages (this is where local vs.
   broadcast TLBI matters), and `fork` (CoW, as in the "page reused in write
   fault" change).
