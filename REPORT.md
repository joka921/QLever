# Spurious stalls and corruptions of the QLever tests on the arm64 runners

Status: 2026-11-06, 00:20 UTC. Related: ad-freiburg/qlever PR #3892 (diagnostics
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
9. **`jemalloc`'s purging (`madvise`) matters:** with immediate purging
   (`MALLOC_CONF=dirty_decay_ms:0,muzzy_decay_ms:0`), corruption came in
   iteration 3. Jobs with no purging and without `jemalloc` are still
   running (no failure after 60+ minutes, which is already well above the
   default time to failure).

## Mitigations

| Mitigation | Status |
|---|---|
| Rerun failed arm64 jobs; classify via the launcher of #3892 (minor faults/s, system time) | works, but costs time and leaves a small chance of silently corrupt test results |
| Wait for a fixed `linux-azure` kernel / runner image (the 6.17 image was fine) | not under our control; report upstream (Ubuntu `linux-azure` Launchpad, GitHub `runner-images`) once a small reproducer exists |
| `vm.compaction_proactiveness=0` on the host | **does not help** (round 4) |
| **Disable THP on the host** (`echo never > /sys/kernel/mm/transparent_hugepage/enabled`, `.../defrag`, `.../khugepaged/defrag` = 0) before the Docker build | **promising**: 1 stall, 0 corruptions in 341 iterations (2 jobs); costs nothing for QLever's tests, since no THP were used anyway |
| Change `jemalloc`'s purging via `MALLOC_CONF` in the Docker build (e.g. `dirty_decay_ms:-1,muzzy_decay_ms:-1`, never return memory) | unclear, round 5 `qlever-nopurge` |
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
| 5 `qlever-nopurge`                  | `MALLOC_CONF=dirty_decay_ms:-1,muzzy_decay_ms:-1` (never returns memory) | running |
| 5 `qlever-nojemalloc`               | built without `jemalloc`                                   | running |
| 5 `synth-madvfree`/`-madvdontneed`/`-jemalloc` | `repro.c` with chunks returned via `madvise` and reused by other threads / with `jemalloc` | running |
| 7 `dontneed-2m`/`-4m`/`free-2m`/`dontneed-2m-16threads` (42134538876) | `repro.c` `madv...` modes with 2 MiB aligned chunks of 2 or 4 MiB, so that `MADV_DONTNEED` empties whole page tables | running |
| 6 `loop-*` (42127430005)            | `IndexBuildLoopTest`: only the build of the 3-triple NQuads test index in a loop, 4 or 1 instances, `jemalloc` default / immediate / no purging | running |
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
