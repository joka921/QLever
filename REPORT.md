# Spurious stalls and corruptions of the QLever tests on the arm64 runners

Status: 2026-11-06, 07:30 UTC. Related: ad-freiburg/qlever PR #3892 (diagnostics
launcher), this branch (`arm64-stall-repro` on `joka921/QLever`) for the
reproducers.

## Summary

**TL;DR:** The stalls and corruptions are caused by the kernel of the arm64
runners (`7.0.0-1017-azure`, a Hyper-V guest), not by QLever. One CPU keeps
taking the same page fault on a valid page-table entry (about 4 million
times per second); the stuck write is the one that later shows up as
corruption. It needs `jemalloc` returning memory to the kernel via
`madvise`, and many short-lived processes. **Mitigation:** run the tests with
`MALLOC_CONF=dirty_decay_ms:-1,muzzy_decay_ms:-1`: **0 failures in 158,859
test processes on 3 VMs, vs. 61 failures in 44,049 processes on 2 VMs with the
defaults** (round 18). Applied to the Docker build in PR #3892 (da5807221). **Fastest reproducer so far:** build
`TransitivePathTest` and `JoinTest`, run their 30 graph tests one process at a
time (rounds 14 and 16): first failure after 22 s to 4 minutes, a failure
every ~60 to 100 s. **It needs idle CPUs:** the same tests with 4 instances
in parallel (all CPUs busy) never failed (round 16, 188,477 processes), which
explains why all synthetic stress tests (which ran `nproc` processes in
parallel) found nothing.

1. **The trigger is the runner kernel, not a QLever change.** All failures
   (POS corruption, Bus error, `unreachable` in `Datatype.h`) and all long
   stalls of the arm64 Docker build ran on `7.0.0-1017-azure`. On
   `6.17.0-1022-azure`, 0 of 148 master builds had any test taking more than
   10 s; on `7.0.0-1017-azure`, 17 of 48 did. For the same code (master
   2026-10-19 to 10-28): 0/56 vs. 6/16, Fisher p ≈ 3e-12.
2. **A stall is a storm of spurious page faults on one CPU.** One thread
   (in `__memmove_sve`, `ZSTD_compress`, or anything else that touches
   memory) takes 3.6 to 4.0 million minor faults per second, almost all in
   the kernel, with a flat RSS. `bpftrace` (round 11): all faults hit a
   single address of the `jemalloc` heap, read or write faults under the
   per-VMA lock, the PTE is present and valid, `handle_pte_fault` →
   `__ptep_set_access_flags` changes nothing, `handle_mm_fault` returns 0,
   and the CPU faults again. **Moving the thread to another CPU ends the
   storm immediately, pinning it to its current CPU doesn't** (round 10,
   every storm), and neither does reading anything in `/proc/<pid>` (round
   9). `SIGSTOP`/`SIGCONT`, gdb attaching, and the CPU stop for inserting
   kprobes also end it (by taking the thread off its CPU). Without
   intervention, a storm lasts indefinitely (up to 9844 s in CI).
3. **Stalls and corruptions have the same cause.** They happen in the same
   tests and code paths (building the test indexes, mostly POS), and the
   corruption shows up right after a storm ends: in rounds 3, 4, 9 and in
   PR #3892, the failing test was the one whose storm had just been ended.
   The corrupt values are zeros, leftover heap bytes, or data from
   elsewhere in one column, i.e. writes that went to the wrong physical
   page. Corruption also occurs without a long storm.
4. **Undefined behavior in QLever is ruled out as the cause.** The PTE is
   valid (so the access itself is legal), the loop is inside the kernel,
   another CPU executes the same access without a fault, unrelated code
   (`ZSTD_compress`) is affected the same way, and the same code is fine on
   6.17 and on amd64.
5. **What triggers it:** `jemalloc`'s purging (returning freed memory via
   `madvise`): no purging, or no `jemalloc` at all, makes the bug disappear
   (round 5); immediate purging makes it more frequent. And **many
   short-lived processes**: the same index builds in long-running processes
   never failed (round 6, 96,000 builds), with a fresh process per build
   they did (round 8). (Transparent huge pages set to `never` seemed to help
   in rounds 3 and 4, but not with the fastest reproducer in round 16: 33
   failures. **Some VMs don't reproduce at all** (round 15: one of two
   identical jobs never stalled in 90 min), so a single clean job is weak
   evidence; all VMs report the same CPU, Neoverse-N2.)
   And **idle CPUs**: one test at a time fails every ~60 s, 4 tests in
   parallel (all 4 CPUs busy) never (round 16). Busy CPUs mean frequent
   context switches and migrations (which end storms, see 2.), and an idle
   vCPU of a Hyper-V guest may handle TLB invalidations differently.
6. **What doesn't trigger it:** swap (round 3), memory compaction / page
   migration (round 4, even 30 million forced migrations), `io_uring`
   (round 15), ASID rollovers (round 12: 226 per hour with a process spawner
   changed nothing).
7. **Synthetic reproducers failed so far:** `memmove`/`zstd`/thread pools
   with glibc or `jemalloc` (with immediate purging), `MADV_FREE` /
   `MADV_DONTNEED` of chunks reused by other threads (also 2 MiB aligned,
   which frees page tables), pinned touchers vs. a zapper (`ptzap`), all
   with and without a process spawner: nothing in about 300,000 processes.
   QLever's index building does something these don't.
8. **Suspects in the kernel** (not confirmed): the change "arm64,
   tlbflush: don't TLBI broadcast if page reused in write fault" (Huang Ying,
   arm64 tree November 2025, in 7.0 but not in 6.17), which replaces
   broadcast TLB invalidations by local ones plus a fixup in the
   spurious-fault path; or a stale page-walk cache entry after page tables
   are freed (the local spurious-fault fixup only invalidates the last
   level, and read faults get no invalidation at all), possibly specific to
   Hyper-V.

## Mitigations

| Mitigation | Status |
|---|---|
| Rerun failed arm64 jobs; classify via the launcher of #3892 (minor faults/s, system time) | works, but costs time and leaves a small chance of silently corrupt test results |
| Wait for a fixed `linux-azure` kernel / runner image (the 6.17 image was fine) | not under our control; report upstream (Ubuntu `linux-azure` Launchpad, GitHub `runner-images`) once a small reproducer exists |
| `vm.compaction_proactiveness=0` on the host | **does not help** (round 4) |
| Disable THP on the host (`echo never > /sys/kernel/mm/transparent_hugepage/enabled`, `.../defrag`, `.../khugepaged/defrag` = 0) before the Docker build | **does not help**: looked promising in rounds 3/4 (1 stall in 341 iterations, probably good VMs), but the fastest reproducer had 33 failures with it (round 16) |
| Build the arm64 test binaries without `jemalloc` (e.g. `apt-get remove libjemalloc-dev` before `cmake` in the Docker build, only for the tests) | **works** in round 5 (0 stalls in 202 iterations), but then the tests don't use the allocator of the release binaries |
| **`MALLOC_CONF=dirty_decay_ms:-1,muzzy_decay_ms:-1` for the test step of the Docker build** (`jemalloc` never returns memory to the kernel; the tests are short-lived, so the memory cost is irrelevant) | **works, applied in PR #3892** (da5807221): round 18, 0 failures in 158,859 processes on 3 VMs vs. 61 failures in 44,049 processes on 2 VMs with the defaults (also round 5, 0 stalls in 188 iterations of the five-test loop); one environment variable, keeps `jemalloc`, no host changes |
| Run the tests with `ctest -j <nproc>` (all CPUs busy) | in round 16, 4 parallel instances never failed (188,477 processes), but this hides the bug rather than avoiding it, and the full test suite may not be safe to run in parallel |
| Run the arm64 tests outside the Docker build or on amd64 only | avoids the symptom only if the trigger is specific to the build; not tested |

### The change in the Docker build (PR #3892, da5807221)

In QLever's `Dockerfile`, run the tests with `jemalloc`'s purging disabled
(only for `ctest`; the shipped binaries are unaffected):

```dockerfile
RUN if [ "$RUN_TESTS" = "true" ]; then \
      cmake --build . && { \
        # The arm64 runners' kernel (7.0.0-1017-azure) has a bug that `jemalloc`'s
        # purging via `madvise` triggers (stalls and memory corruption in the
        # tests), see `REPORT.md` on the `arm64-stall-repro` branch.
        MALLOC_CONF=dirty_decay_ms:-1,muzzy_decay_ms:-1 \
        QLEVER_TEST_DIAGNOSTICS_DIR=/qlever/build/test-diagnostics \
          ctest --rerun-failed --output-on-failure; \
        ...
```

`MALLOC_CONF` is only read by `jemalloc`, and the test processes are
short-lived, so not returning freed memory to the kernel costs nothing. (Disabling THP on
the host, which looked like an alternative, does not help, see round 16.)

## Next steps

- ~~Apply the `MALLOC_CONF` mitigation~~ (done in PR #3892).
- Strip QLever down to a minimal program that still stalls reliably (from
  2026-11-06 on, over the weekend).
- Report upstream (Ubuntu `linux-azure` on Launchpad, and GitHub's
  `actions/runner-images`) with the evidence of appendix D, rounds 10 and 11,
  and the round-14 reproducer (two QLever test binaries, ~25 minutes
  including the build). A self-contained C reproducer would be better, but
  none of the synthetic workloads triggered the bug yet.
- To get there: record the system calls of a failing test (`strace -f -e
  trace=memory` for `madvise`/`mmap`/`munmap`/`mprotect`) and replay that
  pattern synthetically.

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
| 8 `fresh-*` (42138234910)           | as round 6, but **a new process for each build** (or every 10 builds), like `ctest` | **reproduces**: `fresh-10` 8 fault storms (0.7 to 2.5 M faults, stime ≈ 5 × utime) in 3,170 processes / 1 h; `fresh-1-purge0` **the POS corruption of the 3-triple index** (`row V:7 V:792637804536924896 I:0 I:2147483647`) in 1 of 24,841 processes. `fresh-1` (default) and `fresh-1-nopurge`: nothing in about 24,800 processes each. So the stripped QLever reproducer works, but only weakly; the five-test `ctest` loop is much faster (storm every ~2 min, corruption in 10 to 20 min) |
| 11 `qlever-trace-*` (42144842511)   | as round 10, but the watcher runs as root on the host: kernel stack, `bpftrace` of `handle_mm_fault`, `pagemap` | in some storms, attaching the kprobes ended the storm; **two storms were traced** (see below): a single address faults ~1.5 million times per second with a present, valid PTE |
| 12 `fresh-1-spawn`                  | round 8 `fresh-1` plus the spawner: **226 ASID rollovers in 1 h** (16-bit ASIDs, generation 65536 → 14,876,672) | **nothing** in 22,015 processes: frequent ASID rollovers alone don't trigger the bug. Controls: `fresh-1-nospawn` (10 rollovers/h) nothing in about 24,800 processes; `longrun-spawn` (226 rollovers/h, long-running processes) nothing in 28,200 builds (only slow builds from the CPU contention) |
| 12 synthetic `*-spawn` (42147934730) | `repro.c` `mixed` (glibc / `jemalloc` with immediate purging) and `madvdontneed` with 2 MiB chunks, with and without the spawner | **nothing** in 3,800 to 6,000 processes each, no storms |
| 15 `qlever-nouring-*` (42162005315)  | the five-test `ctest` loop, built with `-DUSE_IO_URING=OFF` (QLever reads vocabularies via `io_uring`, which no synthetic workload did) | `qlever-nouring-2`: 4 storms (3 ended only by moving the thread to another CPU) and **corruption in iteration 18**: `io_uring` is not the trigger |
| 14 `tests-graph*` (42157071431)     | only the stalling tests (`TransitivePathTest` `*raph*EmptyPath*`, `JoinTest` `*joinTwoScansWithDifferentGraphs*`, 30 tests), each in its own process, 4 or 1 at a time | **`tests-graph-p1` reproduces everything**: in 1 h, 49,318 processes, 36 failed (POS pattern check 26+, QLever assertions 6, SIGBUS 1, SIGSEGV 1), first failure after 4 minutes, and ~290 storms, 267 of them ended by moving the thread to another CPU. The parallel jobs were invalid (the instances shared a directory and overwrote each other's index files) |
| 16 `tests-graph-*` (42165619238)    | round 14 with a directory per instance, 45 min each | **`tests-graph-p1` (default, 1 at a time): 43 failed of 33,273 processes, 215 storms ended by moving the thread, first failure after 22 s. `tests-graph-p4` (default, 4 in parallel, all CPUs busy): nothing in 188,477 processes. `tests-graph-p4-nopurge` (`MALLOC_CONF=dirty_decay_ms:-1,muzzy_decay_ms:-1`): nothing in 205,860 processes.** **`tests-graph-p1-thpnever` (THP `never` on the host): 33 failed of 30,029 processes, 231 storms: THP doesn't help.** (Round 15 `qlever-nouring-1`: 0 stalls in 176 iterations, while its twin failed in iteration 18: some VMs don't reproduce at all.) |
| 18 `p1-default-a/b`, `p1-nopurge-a/b/c` (42173579386) | the fastest reproducer, one test at a time, 30 min: 2 VMs with the defaults vs. 3 VMs with `MALLOC_CONF=dirty_decay_ms:-1,muzzy_decay_ms:-1`, with full host info | **mitigation confirmed**: defaults 31 + 30 failed of 18,863 + 25,186 processes (157 + 115 storms ended by moving the thread, first failure after 7 s); `nopurge` 0 failures, 0 storms in 51,239 + 50,652 + 56,968 processes |
| 17 `serial-*` (42172669983)         | the synthetic workloads one process at a time | **nothing** (150 to 2,900 processes each) |
| 19 `ibl-p1-*`, `tests-graph-p1-control` (42179707513) | `IndexBuildLoopTest` one process at a time (it always ran 4 in parallel before), with the 2-quad input of the most-stalling test, its default input, 10 builds per process; and the 30 graph tests as control | running |
| 13 `ptzap-*` (42148661716)          | `repro ptzap`: touchers pinned to their CPUs keep writing/reading words in 2 MiB chunks, a zapper on another CPU empties whole chunks via `MADV_DONTNEED` (frees page tables); stall and corruption detection built in | **nothing**: 130 processes × 60 s per job (pinned, unpinned, pinned + spawner), no stall, no corruption (the watcher's hundreds of "storms" are `ptzap`'s legitimate fault load: none ended by moving the thread to another CPU, except one ambiguous case) |
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

Round 11 `qlever-trace-1`, two storms traced with `bpftrace` (0.5 s each):

| | storm 1 | storm 2 |
|---|---|---|
| test | `TransitivePathTest.graphVariableEmptyPathWithValueNotInIndex/...` | `JoinTestParametrized.joinTwoScansWithDifferentGraphs/1` |
| calls of `handle_mm_fault` | 779,066, **all at one address** | 715,993, **all at one address** |
| address | `0xff6543e17d81` (unaligned, a load) | `0xff23e9e1f300` |
| fault flags | `0x1254` = `VMA_LOCK, INTERRUPTIBLE, USER, KILLABLE, ALLOW_RETRY` (a **read** fault) | `0x1255` = the same plus `WRITE` |
| other calls | `handle_pte_fault` 735,732, `__ptep_set_access_flags` 533,874, `lock_vma_under_rcu` 471,774 | 681,215 / 507,667 / 455,699 |
| return value of `handle_mm_fault` | 0 (450,173 times) | 0 (436,818 times) |
| mapping | `ff6407e00000-ff6588000000 rw-p` (anonymous, the `jemalloc` heap) | `ff22ade00000-ff242e000000 rw-p` |
| `pagemap` | present, exclusive, pfn 2425133 | present, exclusive, pfn 4369731 |

So the kernel finds a present, valid PTE (the access flags are already set,
so `__ptep_set_access_flags` changes nothing) and returns successfully, but
the CPU faults again on the same access, about a million times per second,
until the thread runs on another CPU. For a write fault, the generic code
then does a local TLB invalidation of the last level only
(`flush_tlb_fix_spurious_fault`), for a read fault nothing. A stale entry in
the **page-walk cache** of that CPU (which caches the upper levels of the
page table, e.g. a pointer to a PTE table that has been freed by
`MADV_DONTNEED` and possibly reused) would be removed by neither, and would
explain all observations: a valid PTE that the CPU doesn't see, faults on a
single CPU only, the end when the thread moves to another CPU, the need for
`jemalloc`'s purging (`MADV_DONTNEED` of whole ranges can free page tables),
and corruption by data from unrelated memory (if the freed page-table page
was reused, the walker interprets arbitrary data as PTEs). This is a
hypothesis for the kernel developers; it is not proven.

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

## Appendix E: Stripping QLever (branch `arm64-strip`)

The branch `arm64-strip` of `joka921/QLever` starts from the pinned QLever
commit (7f8b3be50), deletes all upstream workflows, and adds
`.github/workflows/strip.yml`. That workflow builds natively on
`ubuntu-24.04-arm`, without Docker, with `ccache` in the Actions cache, so
small changes rebuild in minutes. It then runs the commands of
`strip/commands-*.txt` with `strip/loop.sh`: a fresh process per command,
one at a time, each via the diagnostics launcher (stall threshold 20 s), with
`fault-storm-watch.sh` running. `test/StripLoopTest.cpp` builds the 2-quad
NQuads index of the most-stalling test (`<a> <b> <c> <a> . <a> <b> <c> <c> .`)
once per process via `makeTestIndex`. That function also checks the patterns
of the permutations.

**Iteration 1** (run 42180004143, 40 min each, one process at a time):

| Job | Processes | Failures | Storms (ended by moving the thread) |
|---|---|---|---|
| 30 graph tests of `TransitivePathTest` + `JoinTest` (control) | 15,641 | 44 | 313 (262) |
| `StripLoopTest` (only the index build, no queries) | 13,630 | 30 | 314 (279) |

Natively, the index build alone stalls as often as the graph tests. The
first storm came 10 s into the loop, and then about one storm every 7 s,
in the main thread and in worker threads of the build. The failures are the
same as in CI:
- POS pattern mismatches (`patternIdx` 0 instead of `NoPattern`, with
  garbage IDs like `G:POINT(179.98 -0.31)`);
- the `unreachable` in `Datatype.h`;
- `Assertion firstCol0Id == first[0] failed` in the permutation writer.

Docker doesn't matter: in round 19 (Docker, one process at a time), the
same index build was just as strong (see below). Its weak results in round
14 came from running 4 processes in parallel.

**Round 19** (`arm64-stall-repro`, Docker, `IndexBuildLoopTest`, one process
at a time, 45 min each, run 42179707513):

| Job | Processes | Failed processes | Storms (ended by moving the thread) |
|---|---|---|---|
| default input of `IndexBuildLoopTest`, 1 build per process | 8,385 | 37 | 193 (150) |
| 2-quad input of `graphVariableEmptyPathWithRestrictedGraphs`, 1 build per process | 8,457 | 41 | 173 (132) |
| same, 10 builds per process | 1,045 | 46 | 206 (121) |
| 30 graph tests (control) | 33,248 | 37 | 264 (207) |

So 10 builds per process is no weaker than 1, but a single long-running
process (round 6, thousands of builds in 1 h, also with 1 instance) never
failed. So the bug needs the first builds of a process, i.e. a young address
space (new page tables, `jemalloc` arenas that are still growing and
purging), and idle CPUs.
