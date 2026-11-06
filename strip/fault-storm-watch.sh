#!/usr/bin/env bash

# Watch all threads of all processes whose name matches the given regex for a
# storm of page faults (more than `STORM_FAULTS_PER_SECOND` minor faults per
# second, default 200000, for two consecutive seconds), which is the signature
# of the stalls on the arm64 runners (see `REPORT.md`). For each storm, print
# the state of the thread, and then find out what ends it: the stalls so far
# always ended exactly when `run-test-with-diagnostics.sh` started to look at
# the stalled process. So do one probe after the other (first nothing, then
# reading more and more intrusive files of `/proc/<pid>`, then pinning the
# thread to its CPU and moving it to another CPU, then `SIGSTOP` and `SIGCONT`)
# and measure the fault rate after each, until the storm ends. (In round 9,
# reading `/proc` didn't end a storm, but `SIGSTOP`/`SIGCONT` did.)
#
# Usage: fault-storm-watch.sh <process name regex>

set -u
REGEX="$1"
THRESHOLD="${STORM_FAULTS_PER_SECOND:-200000}"
declare -A LAST_FAULTS HOT_SECONDS

# Print the minor faults of the given thread (`/proc/<pid>/task/<tid>`).
minor_faults() {
  local STAT
  # Use `read` (a builtin) instead of `cat`, which would fork for each thread.
  { read -r STAT < "$1/stat"; } 2> /dev/null || { echo -1; return; }
  local -a FIELDS
  read -r -a FIELDS <<< "${STAT##*) }"
  echo "${FIELDS[7]}"
}

# Print the minor faults per second of the given thread over 0.3 s.
fault_rate() {
  local BEFORE AFTER
  BEFORE=$(minor_faults "$1")
  sleep 0.3
  AFTER=$(minor_faults "$1")
  if [ "$BEFORE" -lt 0 ] || [ "$AFTER" -lt 0 ]; then
    echo -1
  else
    echo $(((AFTER - BEFORE) * 10 / 3))
  fi
}

# If `bpftrace` is available and this runs as root (on the host), trace the
# page faults of the given thread for 0.5 s: the faulting addresses and flags
# that `handle_mm_fault` gets, its return values, and which functions of the
# fault path are called (only those that exist and aren't inlined). Then print
# the mapping and the `pagemap` entry of the most frequent address.
trace_faults() {
  local PID="$1" TID="$2"
  command -v bpftrace > /dev/null && [ "$(id -u)" = 0 ] || return
  local FUNCTIONS=/sys/kernel/tracing/available_filter_functions
  [ -r "$FUNCTIONS" ] || FUNCTIONS=/sys/kernel/debug/tracing/available_filter_functions
  local PROBES="" FUNCTION
  for FUNCTION in handle_pte_fault do_wp_page wp_page_reuse wp_page_copy \
    do_anonymous_page do_numa_page ptep_set_access_flags \
    __ptep_set_access_flags flush_tlb_fix_spurious_fault \
    __flush_tlb_page_nosync do_page_fault do_translation_fault \
    do_bad_area __do_page_fault lock_vma_under_rcu; do
    if grep -qx "$FUNCTION" "$FUNCTIONS" 2> /dev/null; then
      PROBES+="kprobe:$FUNCTION /tid == $TID/ { @calls[func] = count(); } "
    fi
  done
  echo "--- bpftrace of thread $TID for 0.5 s:"
  timeout 20 bpftrace -q -e "
    kprobe:handle_mm_fault /tid == $TID/ {
      @faults[arg1, arg2] = count(); @calls[func] = count(); }
    kretprobe:handle_mm_fault /tid == $TID/ { @ret[retval] = count(); }
    $PROBES
    interval:ms:500 { exit(); }" 2>&1 | head -n 60 | tee "/tmp/bpftrace-$TID.txt"
  # The most frequent faulting address (`bpftrace` prints the entries of a map
  # sorted by their value, but sort anyway).
  local ADDRESS
  ADDRESS=$(grep -E '^@faults\[' "/tmp/bpftrace-$TID.txt" | sort -t : -k 2 -n |
    tail -n 1 | grep -oE '^@faults\[[0-9]+' | grep -oE '[0-9]+$')
  if [ -n "$ADDRESS" ]; then
    printf -- '--- address 0x%x\n' "$ADDRESS"
    local LINE START END
    while read -r LINE; do
      START=$((16#${LINE%%-*}))
      END=${LINE#*-}
      END=$((16#${END%% *}))
      if [ "$ADDRESS" -ge "$START" ] && [ "$ADDRESS" -lt "$END" ]; then
        echo "--- mapping: $LINE"
      fi
    done < "/proc/$PID/maps"
    local ENTRY
    ENTRY=$(dd if="/proc/$PID/pagemap" bs=8 count=1 skip=$((ADDRESS / 4096)) 2> /dev/null | od -An -tx8 | tr -d ' ')
    if [ -n "$ENTRY" ]; then
      local VALUE=$((16#$ENTRY))
      echo "--- pagemap entry: 0x$ENTRY (present $(((VALUE >> 63) & 1))," \
        "swapped $(((VALUE >> 62) & 1)), exclusive $(((VALUE >> 56) & 1))," \
        "soft-dirty $(((VALUE >> 55) & 1)), pfn $((VALUE & ((1 << 55) - 1))))"
    fi
  fi
}

# If `STORM_GDB` is set, attach `gdb` to the process and print the backtrace,
# the current instruction, and the registers of the storming thread. The
# faulting instruction is retried after each fault, so the PC is exactly the
# access that faults. (Attaching stops the thread, which ends the storm.)
gdb_probe() {
  local PID="$1" TID="$2"
  [ -n "${STORM_GDB:-}" ] || return 1
  echo "--- gdb of thread $TID:"
  timeout 60 gdb -p "$PID" -batch -nx -q \
    -ex "python [t.switch() for t in gdb.selected_inferior().threads() if t.ptid[1] == $TID]" \
    -ex "info thread" -ex "bt 20" -ex 'x/4i $pc' -ex "info registers" 2>&1 |
    grep -vE '^\[New LWP|^\[Thread debugging|^Using host libthread|^warning: .*(debug|separate)' |
    head -n 120 | sed 's/^/    /'
  return 0
}

# Print the state of the thread and probe what ends its storm (see above).
probe() {
  local PID="$1" TASK="$2"
  local STAT
  STAT=$(cat "$TASK/stat" 2> /dev/null)
  local -a FIELDS
  read -r -a FIELDS <<< "${STAT##*) }"
  echo "=== FAULT STORM at $(date -u +%H:%M:%S.%N) in thread $(basename "$TASK")" \
    "of pid $PID ($(tr '\0' ' ' < "/proc/$PID/cmdline" 2> /dev/null | cut -c 1-150)):" \
    "state ${FIELDS[0]}, utime ${FIELDS[11]}, stime ${FIELDS[12]}," \
    "minflt ${FIELDS[7]}, majflt ${FIELDS[9]}, processor ${FIELDS[36]}," \
    "wchan $(cat "$TASK/wchan" 2> /dev/null)," \
    "syscall $(cat "$TASK/syscall" 2> /dev/null)"
  sed 's/^/    /' "$TASK/stack" 2> /dev/null
  trace_faults "$PID" "$(basename "$TASK")"
  echo "vmstat: $(grep -E '^(pgfault|pgmajfault|pgmigrate_success|thp_fault_alloc|nr_tlb_remote_flush|nr_tlb_local_flush_all|nr_tlb_local_flush_one) ' /proc/vmstat | tr '\n' ' ')"
  local STEP RATE NOW="${STAT##*) }"
  local CPU OTHER_CPU
  if gdb_probe "$PID" "$(basename "$TASK")"; then
    echo "    after gdb: $(fault_rate "$TASK") minor faults/s"
    echo "=== STORM ENDED after gdb"
    return
  fi
  for STEP in nothing nothing-again stat status maps smaps_rollup numa_maps \
    cmdline environ mem-read affinity-same-cpu affinity-other-cpu \
    sigstop-sigcont; do
    case "$STEP" in
      nothing*) ;;
      stat | status | maps | smaps_rollup | numa_maps | cmdline | environ)
        cat "/proc/$PID/$STEP" > /dev/null 2>&1 ;;
      # Read one byte of the process's memory (at the start of its stack
      # region), which goes through `access_remote_vm` like `cmdline`.
      mem-read)
        local START
        START=$(grep -m 1 '\[stack\]' "/proc/$PID/maps" 2> /dev/null | cut -d - -f 1)
        [ -n "$START" ] && dd if="/proc/$PID/mem" bs=1 count=1 skip=$((16#$START)) \
          iflag=skip_bytes of=/dev/null 2> /dev/null ;;
      # Restrict the thread to the CPU that it is running on (which changes
      # nothing, as a control), and then move it to another CPU. If the
      # storm ends then, it is caused by state of the first CPU (e.g. a stale
      # TLB entry that a local invalidation doesn't remove).
      affinity-same-cpu)
        CPU=$(cut -d ' ' -f 37 <<< "$NOW")
        OTHER_CPU=$(((CPU + 1) % $(nproc)))
        taskset -p -c "$CPU" "$(basename "$TASK")" > /dev/null 2>&1 ;;
      affinity-other-cpu)
        taskset -p -c "$OTHER_CPU" "$(basename "$TASK")" > /dev/null 2>&1 ;;
      sigstop-sigcont)
        kill -STOP "$PID" 2> /dev/null
        sleep 0.1
        kill -CONT "$PID" 2> /dev/null ;;
    esac
    RATE=$(fault_rate "$TASK")
    NOW=$(cat "$TASK/stat" 2> /dev/null)
    NOW="${NOW##*) }"
    echo "    after $STEP: $RATE minor faults/s, on CPU $(echo "$NOW" | cut -d ' ' -f 37)"
    if [ "$RATE" -lt "$((THRESHOLD / 10))" ]; then
      echo "=== STORM ENDED after $STEP"
      taskset -p -c "0-$(($(nproc) - 1))" "$(basename "$TASK")" > /dev/null 2>&1
      return
    fi
  done
  taskset -p -c "0-$(($(nproc) - 1))" "$(basename "$TASK")" > /dev/null 2>&1
  echo "=== STORM DID NOT END after all probes"
}

echo "Watching threads of processes matching \`$REGEX\` for more than" \
  "$THRESHOLD minor faults/s"
while true; do
  for PROCESS in /proc/[0-9]*; do
    { read -r NAME < "$PROCESS/comm"; } 2> /dev/null || continue
    [[ "$NAME" =~ $REGEX ]] || continue
    for TASK in "$PROCESS"/task/[0-9]*; do
    TID="${TASK##*/}"
    { read -r STAT < "$TASK/stat"; } 2> /dev/null || continue
    read -r -a FIELDS <<< "${STAT##*) }"
    FAULTS="${FIELDS[7]}"
    DELTA=$((FAULTS - ${LAST_FAULTS[$TID]:-$FAULTS}))
    LAST_FAULTS[$TID]=$FAULTS
    if [ "$DELTA" -gt "$THRESHOLD" ]; then
      HOT_SECONDS[$TID]=$((${HOT_SECONDS[$TID]:-0} + 1))
    else
      HOT_SECONDS[$TID]=0
    fi
    if [ "${HOT_SECONDS[$TID]}" -ge 2 ]; then
      PID="${TASK#/proc/}"
      PID="${PID%%/*}"
      probe "$PID" "$TASK"
      HOT_SECONDS[$TID]=0
      LAST_FAULTS[$TID]=$(minor_faults "$TASK")
    fi
    done
  done
  sleep 1
done
