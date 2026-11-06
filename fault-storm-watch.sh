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
  echo "vmstat: $(grep -E '^(pgfault|pgmajfault|pgmigrate_success|thp_fault_alloc|nr_tlb_remote_flush|nr_tlb_local_flush_all|nr_tlb_local_flush_one) ' /proc/vmstat | tr '\n' ' ')"
  local STEP RATE NOW="${STAT##*) }"
  local CPU OTHER_CPU
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
