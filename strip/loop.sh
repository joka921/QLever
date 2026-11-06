#!/usr/bin/env bash

# Run the commands of the given file (one command per line) over and over,
# each in a fresh process (round robin over the lines), in `PARALLEL`
# instances (each in its own directory), for the given number of seconds.
# Each process runs via `run-test-with-diagnostics.sh` (stall threshold
# `QLEVER_TEST_STALL_SECONDS`, default 20 s), and `fault-storm-watch.sh`
# watches the processes whose name matches `WATCH_REGEX` for storms of page
# faults (the signature of the kernel bug on the arm64 runners, see
# `REPORT.md` on the `arm64-stall-repro` branch). Print the failures as they
# happen and a summary at the end.
#
# Usage: loop.sh <seconds> <parallel> <commands file>

set -u
DURATION="$1"
PARALLEL="$2"
COMMANDS_FILE="$(realpath "$3")"
DIR="$(cd "$(dirname "$0")" && pwd)"
WORK="$PWD/loop-work"
export QLEVER_TEST_STALL_SECONDS="${QLEVER_TEST_STALL_SECONDS:-20}"
export QLEVER_TEST_DIAGNOSTICS_DIR="$WORK/diagnostics"
rm -rf "$WORK"
mkdir -p "$QLEVER_TEST_DIAGNOSTICS_DIR"
mapfile -t COMMANDS < "$COMMANDS_FILE"
echo "Kernel: $(uname -r), CPUs: $(nproc), parallel: $PARALLEL," \
  "duration: ${DURATION}s, MALLOC_CONF: ${MALLOC_CONF:-}," \
  "${#COMMANDS[@]} commands, e.g. ${COMMANDS[0]}"

# With `WATCH_SUDO`, run the watcher as root (for `bpftrace` and `gdb`).
${WATCH_SUDO:+sudo -E} "$DIR/fault-storm-watch.sh" "${WATCH_REGEX:-Test}" > >(sed -u 's/^/[storm] /') 2>&1 &
WATCH_PID=$!

END=$((SECONDS + DURATION))
PIDS=()
for ((P = 1; P <= PARALLEL; ++P)); do
  (
    mkdir -p "$WORK/$P" && cd "$WORK/$P" || exit 1
    N=0
    FAILED=0
    while [ "$SECONDS" -lt "$END" ]; do
      COMMAND="${COMMANDS[$(((N * 7 + P * 13) % ${#COMMANDS[@]}))]}"
      # shellcheck disable=SC2086
      "$DIR/run-test-with-diagnostics.sh" $COMMAND > ../last-$P.txt 2>&1
      STATUS=$?
      N=$((N + 1))
      if [ "$STATUS" -ne 0 ]; then
        FAILED=$((FAILED + 1))
        echo "$(date -u +%H:%M:%S) === FAILURE of \`$COMMAND\` (process $N of" \
          "instance $P, status $STATUS):"
        grep -vE '^\[ +(RUN|OK) +\]|^Running main|^Note: Google|^\[-+\]|^\[=+\]|^$' \
          ../last-$P.txt | head -n 60
      elif grep -q 'STALL DIAGNOSTICS' ../last-$P.txt; then
        echo "$(date -u +%H:%M:%S) === STALL (passed) of \`$COMMAND\`"
      fi
      rm -rf ./*
    done
    echo "=== instance $P: $N processes, $FAILED failed"
  ) &
  PIDS+=($!)
done
wait "${PIDS[@]}"
${WATCH_SUDO:+sudo} kill "$WATCH_PID" 2> /dev/null
sleep 1
