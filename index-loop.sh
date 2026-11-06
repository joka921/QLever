#!/usr/bin/env bash

# Run `PARALLEL` instances of `IndexBuildLoopTest` (which builds a tiny test
# index over and over and checks it) for the given number of seconds, each via
# `run-test-with-diagnostics.sh`. If `BUILDS_PER_PROCESS` is set, each instance
# starts a new process for every `BUILDS_PER_PROCESS` builds (like `ctest`
# starts a new process for each unit test); otherwise each instance is one
# process that builds until the time is up. `fault-storm-watch.sh` detects
# storms of page faults directly. Print the slow builds, stalls, storms, and
# corruptions as they happen, and a summary at the end. Exit with 1 if any
# build was slow or corrupt. Must be run in the build directory.
#
# Usage: index-loop.sh <seconds> <parallel>

set -u
DURATION="$1"
PARALLEL="$2"
BUILDS_PER_PROCESS="${BUILDS_PER_PROCESS:-}"
export QLEVER_TEST_DIAGNOSTICS_DIR="$PWD/test-diagnostics"
if [ -n "$BUILDS_PER_PROCESS" ]; then
  export QLEVER_REPRO_BUILDS="$BUILDS_PER_PROCESS"
  export QLEVER_REPRO_SECONDS=1000000
  export QLEVER_TEST_STALL_SECONDS=20
else
  export QLEVER_REPRO_SECONDS="$DURATION"
  export QLEVER_TEST_STALL_SECONDS=$((DURATION + 120))
fi
mkdir -p "$QLEVER_TEST_DIAGNOSTICS_DIR" logs
echo "Kernel: $(uname -r), CPUs: $(nproc), parallel: $PARALLEL," \
  "duration: ${DURATION}s, builds per process: ${BUILDS_PER_PROCESS:-all}," \
  "MALLOC_CONF: ${MALLOC_CONF:-}"
for ((P = 1; P <= PARALLEL; ++P)); do : > "logs/out-$P.txt"; done
# Detect storms of page faults directly, and probe what ends them.
/qlever/misc/fault-storm-watch.sh '^IndexBuildLoop' > >(sed -u 's/^/[storm] /') 2>&1 &
WATCH_PID=$!
# Print the slow builds and corruptions as they happen.
tail -q -n +1 -F logs/out-*.txt 2> /dev/null |
  grep --line-buffered -E 'SLOW|CORRUPTION|Failure|Built|exited|Which is|IndexTestHelpers|STALL|terminated|processes' |
  while read -r LINE; do echo "$(date -u +%H:%M:%S) $LINE"; done &
END=$((SECONDS + DURATION))
PIDS=()
for ((P = 1; P <= PARALLEL; ++P)); do
  (
    cd logs || exit 1
    NUM_PROCESSES=0
    while :; do
      /qlever/misc/run-test-with-diagnostics.sh \
        /qlever/build/test/IndexBuildLoopTest >> "out-$P.txt" 2>&1
      STATUS=$?
      NUM_PROCESSES=$((NUM_PROCESSES + 1))
      if [ "$STATUS" -ne 0 ]; then
        echo "instance $P: process $NUM_PROCESSES exited with status $STATUS" >> "out-$P.txt"
      fi
      if [ -z "$BUILDS_PER_PROCESS" ] || [ "$SECONDS" -ge "$END" ]; then
        break
      fi
      if ((NUM_PROCESSES % 1000 == 0)); then
        echo "instance $P: $NUM_PROCESSES processes" >> "out-$P.txt"
      fi
    done
    echo "instance $P exited after $NUM_PROCESSES processes" >> "out-$P.txt"
  ) &
  PIDS+=($!)
done
wait "${PIDS[@]}"
sleep 2
pkill -x tail
kill "$WATCH_PID"
for FILE in "$QLEVER_TEST_DIAGNOSTICS_DIR"/*; do
  [ -f "$FILE" ] && echo "=== Diagnostics in $FILE:" && cat "$FILE"
done
NUM_SLOW=$(cat logs/out-*.txt | grep -c SLOW)
NUM_CORRUPT=$(cat logs/out-*.txt | grep -c CORRUPTION)
NUM_STALLS=$(cat logs/out-*.txt | grep -c 'STALL DIAGNOSTICS')
# A failed check in `makeTestIndex` may also throw, then there is no
# `CORRUPTION` line, but the process exits with a nonzero status.
NUM_FAILED=$(cat logs/out-*.txt | grep -cE 'exited with status [1-9]')
echo "=== Summary: $NUM_SLOW slow builds, $NUM_STALLS stalls, $NUM_CORRUPT corruptions," \
  "$NUM_FAILED failed processes"
grep -h -E 'Built|exited after' logs/out-*.txt
[ "$NUM_SLOW" -eq 0 ] && [ "$NUM_CORRUPT" -eq 0 ] && [ "$NUM_STALLS" -eq 0 ] &&
  [ "$NUM_FAILED" -eq 0 ]
