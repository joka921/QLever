#!/usr/bin/env bash

# Run `PARALLEL` instances of `IndexBuildLoopTest` (which builds a tiny test
# index over and over and checks it) for the given number of seconds, each via
# `run-test-with-diagnostics.sh` (which reports if an instance hangs for more
# than 120 s longer than planned). Print the slow builds and corruptions as
# they happen, and a summary at the end. Exit with 1 if any build was slow or
# corrupt. Must be run in the build directory.
#
# Usage: index-loop.sh <seconds> <parallel>

set -u
DURATION="$1"
PARALLEL="$2"
export QLEVER_REPRO_SECONDS="$DURATION"
export QLEVER_TEST_STALL_SECONDS=$((DURATION + 120))
export QLEVER_TEST_DIAGNOSTICS_DIR="$PWD/test-diagnostics"
mkdir -p "$QLEVER_TEST_DIAGNOSTICS_DIR" logs
echo "Kernel: $(uname -r), CPUs: $(nproc), parallel: $PARALLEL," \
  "duration: ${DURATION}s, MALLOC_CONF: ${MALLOC_CONF:-}"
for ((P = 1; P <= PARALLEL; ++P)); do : > "logs/out-$P.txt"; done
# Print the slow builds and corruptions as they happen.
tail -q -n +1 -F logs/out-*.txt 2> /dev/null |
  grep --line-buffered -E 'SLOW|CORRUPTION|Failure|Built|exited|Which is|IndexTestHelpers' |
  while read -r LINE; do echo "$(date -u +%H:%M:%S) $LINE"; done &
PIDS=()
for ((P = 1; P <= PARALLEL; ++P)); do
  (cd logs && /qlever/misc/run-test-with-diagnostics.sh \
    /qlever/build/test/IndexBuildLoopTest > "out-$P.txt" 2>&1
   echo "instance $P exited with status $?" >> "out-$P.txt") &
  PIDS+=($!)
done
wait "${PIDS[@]}"
sleep 2
pkill -x tail
for FILE in "$QLEVER_TEST_DIAGNOSTICS_DIR"/*; do
  [ -f "$FILE" ] && echo "=== Diagnostics in $FILE:" && cat "$FILE"
done
NUM_SLOW=$(cat logs/out-*.txt | grep -c SLOW)
NUM_CORRUPT=$(cat logs/out-*.txt | grep -c CORRUPTION)
echo "=== Summary: $NUM_SLOW slow builds, $NUM_CORRUPT corruptions"
grep -h 'Built' logs/out-*.txt
[ "$NUM_SLOW" -eq 0 ] && [ "$NUM_CORRUPT" -eq 0 ]
