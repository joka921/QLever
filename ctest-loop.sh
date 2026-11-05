#!/usr/bin/env bash

# Run the tests with the given labels (by default the ones of which some
# stalled or failed on the arm64 runners) via `ctest` over and over, until the
# given number of seconds has passed, or until a test fails or stalls (takes
# longer than `QLEVER_TEST_STALL_SECONDS`, default 5, see
# `run-test-with-diagnostics.sh`, which must be the `CMAKE_TEST_LAUNCHER`). In
# the latter case print the output of `ctest` and the diagnostics and exit
# with 1. Must be run in the build directory.
#
# Usage: ctest-loop.sh <seconds> [<label regex>]

set -u
DURATION="$1"
LABELS="${2:-^(TransitivePathTest|JoinTest|CompressedRelationsTest|GroupByTest|OptionalJoinTest)$}"
export QLEVER_TEST_STALL_SECONDS="${QLEVER_TEST_STALL_SECONDS:-5}"
export QLEVER_TEST_DIAGNOSTICS_DIR="$PWD/test-diagnostics"
rm -rf "$QLEVER_TEST_DIAGNOSTICS_DIR"
mkdir -p "$QLEVER_TEST_DIAGNOSTICS_DIR"

END=$((SECONDS + DURATION))
ITERATION=0
NUM_TESTS=0
echo "Kernel: $(uname -r), CPUs: $(nproc), labels: $LABELS, duration: ${DURATION}s"
while [ "$SECONDS" -lt "$END" ]; do
  ITERATION=$((ITERATION + 1))
  START=$SECONDS
  ctest -L "$LABELS" --output-on-failure > ctest.log 2>&1
  STATUS=$?
  NUM_TESTS=$((NUM_TESTS + $(grep -cE 'Test +#[0-9]+: ' ctest.log)))
  SLOWEST=$(grep -oE 'Test +#[0-9]+: [^ ]+ \.+ +[A-Za-z*]+ +[0-9.]+ sec' ctest.log |
    sed -E 's/ \.+ +/ /' | awk '{print $(NF-1), $3}' |
    sort -n | tail -n 1)
  echo "$(date -u +%H:%M:%S) iteration $ITERATION: $(grep -E 'tests passed' ctest.log)," \
    "took $((SECONDS - START))s, slowest: $SLOWEST, total tests: $NUM_TESTS"
  if [ "$STATUS" -ne 0 ] || [ -n "$(ls -A "$QLEVER_TEST_DIAGNOSTICS_DIR")" ]; then
    echo "=== FOUND a failure or stall in iteration $ITERATION (ctest status $STATUS)"
    echo "=== Output of ctest:"
    grep -vE 'Passed +[0-9.]+ sec|^ +Start +[0-9]+:' ctest.log
    for FILE in "$QLEVER_TEST_DIAGNOSTICS_DIR"/*; do
      [ -f "$FILE" ] && echo "=== Diagnostics in $FILE:" && cat "$FILE"
    done
    exit 1
  fi
done
echo "=== Nothing found in $ITERATION iterations with $NUM_TESTS tests"
