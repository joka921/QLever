#!/usr/bin/env bash

# Run the tests with the given labels (by default the ones of which some
# stalled or failed on the arm64 runners) via `ctest` over and over, until the
# given number of seconds has passed, until a test fails, or until
# `MAX_STALLS` (default 10) tests have stalled (taken longer than
# `QLEVER_TEST_STALL_SECONDS`, default 3, see `run-test-with-diagnostics.sh`,
# which must be the `CMAKE_TEST_LAUNCHER`). The diagnostics of each stall are
# printed when it is found. Exit with 1 if any test failed or stalled. Must be
# run in the build directory.
#
# Usage: ctest-loop.sh <seconds> [<label regex>]

set -u
DURATION="$1"
LABELS="${2:-^(TransitivePathTest|JoinTest|CompressedRelationsTest|GroupByTest|OptionalJoinTest)$}"
export QLEVER_TEST_STALL_SECONDS="${QLEVER_TEST_STALL_SECONDS:-3}"
MAX_STALLS="${MAX_STALLS:-10}"
export QLEVER_TEST_DIAGNOSTICS_DIR="$PWD/test-diagnostics"
rm -rf "$QLEVER_TEST_DIAGNOSTICS_DIR"
mkdir -p "$QLEVER_TEST_DIAGNOSTICS_DIR"

END=$((SECONDS + DURATION))
ITERATION=0
NUM_TESTS=0
NUM_STALLS=0
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
  for FILE in "$QLEVER_TEST_DIAGNOSTICS_DIR"/*; do
    if [ -f "$FILE" ]; then
      NUM_STALLS=$((NUM_STALLS + 1))
      echo "=== STALL $NUM_STALLS in iteration $ITERATION, diagnostics in $FILE:"
      cat "$FILE"
      rm -f "$FILE"
    fi
  done
  if [ "$STATUS" -ne 0 ]; then
    echo "=== FOUND a failure in iteration $ITERATION (ctest status $STATUS)"
    echo "=== Output of ctest:"
    grep -vE 'Passed +[0-9.]+ sec|^ +Start +[0-9]+:' ctest.log
    exit 1
  fi
  if [ "$NUM_STALLS" -ge "$MAX_STALLS" ]; then
    break
  fi
done
echo "=== $NUM_STALLS stalls in $ITERATION iterations with $NUM_TESTS tests" \
  "($((SECONDS))s)"
[ "$NUM_STALLS" -eq 0 ]
