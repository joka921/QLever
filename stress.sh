#!/usr/bin/env bash

# Run the given command over and over (as many processes in parallel as there
# are CPUs), each via `run-test-with-diagnostics.sh`, until the given number of
# seconds has passed, or until one of the processes stalls (takes longer than
# `QLEVER_TEST_STALL_SECONDS`, default 5), crashes, or fails (e.g. reports a
# corruption). In the latter case print the diagnostics and exit with 1.
#
# Usage: stress.sh <seconds> <command> [<args>...]

set -u
DURATION="$1"
shift
DIR="$(cd "$(dirname "$0")" && pwd)"
export QLEVER_TEST_STALL_SECONDS="${QLEVER_TEST_STALL_SECONDS:-5}"
export QLEVER_TEST_DIAGNOSTICS_DIR="$PWD/diagnostics"
rm -rf "$QLEVER_TEST_DIAGNOSTICS_DIR"
mkdir -p "$QLEVER_TEST_DIAGNOSTICS_DIR"

BATCH="${STRESS_BATCH:-200}"
PARALLEL="${STRESS_PARALLEL:-$(nproc)}"
END=$((SECONDS + DURATION))
NUM_PROCESSES=0
echo "Kernel: $(uname -r), CPUs: $(nproc), parallel: $PARALLEL," \
  "command: $*, duration: ${DURATION}s"
while [ "$SECONDS" -lt "$END" ]; do
  BATCH_START=$SECONDS
  # `xargs` exits with 123 if any of the commands failed.
  seq 1 "$BATCH" | xargs -P "$PARALLEL" -I{} \
    "$DIR/run-test-with-diagnostics.sh" "$@" > /dev/null 2> errors.txt
  STATUS=$?
  NUM_PROCESSES=$((NUM_PROCESSES + BATCH))
  echo "$(date -u +%H:%M:%S) $NUM_PROCESSES processes," \
    "last batch took $((SECONDS - BATCH_START))s"
  if [ "$STATUS" -ne 0 ] || [ -n "$(ls -A "$QLEVER_TEST_DIAGNOSTICS_DIR")" ]; then
    echo "=== FOUND a stall, crash, or failure after $NUM_PROCESSES processes" \
      "(xargs status $STATUS)"
    echo "=== Errors of the last batch:"
    cat errors.txt
    for FILE in "$QLEVER_TEST_DIAGNOSTICS_DIR"/*; do
      [ -f "$FILE" ] && echo "=== Diagnostics in $FILE:" && cat "$FILE"
    done
    exit 1
  fi
done
echo "=== Nothing found in $NUM_PROCESSES processes"
