#!/usr/bin/env bash

# Start new processes as fast as possible (`/bin/true` in `PARALLEL` loops,
# default 2), each with a new address space and thus a new ASID, so that the
# ASIDs of arm64 roll over much more often. Run until killed.
#
# Usage: spawn-loop.sh [<parallel>]

PARALLEL="${1:-2}"
for ((P = 0; P < PARALLEL; ++P)); do
  while :; do
    /bin/true
  done &
done
wait
