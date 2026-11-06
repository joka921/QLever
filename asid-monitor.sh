#!/usr/bin/env bash

# Print the number of ASID bits of the arm64 kernel once, and then every
# `<interval>` seconds (default 5) the ASID generation, which grows by
# `2^asid_bits` with each rollover of the ASIDs. Needs root and `bpftrace`;
# reading the kernel variables needs no kprobes (which would stop the machine
# for a moment, which ends the fault storms, see `REPORT.md`).
#
# Usage: asid-monitor.sh [<interval>]

INTERVAL="${1:-5}"
bpftrace -q -e "
BEGIN {
  printf(\"asid_bits %u\n\", *(uint32 *)kaddr(\"asid_bits\"));
}
interval:s:$INTERVAL {
  printf(\"%s asid_generation %lld\n\", strftime(\"%H:%M:%S\", nsecs),
         *(int64 *)kaddr(\"asid_generation\"));
}"
