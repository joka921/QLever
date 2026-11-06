// Copyright 2026 The QLever Authors, in particular:
//
// 2026 Johannes Kalmbach <kalmbach@cs.uni-freiburg.de>, UFR
//
// UFR = University of Freiburg, Chair of Algorithms and Data Structures
//
// You may not use this file except in compliance with the Apache 2.0 License,
// which can be found in the `LICENSE` file at the root of the QLever project.

// A further stripped-down reproducer for the stalls and corruptions on the
// arm64 GitHub runners (see `REPORT.md` on the `arm64-stall-repro` branch):
// the fault storms of `StripLoopTest` happen in the `CompressedExternalIdTable`
// sorters of the permutation writer, which are created with a memory limit of
// 4 GB and therefore reserve gigabytes of virtual memory for their first
// block, of which a tiny index touches only the first few pages. This test does
// only that: create such sorters one after the other, push a few rows, sort
// them, check the result, and destroy the sorter (`jemalloc` then returns the
// huge block to the kernel via `madvise`).
//
// Environment variables: `QLEVER_REPRO_SORTERS` (the number of sorters, default
// 12), `QLEVER_REPRO_ROWS` (the rows per sorter, default 1000),
// `QLEVER_REPRO_MEMORY_MB` (the memory limit of each sorter, default 4000),
// `QLEVER_REPRO_ROUNDS` (how often each sorter is filled and cleared, default
// 2).

#include <gtest/gtest.h>
#include <unistd.h>

#include <algorithm>
#include <cstdlib>
#include <iostream>
#include <random>
#include <string>
#include <vector>

#include "engine/idTable/CompressedExternalIdTable.h"
#include "global/Id.h"
#include "util/AllocatorWithLimit.h"
#include "util/File.h"
#include "util/MemorySize/MemorySize.h"

namespace {
// Return the value of the environment variable `name`, or `def` if not set.
size_t getEnv(const char* name, size_t def) {
  const char* value = std::getenv(name);
  return value != nullptr ? std::stoul(value) : def;
}

// Sort the rows by all their four columns.
struct SortByAllColumns {
  template <typename A, typename B>
  bool operator()(const A& a, const B& b) const {
    for (size_t i = 0; i < 4; ++i) {
      if (a[i] != b[i]) {
        return a[i].getBits() < b[i].getBits();
      }
    }
    return false;
  }
};
}  // namespace

// _____________________________________________________________________________
TEST(SorterLoop, hugeSorters) {
  const size_t numSorters = getEnv("QLEVER_REPRO_SORTERS", 12);
  const size_t numRows = getEnv("QLEVER_REPRO_ROWS", 1000);
  const size_t numRounds = getEnv("QLEVER_REPRO_ROUNDS", 2);
  const auto memory =
      ad_utility::MemorySize::megabytes(getEnv("QLEVER_REPRO_MEMORY_MB", 4000));
  std::mt19937_64 rng{static_cast<uint64_t>(getpid())};
  for (size_t s = 0; s < numSorters; ++s) {
    std::string filename =
        "sorterLoop_" + std::to_string(getpid()) + "_" + std::to_string(s);
    {
      ad_utility::CompressedExternalIdTableSorter<SortByAllColumns, 0> sorter{
          filename, 4, memory, ad_utility::makeUnlimitedAllocator<Id>()};
      for (size_t r = 0; r < numRounds; ++r) {
        IdTableStatic<0> block{4, ad_utility::makeUnlimitedAllocator<Id>()};
        std::vector<std::array<uint64_t, 4>> expected;
        for (size_t i = 0; i < numRows; ++i) {
          std::array<uint64_t, 4> row;
          for (auto& value : row) {
            value = rng() % 1000;
          }
          expected.push_back(row);
          block.emplace_back();
          for (size_t c = 0; c < 4; ++c) {
            block(i, c) = Id::makeFromInt(static_cast<int64_t>(row[c]));
          }
        }
        sorter.pushBlock(block);
        std::sort(expected.begin(), expected.end());
        size_t i = 0;
        for (const auto& sortedBlock : sorter.getSortedBlocks()) {
          for (const auto& row : sortedBlock) {
            for (size_t c = 0; c < 4; ++c) {
              if (i >= expected.size() ||
                  row[c] !=
                      Id::makeFromInt(static_cast<int64_t>(expected[i][c]))) {
                std::cerr << "CORRUPTION in sorter " << s << ", round " << r
                          << ", row " << i << ", column " << c << ": " << row[c]
                          << std::endl;
                FAIL();
              }
            }
            ++i;
          }
        }
        ASSERT_EQ(i, expected.size());
        sorter.clear();
      }
    }
    ad_utility::deleteFile(filename, false);
  }
}
