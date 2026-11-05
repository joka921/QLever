// Copyright 2026 The QLever Authors, in particular:
//
// 2026 Johannes Kalmbach <kalmbach@cs.uni-freiburg.de>, UFR
//
// UFR = University of Freiburg, Chair of Algorithms and Data Structures
//
// You may not use this file except in compliance with the Apache 2.0 License,
// which can be found in the `LICENSE` file at the root of the QLever project.

// A stripped-down reproducer for the stalls and corruptions of the QLever unit
// tests on the arm64 GitHub runners (see `REPORT.md` on the `arm64-stall-repro`
// branch): build the tiny NQuads test index of the failing tests (the same
// triple in several graphs) over and over. `makeTestIndex` checks the
// consistency of the patterns in the POS permutation, which is exactly the
// check that failed in CI.
//
// Environment variables: `QLEVER_REPRO_SECONDS` (how long to loop, default
// 600), `QLEVER_REPRO_INPUT` (the NQuads, default as in
// `GroupByOptimizations.computeGroupByForSingleIndexScanWithNamedGraphDuplicates`),
// `QLEVER_REPRO_SLOW_SECONDS` (report builds that take longer, default 1).

#include <gtest/gtest.h>
#include <unistd.h>

#include <chrono>
#include <cstdlib>
#include <iostream>
#include <string>

#include "util/File.h"
#include "util/IndexTestHelpers.h"

using namespace ad_utility::testing;

namespace {
// Return the value of the environment variable `name`, or `def` if not set.
std::string getEnv(const char* name, const std::string& def) {
  const char* value = std::getenv(name);
  return value != nullptr ? std::string{value} : def;
}
}  // namespace

// _____________________________________________________________________________
TEST(IndexBuildLoop, namedGraphDuplicates) {
  using Clock = std::chrono::steady_clock;
  const double seconds =
      std::stod(getEnv("QLEVER_REPRO_SECONDS", "600"));
  const double slowSeconds =
      std::stod(getEnv("QLEVER_REPRO_SLOW_SECONDS", "1"));
  const std::string input = getEnv(
      "QLEVER_REPRO_INPUT",
      "<s> <p> <o> <g1> . <s> <p> <o> <g2> . <s> <p> <o> <g3> .");
  auto elapsed = [](Clock::time_point since) {
    return std::chrono::duration<double>(Clock::now() - since).count();
  };
  const auto start = Clock::now();
  size_t numSlow = 0;
  size_t i = 0;
  for (; elapsed(start) < seconds; ++i) {
    std::string basename =
        "indexBuildLoop_" + std::to_string(getpid()) + "_" + std::to_string(i);
    const auto buildStart = Clock::now();
    {
      TestIndexConfig config;
      config.indexType = qlever::Filetype::NQuad;
      config.turtleInput = input;
      [[maybe_unused]] Index index = makeTestIndex(basename, config);
    }
    for (const std::string& filename : getAllIndexFilenames(basename)) {
      ad_utility::deleteFile(filename, false);
    }
    const double buildSeconds = elapsed(buildStart);
    if (buildSeconds > slowSeconds) {
      ++numSlow;
      std::cerr << "SLOW build " << i << " took " << buildSeconds << " s"
                << std::endl;
    }
    if (::testing::Test::HasFailure()) {
      std::cerr << "CORRUPTION in build " << i << " after " << elapsed(start)
                << " s" << std::endl;
      return;
    }
  }
  std::cerr << "Built " << i << " indexes in " << elapsed(start) << " s, "
            << numSlow << " slow" << std::endl;
}
