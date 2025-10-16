// Copyright 2025 The QLever Authors, in particular:
//
// 2025 Johannes Kalmbach <kalmbach@cs.uni-freiburg.de>, UFR
//
// UFR = University of Freiburg, Chair of Algorithms and Data Structures

#include <iomanip>
#include <iostream>

#include "./IncrementalQueryExecutor.h"
#include "./InterfaceFilling.h"
#include "./QueryTemplates.h"
#include "./SimulationData.h"
#include "libqlever/Qlever.h"
#include "util/Exception.h"
#include "util/Log.h"
#include "util/NullStream.h"

static const auto filenames = []() {
  std::vector<qlever::InputFileSpecification> res;
  res.push_back(qlever::InputFileSpecification{
      "ttl33588354_v32/full_33588354_v32.ttl", qlever::Filetype::Turtle});
  return res;
};

int main() {
  // Parse command line arguments.
  std::string indexBasename = "demo-v1";

  // Build index for the given input file and write index files to disk.
  std::cout << "\x1b[1mBuilding index"
            << " with basename \"" << indexBasename << "\x1b[0m" << std::endl;
  qlever::IndexBuilderConfig config;
  config.inputFiles_ = filenames();
  config.baseName_ = indexBasename;
  config.noPatterns_ = true;
  config.onlyPsoAndPos_ = true;
  try {
    qlever::Qlever::buildIndex(config);
  } catch (const std::exception& e) {
    std::cerr << "Building the index failed: " << e.what() << std::endl;
    return 1;
  }
  std::cout << std::endl;

  // Load index.
  std::cout << "\x1b[1mLoading index with basename \"" << indexBasename
            << "\"\x1b[0m" << std::endl;
  qlever::EngineConfig engineConfig{config};
  qlever::Qlever qlever{engineConfig};
  std::cout << std::endl;

  // Suppress QLever internal logging
  static ad_utility::NullStream nullStream;
  ad_utility::setGlobalLoggingStream(&nullStream);

  // Extract query points data from simulation data
  auto queryPointsData = extractQueryPointsData(0);

  std::cout << "pinning the geometries" << std::endl;
  qlever.queryAndPinResultWithName({"geos", Variable{"?geom"}},
                                   qlever::geometryQuery);

  std::cout << "pinning the payload" << std::endl;
  qlever.queryAndPinResultWithName({"payload", std::nullopt},
                                   qlever::payloadQuerySingleColumn);

  // Create incremental query executor
  qlever::IncrementalQueryExecutor executor(qlever);

  // Flag to control detailed timing output
  constexpr bool showDetailedTiming = false;

  for (size_t i = 0; i < queryPointsData.size(); ++i) {
    const auto& pointData = queryPointsData[i];

    try {
      auto stepResult = executor.processNextPoint(pointData);

      // Compact single-line output for step info
      std::cout << "Step " << (i + 1) << " @ " << pointData.coordinates << " - "
                << stepResult.timing.totalUs << " us";
      if (stepResult.distanceFromPreviousMeters.has_value()) {
        std::cout << " - Distance: "
                  << stepResult.distanceFromPreviousMeters.value() << " m";
      }
      std::cout << std::endl;

      // Compact single-line output for results
      std::cout << "  Num DPs (Total/Added/Removed/MPP): "
                << stepResult.totalDrivePaths << "/"
                << stepResult.addedDrivePaths.size() << "/"
                << stepResult.removedDrivePathIds.size() << "/"
                << stepResult.mppDrivePaths.size() << std::endl;

      // Detailed timing breakdown (controlled by flag)
      if (showDetailedTiming) {
        std::cout << "  Timing breakdown:" << std::endl;
        std::cout << "    Spatial query:      " << std::setw(8)
                  << stepResult.timing.spatialQueryUs << " us" << std::endl;
        std::cout << "    ID extraction:      " << std::setw(8)
                  << stepResult.timing.idExtractionUs << " us" << std::endl;
        std::cout << "    Diff computation:   " << std::setw(8)
                  << stepResult.timing.diffComputationUs << " us" << std::endl;
        std::cout << "    Feature query:      " << std::setw(8)
                  << stepResult.timing.featureQueryUs << " us" << std::endl;
        std::cout << "    MPP query:          " << std::setw(8)
                  << stepResult.timing.mppQueryUs << " us" << std::endl;
        std::cout << "    Total:              " << std::setw(8)
                  << stepResult.timing.totalUs << " us" << std::endl;
      }

    } catch (const std::exception& e) {
      std::cerr << "Executing the query failed: " << e.what() << std::endl;
      return 1;
    }
  }
}
