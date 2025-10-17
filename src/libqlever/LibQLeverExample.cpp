// Copyright 2025 The QLever Authors, in particular:
//
// 2025 Johannes Kalmbach <kalmbach@cs.uni-freiburg.de>, UFR
//
// UFR = University of Freiburg, Chair of Algorithms and Data Structures

#include <iomanip>
#include <iostream>

#include "./IncrementalQueryExecutor.h"
#include "./IndexManager.h"
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

  // Build and run QLever index
  qlever::IndexBuilderConfig config;
  config.inputFiles_ = filenames();
  config.baseName_ = indexBasename;
  config.noPatterns_ = true;
  config.onlyPsoAndPos_ = true;

  std::unique_ptr<qlever::Qlever> qleverPtr;
  try {
    qleverPtr = qlever::buildAndRunQleverIndex(indexBasename, config);
  } catch (const std::exception& e) {
    std::cerr << "Building/loading the index failed: " << e.what() << std::endl;
    return 1;
  }

  // Extract query points data from simulation data
  auto queryPointsData = extractQueryPointsData(0);

  // Create incremental query executor
  qlever::IncrementalQueryExecutor executor(*qleverPtr);

  // Pin the geometry and payload queries
  executor.pinQueries();

  // Flag to control detailed timing output
  constexpr bool showDetailedTiming = false;
  // Flag to control detailed drive path printing
  constexpr bool showDetailedDrivePaths = true;

  for (size_t i = 0; i < queryPointsData.size(); ++i) {
    const auto& pointData = queryPointsData[i];

    try {
      // std::this_thread::sleep_for(std::chrono::milliseconds(1000));
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
        qlever::printDetailedTimings(stepResult);
      }

      // Print detailed drive path information (controlled by flag)
      if (showDetailedDrivePaths) {
        // Print one added drive path from surroundings if available
        if (!stepResult.addedDrivePaths.empty()) {
          std::cout << "  Sample added drive path from surroundings:"
                    << std::endl;
          qlever::printDrivePathDetailed(stepResult.addedDrivePaths[0]);
        }

        // Print one MPP drive path if available
        if (!stepResult.mppDrivePaths.empty()) {
          std::cout << "  Sample MPP drive path:" << std::endl;
          qlever::printDrivePathDetailed(stepResult.mppDrivePaths[0]);
        }
      }

    } catch (const std::exception& e) {
      std::cerr << "Executing the query failed: " << e.what() << std::endl;
      return 1;
    }
  }
}
