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

// The filename[s] of the turtle inputs. Currently hardcoded, feel  free to make
// this an argc/argv style command line argument.
static const auto filenames = []() {
  std::vector<qlever::InputFileSpecification> res;
  res.push_back(qlever::InputFileSpecification{
      "ttl33588354_v32/full_33588354_v32.ttl", qlever::Filetype::Turtle});
  return res;
};

int main() {
  // basename of the index (feel free to make this a command line argument).
  std::string indexBasename = "demo-v1";

  // Build and run QLever index
  qlever::IndexBuilderConfig config;
  config.inputFiles_ = filenames();
  config.baseName_ = indexBasename;
  // We don't use QLever's "pattern" extension.
  config.noPatterns_ = true;
  // All queries have fixed predicates.
  config.onlyPsoAndPos_ = true;

  // A lot of the drivepath and roadPart IRIs can be encoded directly in the ID,
  // set up the configuration for this.
  // NOTE: If the encoding fails, the IRIs still will behave as expected.
  config.prefixesForIdEncodedIrisWithBitPattern_.push_back(
      {"http://www.bmw-carit.de/Foresight/Map/Ontologies/Low/behaviorMap#dp_",
       16, 32});
  config.prefixesForIdEncodedIrisWithBitPattern_.push_back(
      {"http://www.bmw-carit.de/Foresight/Map/Ontologies/Low/"
       "behaviorMap#roadPartId_",
       16, 32});

  std::unique_ptr<qlever::Qlever> qleverPtr;
  // We don't need these updates, and they cost a lot of time for cheap queries.
  setRuntimeParameter<&RuntimeParameters::websocketUpdatesEnabled_>(false);
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

  // Pin the geometry and payload queries to RAM. Those are the same for the
  // whole 30 km range, and do not depend on the current position.
  executor.pinQueries();

  // Set the following flags to true for some example output.
  // Flag to control detailed timing output
  constexpr bool showDetailedTiming = false;
  // Flag to control detailed drive path printing
  constexpr bool showDetailedDrivePaths = false;

  for (size_t i = 0; i < queryPointsData.size(); ++i) {
    // for (size_t i = 0; i < 5; ++i) {
    const auto& pointData = queryPointsData[i];

    try {
      auto stepResult = executor.processNextPoint(pointData);

      // Compact single-line output for step info
      std::cout << "\nStep " << (i + 1) << " @ " << pointData.coordinates
                << " - " << stepResult.timing.totalUs << " us";
      if (stepResult.distanceFromPreviousMeters.has_value()) {
        std::cout << " - Distance: "
                  << stepResult.distanceFromPreviousMeters.value() << " m";
      }
      std::cout << std::endl;

      // Compact output for results
      std::cout << "  DPs in ROI (total/added/removed): "
                << stepResult.totalDrivePaths << "/"
                << stepResult.addedDrivePaths.size() << "/"
                << stepResult.removedDrivePathIds.size() << std::endl;
      std::cout << "  DPs in MPP (total/added/removed): "
                << stepResult.totalMppDrivePaths << "/"
                << stepResult.mppDrivePaths.size() << "/"
                << stepResult.removedMppDrivePathIds.size() << std::endl;

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
