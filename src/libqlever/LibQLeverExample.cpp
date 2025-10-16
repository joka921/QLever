// Copyright 2025 The QLever Authors, in particular:
//
// 2025 Johannes Kalmbach <kalmbach@cs.uni-freiburg.de>, UFR
//
// UFR = University of Freiburg, Chair of Algorithms and Data Structures

#include <iostream>

#include "./InterfaceFilling.h"
#include "./QueryTemplates.h"
#include "./SimulationData.h"
#include "libqlever/Qlever.h"
#include "util/Exception.h"
#include "util/Log.h"
#include "util/NullStream.h"
#include "util/Timer.h"

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
  auto queryPointsData = extractQueryPointsData(10);

  std::cout << "pinning the geometries" << std::endl;
  qlever.queryAndPinResultWithName({"geos", Variable{"?geom"}},
                                   qlever::geometryQuery);

  std::cout << "pinning the payload" << std::endl;
  qlever.queryAndPinResultWithName({"payload", std::nullopt},
                                   qlever::payloadQuerySingleColumn);
  for (const auto& pointData : queryPointsData) {
    // Execute query.
    std::cout << "\x1b[1mExecuting test query for point "
              << pointData.coordinates << "\x1b[0m" << std::endl;
    std::string queryResult;
    ad_utility::Timer timer{ad_utility::Timer::Started};
    try {
      qlever.queryAndPinResultWithName(
          {"currentDrivepaths", std::nullopt},
          qlever::getCurrentDrivePathQuery(pointData.coordinates));
      auto plan = qlever.parseAndPlanQuery(qlever::queryTemplateForFeatures);
      qlever.clearCache();
      auto& [qet, qec, parsedQuery] = plan;
      auto result = qlever.getResult(plan, false);
      auto drivePaths = qlever::fillInterfaceForSimpleFeatures(
          *result, qec->getIndex(), qet->getVariableColumns());

      // Execute MPP-based query
      auto mppQuery = qlever::getMppFeaturesQuery(pointData.mppIds);
      auto mppPlan = qlever.parseAndPlanQuery(mppQuery);
      auto mppResult = qlever.getResult(mppPlan, false);
      auto mppDrivePaths = qlever::fillInterfaceForSimpleFeatures(
          *mppResult, qec->getIndex(),
          std::get<0>(mppPlan)->getVariableColumns());
      std::cout << "Found " << drivePaths.size() << " DPs around the car, and "
                << mppDrivePaths.size() << " drive paths from MPP in "
                << timer.value().count() << "us" << std::endl;

      qlever::printDrivePaths(drivePaths, 1);
    } catch (const std::exception& e) {
      std::cerr << "Executing the query failed: " << e.what() << std::endl;
      return 1;
    }
    std::cout.imbue(std::locale(""));
    std::cout << std::endl;
  }
}
