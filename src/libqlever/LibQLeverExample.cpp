// Copyright 2025 The QLever Authors, in particular:
//
// 2025 Johannes Kalmbach <kalmbach@cs.uni-freiburg.de>, UFR
//
// UFR = University of Freiburg, Chair of Algorithms and Data Structures

#include <absl/strings/str_cat.h>

#include <iostream>

#include "./InterfaceFilling.h"
#include "./QueryTemplates.h"
#include "./SimulationData.h"
#include "libqlever/Qlever.h"
#include "util/Exception.h"
#include "util/Timer.h"
#include "util/Views.h"

static const auto filenames = []() {
  std::vector<qlever::InputFileSpecification> res;
  res.push_back(qlever::InputFileSpecification{
      "ttl33588354_v32/full_33588354_v32.ttl", qlever::Filetype::Turtle});
  return res;
};

// Add additional points here...
std::vector<std::string> queryPoints = []() {
  std::vector<std::string> res;
  for (const auto& [mpp, coords] : SIM_DATA | ql::views::drop(10)) {
    res.push_back(absl::StrCat(coords.longitude, " ", coords.latitude));
  }
  return res;
}();

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

  std::cout << "pinning the geometries" << std::endl;
  qlever.queryAndPinResultWithName({"geos", Variable{"?geom"}},
                                   qlever::geometryQuery);

  std::cout << "pinning the payload" << std::endl;
  qlever.queryAndPinResultWithName({"payload", std::nullopt},
                                   qlever::payloadQuerySingleColumn);

  for (const auto& point : queryPoints) {
    // Execute query.
    std::cout << "\x1b[1mExecuting test query for point " << point << "\x1b[0m"
              << std::endl;
    std::string queryResult;
    ad_utility::Timer timer{ad_utility::Timer::Started};
    try {
      qlever.queryAndPinResultWithName({"currentDrivepaths", std::nullopt},
                                       qlever::getCurrentDrivePathQuery(point));
      auto plan = qlever.parseAndPlanQuery(qlever::queryTemplateForFeatures);
      qlever.clearCache();
      auto& [qet, qec, parsedQuery] = plan;
      auto result = qlever.getResult(plan, false);
      auto drivePaths = qlever::fillInterfaceForSimpleFeatures(
          *result, qec->getIndex(), qet->getVariableColumns());
      std::cout << "Found " << drivePaths.size() << " drive paths in "
                << timer.msecs().count() << "ms" << std::endl;
      qlever::printDrivePaths(drivePaths, 1);
    } catch (const std::exception& e) {
      std::cerr << "Executing the query failed: " << e.what() << std::endl;
      return 1;
    }
    std::cout.imbue(std::locale(""));
    std::cout << std::endl;
  }
}
