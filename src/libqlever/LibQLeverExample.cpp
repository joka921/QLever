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
#include "util/Log.h"
#include "util/Timer.h"
#include "util/Views.h"

// Null output stream that discards all input
class NullStream : public std::ostream {
 private:
  class NullBuffer : public std::streambuf {
   public:
    int overflow(int c) override { return c; }
  };
  NullBuffer buffer_;

 public:
  NullStream() : std::ostream(&buffer_) {}
};

static const auto filenames = []() {
  std::vector<qlever::InputFileSpecification> res;
  res.push_back(qlever::InputFileSpecification{
      "ttl33588354_v32/full_33588354_v32.ttl", qlever::Filetype::Turtle});
  return res;
};

// Data structure to hold both coordinates and MPP IDs for each point
struct QueryPointData {
  std::string coordinates;
  std::vector<uint64_t> mppIds;
};

// Extract query points and MPP data from simulation data
std::vector<QueryPointData> queryPointsData = []() {
  std::vector<QueryPointData> res;
  for (const auto& [mpp, coords] : SIM_DATA | ql::views::drop(10)) {
    QueryPointData data;
    data.coordinates = absl::StrCat(coords.longitude, " ", coords.latitude);
    data.mppIds = mpp;
    res.push_back(std::move(data));
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

  // Suppress QLever internal logging
  static NullStream nullStream;
  ad_utility::setGlobalLoggingStream(&nullStream);

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

      qlever::printDrivePaths(drivePaths, 0);
    } catch (const std::exception& e) {
      std::cerr << "Executing the query failed: " << e.what() << std::endl;
      return 1;
    }
    std::cout.imbue(std::locale(""));
    std::cout << std::endl;
  }
}
