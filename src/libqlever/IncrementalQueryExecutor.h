// Copyright 2025 The QLever Authors, in particular:
//
// 2025 Johannes Kalmbach <kalmbach@cs.uni-freiburg.de>, UFR
//
// UFR = University of Freiburg, Chair of Algorithms and Data Structures

#pragma once

#include <cstdint>
#include <optional>
#include <vector>

#include "./InterfaceFilling.h"
#include "./SimulationData.h"
#include "global/Id.h"
#include "libqlever/Qlever.h"
#include "util/HashSet.h"

namespace qlever {

// Detailed timing information for a query step
struct QueryStepTiming {
  uint64_t spatialQueryUs = 0;
  uint64_t idExtractionUs = 0;
  uint64_t diffComputationUs = 0;
  uint64_t featureQueryUs = 0;
  uint64_t mppQueryUs = 0;
  uint64_t interfaceFillingUs = 0;
  uint64_t totalUs = 0;
};

// Result of processing a query point, showing changes from previous step
struct QueryStepResult {
  std::vector<Id> removedDrivePathIds;
  std::vector<DrivePath> addedDrivePaths;
  std::vector<DrivePath> mppDrivePaths;
  size_t totalDrivePaths;
  std::optional<double> distanceFromPreviousMeters;
  QueryStepTiming timing;
};

// Manages incremental query execution, tracking drive path changes
class IncrementalQueryExecutor {
 public:
  explicit IncrementalQueryExecutor(Qlever& qlever) : qlever_(qlever) {}

  // Process the next query point and return changes from previous step
  QueryStepResult processNextPoint(const QueryPointData& pointData);

  // Reset state (useful for restarting)
  void reset() {
    previousDrivePathIds_.clear();
    previousCoordinate_.reset();
    isFirstStep_ = true;
  }

 private:
  Qlever& qlever_;
  ad_utility::HashSet<Id> previousDrivePathIds_;
  std::optional<Wgs84Coord> previousCoordinate_;
  bool isFirstStep_ = true;

  // Query for features of specific drive path IDs only
  std::vector<DrivePath> queryDrivePathFeatures(
      const std::vector<Id>& drivePathIds, const Index& index);

  // Query for features from MPP (all features for given MPP IDs)
  std::vector<DrivePath> queryMppFeatures(const std::vector<uint64_t>& mppIds,
                                          const Index& index);
};

}  // namespace qlever
