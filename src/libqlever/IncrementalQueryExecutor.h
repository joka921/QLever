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
  std::vector<Id> removedMppDrivePathIds;
  size_t totalDrivePaths;
  size_t totalMppDrivePaths;
  std::optional<double> distanceFromPreviousMeters;
  QueryStepTiming timing;
};

// Manages incremental query execution, tracking drive path changes
class IncrementalQueryExecutor {
 public:
  explicit IncrementalQueryExecutor(Qlever& qlever) : qlever_(qlever) {}

  // Process the next query point and return changes from previous step
  QueryStepResult processNextPoint(const QueryPointData& pointData);

  // Pin geometry and payload queries for efficient reuse
  void pinQueries();

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

  // MPP diff tracking
  std::vector<uint64_t> previousMppIds_;
  // Map from drive path Id to count of road refs leading to it
  ad_utility::HashMap<Id, size_t> previousMppDrivePathCounts_;

  // Query for features of specific drive path IDs only
  std::vector<DrivePath> queryDrivePathFeatures(
      const std::vector<Id>& drivePathIds, const Index& index);

  // Query for features from MPP (all features for given MPP IDs)
  std::vector<DrivePath> queryMppFeatures(const std::vector<uint64_t>& mppIds,
                                          const Index& index);

  // Query for speed profiles of specific drive path IDs
  ad_utility::HashMap<Id, std::vector<SpeedProfile>>
  queryDrivePathSpeedProfiles(const std::vector<Id>& drivePathIds,
                              const Index& index);

  // Query for speed profiles from MPP (all speed profiles for given MPP IDs)
  ad_utility::HashMap<Id, std::vector<SpeedProfile>> queryMppSpeedProfiles(
      const std::vector<uint64_t>& mppIds, const Index& index);

  // Merge speed profiles into drive paths
  void mergeSpeedProfilesIntoDrivePaths(
      std::vector<DrivePath>& drivePaths,
      const ad_utility::HashMap<Id, std::vector<SpeedProfile>>& speedProfiles);

  // Query road ref to drive path mapping
  ad_utility::HashMap<Id, size_t> queryRoadRefToDrivePaths(
      const std::vector<uint64_t>& mppIds, bool added, const Index& index);

  // Query features for specific drive path Ids (from VALUES clause)
  std::vector<DrivePath> queryDrivePathFeaturesFromIds(
      const std::vector<Id>& dpIds, const Index& index);

  // Query speed profiles for specific drive path Ids (from VALUES clause)
  ad_utility::HashMap<Id, std::vector<SpeedProfile>>
  queryDrivePathSpeedProfilesFromIds(const std::vector<Id>& dpIds,
                                     const Index& index);
};

// Print detailed timing breakdown to stdout
void printDetailedTimings(const QueryStepResult& stepResult);

}  // namespace qlever
