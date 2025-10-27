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
// For both the ROI and MPP drive paths we store
// 1. The IDs of the drive paths from the previous step that are no longer
// relevant
// 2. The ID + all features of the drive paths that are new.
// 3. The total number of drive paths (numFromPreviousStep + added - removed)
// for logging.
struct QueryStepResult {
  std::vector<Id> removedDrivePathIds;
  std::vector<DrivePath> addedDrivePaths;
  std::vector<DrivePath> mppDrivePaths;
  std::vector<Id> removedMppDrivePathIds;
  size_t totalDrivePaths;
  size_t totalMppDrivePaths;
  // If not the first step, report the distance to the previous point.
  std::optional<double> distanceFromPreviousMeters;
  QueryStepTiming timing;
};

// Manages incremental query execution, tracking drive path changes
class IncrementalQueryExecutor {
 public:
  explicit IncrementalQueryExecutor(Qlever& qlever) : qlever_(qlever) {}

  // Process the next query point and return changes from previous step
  QueryStepResult processNextPoint(const QueryPointData& pointData);

  // Pin geometry and payload queries for efficient reuse. Has to be called
  // before `processNextPoint` is called for the first time, else the
  // queries won't work.
  void pinQueries();

  // Reset state (useful for restarting)
  void reset() {
    previousDrivePathIds_.clear();
    previousCoordinate_.reset();
    isFirstStep_ = true;
  }

 private:
  Qlever& qlever_;
  // The drive paths of the previous step (from the ROI)
  ad_utility::HashSet<Id> previousDrivePathIds_;
  std::optional<Wgs84Coord> previousCoordinate_;
  bool isFirstStep_ = true;

  // MPP diff tracking
  // Road segment ids from the previous MPP.
  std::vector<uint64_t> previousMppIds_;
  // Map from drive path Id to count of road refs leading to it
  ad_utility::HashMap<Id, size_t> previousMppDrivePathCounts_;

  // Query for features from MPP (all features for given MPP IDs)
  std::vector<DrivePath> queryMppFeatures(const std::vector<uint64_t>& mppIds,
                                          const Index& index);

  // Query road ref to drive path mapping.
  // TODO<joka921> Explain and document the `added` argument.
  ad_utility::HashMap<Id, size_t> queryRoadRefToDrivePaths(
      const std::vector<uint64_t>& mppIds, bool added, const Index& index);

  // Query features for specific drive path Ids (from VALUES clause)
  std::vector<DrivePath> queryDrivePathFeaturesFromIds(
      const std::vector<Id>& dpIds, const Index& index);

  // Query speed profiles for specific drive path Ids (from VALUES clause)
  // TODO<joka921> This also seems to be a duplicate function.
  ad_utility::HashMap<Id, std::vector<SpeedProfile>>
  queryDrivePathSpeedProfilesFromIds(const std::vector<Id>& dpIds,
                                     const Index& index);

  // Query both features and speed profiles for drive path IDs and merge them
  std::vector<DrivePath> queryDrivePathsWithFeatures(
      const std::vector<Id>& dpIds, const Index& index);

  // Result of spatial query execution
  struct SpatialQueryResult {
    ad_utility::HashSet<Id> drivePathIds;
    std::shared_ptr<QueryExecutionContext> qec;
  };

  // Execute spatial query and extract drive path IDs, updating timing info
  SpatialQueryResult executeSpatialQuery(const QueryPointData& pointData,
                                         QueryStepResult& result);

  // Result of updating MPP drive path counts
  struct MppUpdateResult {
    std::vector<Id> addedDrivePathIds;
    std::vector<Id> removedDrivePathIds;
    size_t totalCount;
  };

  // Update MPP drive path counts and return added/removed drive paths
  MppUpdateResult updateMppDrivePathCounts(
      const std::vector<uint64_t>& currentMppIds, const Index& index);

  qlever::Qlever::QueryPlan planForSpatialQuery_;
  qlever::Qlever::QueryPlan planForRoadRefToDp_;
  qlever::Qlever::QueryPlan planForDpFeatures_;
  qlever::Qlever::QueryPlan planForDpSpeedProfiles_;

  Qlever::QueryPlan getQueryPlanForSpatialQuery(
      const QueryPointData& pointData);
  Qlever::QueryPlan getQueryPlanForRoadRefToDp(
      const std::vector<uint64_t>& mppIds, bool added);
  Qlever::QueryPlan getQueryPlanForDpFeatures(const std::vector<Id>& dpIds,
                                              const Index& index);
  Qlever::QueryPlan getQueryPlanForDpSpeedProfiles(const std::vector<Id>& dpIds,
                                                   const Index& index);
};

// Print detailed timing breakdown to stdout
void printDetailedTimings(const QueryStepResult& stepResult);

}  // namespace qlever
