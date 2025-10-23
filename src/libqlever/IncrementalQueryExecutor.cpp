// Copyright 2025 The QLever Authors, in particular:
//
// 2025 Johannes Kalmbach <kalmbach@cs.uni-freiburg.de>, UFR
//
// UFR = University of Freiburg, Chair of Algorithms and Data Structures

#include "libqlever/IncrementalQueryExecutor.h"

#include <absl/strings/str_replace.h>
#include <s2/s2earth.h>
#include <s2/s2latlng.h>
#include <s2/s2point.h>

#include <algorithm>
#include <iomanip>
#include <iostream>

#include "./IdTableValueExtraction.h"
#include "./QueryTemplates.h"
#include "engine/ExportQueryExecutionTrees.h"
#include "util/Timer.h"

namespace qlever {

// Helper function to extract column index for a variable, with assertion
static ColumnIndex getColumnIndex(const VariableToColumnMap& variableColumns,
                                  const Variable& var) {
  auto it = variableColumns.find(var);
  AD_CONTRACT_CHECK(it != variableColumns.end(),
                    "Variable " + var.name() + " not found in result");
  return it->second.columnIndex_;
}

// Helper to convert vector of Ids to VALUES clause
static std::string buildValuesClauseFromIds(
    const std::vector<Id>& dpIds, const Index& index,
    std::string_view variableName = "?dp") {
  std::vector<std::string> iris;
  iris.reserve(dpIds.size());

  for (const Id& id : dpIds) {
    auto optString =
        ExportQueryExecutionTrees::idToStringAndType(index, id, LocalVocab{});
    if (optString.has_value()) {
      iris.push_back(optString->first);
    }
  }

  if (iris.empty()) {
    return "";
  }

  std::string result = "VALUES " + std::string(variableName) + " { ";
  for (const auto& iri : iris) {
    result += iri + " ";
  }
  result += "}";
  return result;
}

// Calculate distance in meters between two WGS84 coordinates
static double calculateDistanceMeters(const Wgs84Coord& coord1,
                                      const Wgs84Coord& coord2) {
  auto p1 = S2Point{S2LatLng::FromDegrees(coord1.latitude, coord1.longitude)};
  auto p2 = S2Point{S2LatLng::FromDegrees(coord2.latitude, coord2.longitude)};
  // S2Earth::ToKm returns kilometers, convert to meters
  return S2Earth::ToKm(S1Angle(p1, p2)) * 1000.0;
}

// Extract drive path IDs directly from a Result containing ?dp variable
static ad_utility::HashSet<Id> extractDrivePathIdsFromResult(
    const Result& result, const VariableToColumnMap& variableColumns) {
  ad_utility::HashSet<Id> ids;

  const auto& table = result.idTable();
  auto dpColIt = variableColumns.find(Variable{"?dp"});
  if (dpColIt == variableColumns.end()) {
    return ids;
  }

  ColumnIndex dpCol = dpColIt->second.columnIndex_;

  // Extract all unique drive path IDs as Id objects
  for (size_t i = 0; i < table.numRows(); ++i) {
    Id dpId = table(i, dpCol);
    ids.insert(dpId);
  }

  return ids;
}

ad_utility::HashMap<Id, size_t>
IncrementalQueryExecutor::queryRoadRefToDrivePaths(
    const std::vector<uint64_t>& mppIds, bool added, const Index& index) {
  if (mppIds.empty()) {
    return {};
  }

  auto query = getRoadRefToDpQuery(mppIds, added);
  auto plan = qlever_.parseAndPlanQuery(query);
  auto result = qlever_.getResult(plan, false);

  const auto& table = result->idTable();
  auto& variableColumns = std::get<0>(plan)->getVariableColumns();

  ColumnIndex dpCol = getColumnIndex(variableColumns, Variable{"?dp"});
  ColumnIndex cntCol = getColumnIndex(variableColumns, Variable{"?cnt"});

  ad_utility::HashMap<Id, size_t> drivePathCounts;

  for (size_t i = 0; i < table.numRows(); ++i) {
    Id dpId = table(i, dpCol);
    Id cntId = table(i, cntCol);
    auto cnt = getInt(cntId);
    AD_CONTRACT_CHECK(cnt.has_value(),
                      "Count must be an integer at row " + std::to_string(i));
    drivePathCounts[dpId] = static_cast<size_t>(cnt.value());
  }

  return drivePathCounts;
}

std::vector<DrivePath> IncrementalQueryExecutor::queryDrivePathFeaturesFromIds(
    const std::vector<Id>& dpIds, const Index& index) {
  if (dpIds.empty()) {
    return {};
  }

  auto valuesClause = buildValuesClauseFromIds(dpIds, index);
  if (valuesClause.empty()) {
    return {};
  }

  // Use the template and replace #values#
  std::string query =
      absl::StrReplaceAll(queryTemplateForDpFeaturesFromIds,
                          {{std::string_view{"#values#"}, valuesClause}});

  auto plan = qlever_.parseAndPlanQuery(query);
  auto result = qlever_.getResult(plan, false);

  return fillInterfaceForSimpleFeatures(
      *result, index, std::get<0>(plan)->getVariableColumns());
}

ad_utility::HashMap<Id, std::vector<SpeedProfile>>
IncrementalQueryExecutor::queryDrivePathSpeedProfilesFromIds(
    const std::vector<Id>& dpIds, const Index& index) {
  if (dpIds.empty()) {
    return {};
  }

  auto valuesClause = buildValuesClauseFromIds(dpIds, index);
  if (valuesClause.empty()) {
    return {};
  }

  // Use the template and replace #values#
  std::string query =
      absl::StrReplaceAll(queryTemplateForDpSpeedFromIds,
                          {{std::string_view{"#values#"}, valuesClause}});

  auto plan = qlever_.parseAndPlanQuery(query);
  auto result = qlever_.getResult(plan, false);

  return fillSpeedProfiles(*result, index,
                           std::get<0>(plan)->getVariableColumns());
}

std::vector<DrivePath> IncrementalQueryExecutor::queryDrivePathsWithFeatures(
    const std::vector<Id>& dpIds, const Index& index) {
  if (dpIds.empty()) {
    return {};
  }

  // Query features
  auto drivePaths = queryDrivePathFeaturesFromIds(dpIds, index);

  // Query and merge speed profiles
  if (!drivePaths.empty()) {
    auto speedProfiles = queryDrivePathSpeedProfilesFromIds(dpIds, index);
    for (auto& dp : drivePaths) {
      auto it = speedProfiles.find(dp.dpId_);
      if (it != speedProfiles.end()) {
        dp.speedProfiles_ = it->second;
      }
    }
  }

  return drivePaths;
}

// Update MPP drive path counts based on changed MPP IDs, tracking which
// drive paths were added or removed in the process
IncrementalQueryExecutor::MppUpdateResult
IncrementalQueryExecutor::updateMppDrivePathCounts(
    const std::vector<uint64_t>& currentMppIds, const Index& index) {
  MppUpdateResult result;

  // Calculate diff in MPP IDs
  ad_utility::HashSet<uint64_t> prevMppSet(previousMppIds_.begin(),
                                           previousMppIds_.end());
  ad_utility::HashSet<uint64_t> currMppSet(currentMppIds.begin(),
                                           currentMppIds.end());

  std::vector<uint64_t> addedMppIds;
  std::vector<uint64_t> removedMppIds;

  for (uint64_t mppId : currentMppIds) {
    if (prevMppSet.find(mppId) == prevMppSet.end()) {
      addedMppIds.push_back(mppId);
    }
  }

  for (uint64_t mppId : previousMppIds_) {
    if (currMppSet.find(mppId) == currMppSet.end()) {
      removedMppIds.push_back(mppId);
    }
  }

  // Update counts based on added MPP road refs
  if (!addedMppIds.empty()) {
    auto addedCounts = queryRoadRefToDrivePaths(addedMppIds, true, index);
    for (const auto& [dpId, cnt] : addedCounts) {
      auto& currentCount = previousMppDrivePathCounts_[dpId];
      if (currentCount == 0) {
        // This is a newly added drive path
        result.addedDrivePathIds.push_back(dpId);
      }
      currentCount += cnt;
    }
  }

  // Update counts based on removed MPP road refs
  if (!removedMppIds.empty()) {
    auto removedCounts = queryRoadRefToDrivePaths(removedMppIds, false, index);
    for (const auto& [dpId, cnt] : removedCounts) {
      auto it = previousMppDrivePathCounts_.find(dpId);
      if (it != previousMppDrivePathCounts_.end()) {
        if (it->second <= cnt) {
          // This drive path is being completely removed
          result.removedDrivePathIds.push_back(dpId);
          previousMppDrivePathCounts_.erase(it);
        } else {
          it->second -= cnt;
        }
      }
    }
  }

  result.totalCount = previousMppDrivePathCounts_.size();
  return result;
}

// _____________________________________________________________________________
QueryStepResult IncrementalQueryExecutor::processNextPoint(
    const QueryPointData& pointData) {
  ad_utility::Timer totalTimer{ad_utility::Timer::Started};
  QueryStepResult result;

  // Calculate distance from previous point (if not first step)
  if (!isFirstStep_ && previousCoordinate_.has_value()) {
    result.distanceFromPreviousMeters = calculateDistanceMeters(
        previousCoordinate_.value(), pointData.wgs84Coord);
  }

  // Time: Execute spatial query to get current drive paths around the car
  ad_utility::Timer spatialTimer{ad_utility::Timer::Started};
  auto [spatialResult, spatialPlan] =
      qlever_.queryAndPinResultWithNameReturningResult(
          {"currentDrivepaths", std::nullopt},
          getCurrentDrivePathQuery(pointData.coordinates));
  auto& [spatialQet, spatialQec, spatialParsedQuery] = spatialPlan;
  result.timing.spatialQueryUs = spatialTimer.value().count();

  // Time: Extract IDs from spatial result
  ad_utility::Timer idExtractionTimer{ad_utility::Timer::Started};
  ad_utility::HashSet<Id> currentDrivePathIds = extractDrivePathIdsFromResult(
      *spatialResult, spatialQet->getVariableColumns());
  result.timing.idExtractionUs = idExtractionTimer.value().count();

  if (isFirstStep_) {
    // First step: get all features for all drive paths from spatial query
    ad_utility::Timer diffTimer{ad_utility::Timer::Started};
    std::vector<Id> allIds(currentDrivePathIds.begin(),
                           currentDrivePathIds.end());
    result.timing.diffComputationUs = diffTimer.value().count();

    if (!allIds.empty()) {
      ad_utility::Timer featureTimer{ad_utility::Timer::Started};
      result.addedDrivePaths =
          queryDrivePathsWithFeatures(allIds, spatialQec->getIndex());
      result.timing.featureQueryUs = featureTimer.value().count();
    }

    // Time: Query all features from MPP (full query, no diff for first step)
    ad_utility::Timer mppTimer{ad_utility::Timer::Started};

    // Build the initial road ref to drive path mapping for current MPPs
    previousMppDrivePathCounts_ = queryRoadRefToDrivePaths(
        pointData.mppIds, true, spatialQec->getIndex());

    // Get all drive path IDs from MPP
    std::vector<Id> mppDpIds;
    mppDpIds.reserve(previousMppDrivePathCounts_.size());
    for (const auto& [dpId, cnt] : previousMppDrivePathCounts_) {
      mppDpIds.push_back(dpId);
    }

    // Query features and speed profiles for all MPP drive paths
    result.mppDrivePaths =
        queryDrivePathsWithFeatures(mppDpIds, spatialQec->getIndex());

    result.timing.mppQueryUs = mppTimer.value().count();

    // Store state for next iteration
    previousDrivePathIds_ = std::move(currentDrivePathIds);
    result.totalDrivePaths = previousDrivePathIds_.size();
    result.totalMppDrivePaths = previousMppDrivePathCounts_.size();
    previousCoordinate_ = pointData.wgs84Coord;
    previousMppIds_ = pointData.mppIds;
    isFirstStep_ = false;
  } else {
    // Time: Calculate diff (removed and added IDs)
    ad_utility::Timer diffTimer{ad_utility::Timer::Started};

    // Calculate removed IDs: in previous but not in current
    for (const Id& prevId : previousDrivePathIds_) {
      if (currentDrivePathIds.find(prevId) == currentDrivePathIds.end()) {
        result.removedDrivePathIds.push_back(prevId);
      }
    }

    // Calculate added IDs: in current but not in previous
    std::vector<Id> addedIds;
    for (const Id& currentId : currentDrivePathIds) {
      if (previousDrivePathIds_.find(currentId) ==
          previousDrivePathIds_.end()) {
        addedIds.push_back(currentId);
      }
    }
    result.timing.diffComputationUs = diffTimer.value().count();

    // Time: Query features from payload only for newly added drive paths
    if (!addedIds.empty()) {
      ad_utility::Timer featureTimer{ad_utility::Timer::Started};
      result.addedDrivePaths =
          queryDrivePathsWithFeatures(addedIds, spatialQec->getIndex());
      result.timing.featureQueryUs = featureTimer.value().count();
    }

    // Time: Query features from MPP with diff-based approach
    ad_utility::Timer mppTimer{ad_utility::Timer::Started};

    // Update MPP drive path counts and get added/removed drive paths
    auto mppUpdate =
        updateMppDrivePathCounts(pointData.mppIds, spatialQec->getIndex());

    result.removedMppDrivePathIds = std::move(mppUpdate.removedDrivePathIds);
    result.totalMppDrivePaths = mppUpdate.totalCount;

    // Query features only for newly added drive paths
    if (!mppUpdate.addedDrivePathIds.empty()) {
      result.mppDrivePaths = queryDrivePathsWithFeatures(
          mppUpdate.addedDrivePathIds, spatialQec->getIndex());
    }

    result.timing.mppQueryUs = mppTimer.value().count();

    // Update state for next iteration
    previousDrivePathIds_ = std::move(currentDrivePathIds);
    previousCoordinate_ = pointData.wgs84Coord;
    result.totalDrivePaths = previousDrivePathIds_.size();
    previousMppIds_ = pointData.mppIds;
  }

  result.timing.totalUs = totalTimer.value().count();
  return result;
}

void IncrementalQueryExecutor::pinQueries() {
  ad_utility::Timer timer{ad_utility::Timer::Started};
  std::cout << "pinning the geometries" << std::endl;
  qlever_.queryAndPinResultWithName({"geos", Variable{"?geom"}},
                                    qlever::geometryQuery);

  std::cout << "pinning the payload" << std::endl;
  qlever_.queryAndPinResultWithName({"payload", std::nullopt},
                                    qlever::payloadQuerySingleColumn);

  std::cout << "pinning the speed profiles" << std::endl;
  qlever_.queryAndPinResultWithName({"speed", std::nullopt},
                                    qlever::payloadQuerySpeedProfiles);
  std::cout << "Time for pinning the queries " << timer.msecs().count() << "ms"
            << std::endl;
}

void printDetailedTimings(const QueryStepResult& stepResult) {
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

}  // namespace qlever
