// Copyright 2025 The QLever Authors, in particular:
//
// 2025 Johannes Kalmbach <kalmbach@cs.uni-freiburg.de>, UFR
//
// UFR = University of Freiburg, Chair of Algorithms and Data Structures

#include "libqlever/IncrementalQueryExecutor.h"

#include <s2/s2earth.h>
#include <s2/s2latlng.h>
#include <s2/s2point.h>

#include <algorithm>
#include <iomanip>
#include <iostream>

#include "./QueryTemplates.h"
#include "engine/ExportQueryExecutionTrees.h"
#include "util/Timer.h"

namespace qlever {

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

std::vector<DrivePath> IncrementalQueryExecutor::queryDrivePathFeatures(
    const std::vector<Id>& drivePathIds, const Index& index) {
  if (drivePathIds.empty()) {
    return {};
  }

  // Build a VALUES clause with the actual Id IRIs
  // We need to get the IRI strings for these Ids to use in the query
  std::vector<std::string> iris;
  iris.reserve(drivePathIds.size());

  for (const Id& id : drivePathIds) {
    // Get the string representation of this Id (IRI)
    auto optString =
        ExportQueryExecutionTrees::idToStringAndType(index, id, LocalVocab{});
    if (optString.has_value()) {
      iris.push_back(optString->first);
    }
  }

  if (iris.empty()) {
    return {};
  }

  // Build VALUES clause with the actual IRIs
  std::string valuesClause = "VALUES ?dp { ";
  for (const auto& iri : iris) {
    valuesClause += iri + " ";
  }
  valuesClause += "}";

  // Build query with VALUES clause
  std::string query = R"(
PREFIX lbm: <http://www.bmw-carit.de/Foresight/Map/Ontologies/Low/behaviorMap#>
SELECT ?dp ?type ?c1 ?c2 WHERE {
  {
    SELECT ?dp {
      )" + valuesClause +
                      R"(
    }
  }
  {
    SELECT ?dp ?type ?c1 ?c2 {
      SERVICE ql:cached-result-with-name-payload {}
    }
  }
})";

  auto plan = qlever_.parseAndPlanQuery(query);
  auto result = qlever_.getResult(plan, false);

  // Fill interface with the results
  return fillInterfaceForSimpleFeatures(
      *result, index, std::get<0>(plan)->getVariableColumns());
}

std::vector<DrivePath> IncrementalQueryExecutor::queryMppFeatures(
    const std::vector<uint64_t>& mppIds, const Index& index) {
  if (mppIds.empty()) {
    return {};
  }

  // Use the existing getMppFeaturesQuery function
  auto query = getMppFeaturesQuery(mppIds);
  auto plan = qlever_.parseAndPlanQuery(query);
  auto result = qlever_.getResult(plan, false);

  // Fill interface with the results
  return fillInterfaceForSimpleFeatures(
      *result, index, std::get<0>(plan)->getVariableColumns());
}

ad_utility::HashMap<Id, std::vector<SpeedProfile>>
IncrementalQueryExecutor::queryDrivePathSpeedProfiles(
    const std::vector<Id>& drivePathIds, const Index& index) {
  if (drivePathIds.empty()) {
    return {};
  }

  // Build a VALUES clause with the actual Id IRIs
  std::vector<std::string> iris;
  iris.reserve(drivePathIds.size());

  for (const Id& id : drivePathIds) {
    auto optString =
        ExportQueryExecutionTrees::idToStringAndType(index, id, LocalVocab{});
    if (optString.has_value()) {
      iris.push_back(optString->first);
    }
  }

  if (iris.empty()) {
    return {};
  }

  // Build VALUES clause with the actual IRIs
  std::string valuesClause = "VALUES ?dp { ";
  for (const auto& iri : iris) {
    valuesClause += iri + " ";
  }
  valuesClause += "}";

  // Build query with VALUES clause
  std::string query = R"(
PREFIX lbm: <http://www.bmw-carit.de/Foresight/Map/Ontologies/Low/behaviorMap#>
SELECT ?dp ?start ?end ?minSpeed ?maxSpeed WHERE {
  {
    SELECT ?dp {
      )" + valuesClause +
                      R"(
    }
  }
  {
    SELECT ?dp ?start ?end ?minSpeed ?maxSpeed {
      SERVICE ql:cached-result-with-name-speed {}
    }
  }
})";

  auto plan = qlever_.parseAndPlanQuery(query);
  auto result = qlever_.getResult(plan, false);

  // Fill speed profiles from the results
  return fillSpeedProfiles(*result, index,
                           std::get<0>(plan)->getVariableColumns());
}

ad_utility::HashMap<Id, std::vector<SpeedProfile>>
IncrementalQueryExecutor::queryMppSpeedProfiles(
    const std::vector<uint64_t>& mppIds, const Index& index) {
  if (mppIds.empty()) {
    return {};
  }

  // Build VALUES clause for MPP IDs
  std::string valuesClause = generateValuesClause(mppIds);

  // Build query with VALUES clause
  std::string query = R"(
PREFIX lbm: <http://www.bmw-carit.de/Foresight/Map/Ontologies/Low/behaviorMap#>
SELECT ?dp ?start ?end ?minSpeed ?maxSpeed WHERE {
  {
    SELECT DISTINCT ?dp {
      )" + valuesClause +
                      R"(
     ?roadPart lbm:hasDrivePaths ?dp
    }
  }
  {
    SELECT ?dp ?start ?end ?minSpeed ?maxSpeed {
      SERVICE ql:cached-result-with-name-speed {}
    }
  }
})";

  auto plan = qlever_.parseAndPlanQuery(query);
  auto result = qlever_.getResult(plan, false);

  // Fill speed profiles from the results
  return fillSpeedProfiles(*result, index,
                           std::get<0>(plan)->getVariableColumns());
}

void IncrementalQueryExecutor::mergeSpeedProfilesIntoDrivePaths(
    std::vector<DrivePath>& drivePaths,
    const ad_utility::HashMap<Id, std::vector<SpeedProfile>>& speedProfiles) {
  std::cout << "Merging speed profiles" << std::endl;
  for (auto& dp : drivePaths) {
    // Use dpId_ instead of id_ for matching
    auto it = speedProfiles.find(dp.dpId_);
    if (it != speedProfiles.end()) {
      dp.speedProfiles_ = it->second;
    }
  }
}

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
  auto spatialResult = qlever_.queryAndPinResultWithNameReturningResult(
      {"currentDrivepaths", std::nullopt},
      getCurrentDrivePathQuery(pointData.coordinates));
  result.timing.spatialQueryUs = spatialTimer.value().count();

  // Time: Extract IDs from spatial result
  ad_utility::Timer idExtractionTimer{ad_utility::Timer::Started};
  auto spatialPlan =
      qlever_.parseAndPlanQuery(queryTemplateForCurrentDrivePaths);
  auto& [spatialQet, spatialQec, spatialParsedQuery] = spatialPlan;
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
          queryDrivePathFeatures(allIds, spatialQec->getIndex());
      result.timing.featureQueryUs = featureTimer.value().count();

      // Query speed profiles for added drive paths
      auto speedProfiles =
          queryDrivePathSpeedProfiles(allIds, spatialQec->getIndex());
      mergeSpeedProfilesIntoDrivePaths(result.addedDrivePaths, speedProfiles);
    }

    // Time: Query all features from MPP (full query, no diff)
    ad_utility::Timer mppTimer{ad_utility::Timer::Started};
    result.mppDrivePaths =
        queryMppFeatures(pointData.mppIds, spatialQec->getIndex());

    // Query speed profiles for MPP drive paths
    auto mppSpeedProfiles =
        queryMppSpeedProfiles(pointData.mppIds, spatialQec->getIndex());
    mergeSpeedProfilesIntoDrivePaths(result.mppDrivePaths, mppSpeedProfiles);

    result.timing.mppQueryUs = mppTimer.value().count();

    // Store IDs and coordinate for next iteration
    previousDrivePathIds_ = std::move(currentDrivePathIds);
    result.totalDrivePaths = previousDrivePathIds_.size();
    previousCoordinate_ = pointData.wgs84Coord;
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
          queryDrivePathFeatures(addedIds, spatialQec->getIndex());
      result.timing.featureQueryUs = featureTimer.value().count();

      // Query speed profiles for added drive paths
      auto speedProfiles =
          queryDrivePathSpeedProfiles(addedIds, spatialQec->getIndex());
      mergeSpeedProfilesIntoDrivePaths(result.addedDrivePaths, speedProfiles);
    }

    // Time: Query all features from MPP (full query, no diff)
    ad_utility::Timer mppTimer{ad_utility::Timer::Started};
    result.mppDrivePaths =
        queryMppFeatures(pointData.mppIds, spatialQec->getIndex());

    // Query speed profiles for MPP drive paths
    auto mppSpeedProfiles =
        queryMppSpeedProfiles(pointData.mppIds, spatialQec->getIndex());
    mergeSpeedProfilesIntoDrivePaths(result.mppDrivePaths, mppSpeedProfiles);

    result.timing.mppQueryUs = mppTimer.value().count();

    // Update state for next iteration
    previousDrivePathIds_ = std::move(currentDrivePathIds);
    previousCoordinate_ = pointData.wgs84Coord;
    result.totalDrivePaths = previousDrivePathIds_.size();
  }

  result.timing.totalUs = totalTimer.value().count();
  return result;
}

void IncrementalQueryExecutor::pinQueries() {
  std::cout << "pinning the geometries" << std::endl;
  qlever_.queryAndPinResultWithName({"geos", Variable{"?geom"}},
                                    qlever::geometryQuery);

  std::cout << "pinning the payload" << std::endl;
  qlever_.queryAndPinResultWithName({"payload", std::nullopt},
                                    qlever::payloadQuerySingleColumn);

  std::cout << "pinning the speed profiles" << std::endl;
  qlever_.queryAndPinResultWithName({"speed", std::nullopt},
                                    qlever::payloadQuerySpeedProfiles);
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
