// Copyright 2025 The QLever Authors, in particular:
//
// 2025 Johannes Kalmbach <kalmbach@cs.uni-freiburg.de>, UFR
//
// UFR = University of Freiburg, Chair of Algorithms and Data Structures

#pragma once

#include <cstdint>
#include <string>
#include <string_view>
#include <vector>

namespace qlever {

// SPARQL query templates for drive path queries

extern const std::string payloadQuerySingleColumn;
extern const std::string payloadQuerySpeedProfiles;
extern const std::string geometryQuery;
extern const std::string queryTemplateForCurrentDrivePaths;
extern const std::string queryTemplateForRoadRefToDp;
extern const std::string queryTemplateForDpFeaturesFromIds;
extern const std::string queryTemplateForDpSpeedFromIds;
extern const std::string queryDpToRoadRef;

extern const std::string queryCurrentDrivePathsWithExternalValues;
extern const std::string queryRoadRefToDpWithExternalValues;
extern const std::string queryDpFeaturesFromIdsWithExternalValues;
extern const std::string queryDpSpeedFromIdsWithExternalValues;

// Helper function to get current drive path query with coordinates
std::string getCurrentDrivePathQuery(std::string_view point);

// Helper function to convert MPP IDs to IRIs
std::string mppIdToIri(uint64_t id);

// Helper function to generate VALUES clause with ?roadPart and ?added variables
std::string generateValuesClauseWithAdded(const std::vector<uint64_t>& mppIds,
                                          bool added);

// Helper function to get road ref to drive path mapping query
std::string getRoadRefToDpQuery(const std::vector<uint64_t>& mppIds,
                                bool added);

}  // namespace qlever
