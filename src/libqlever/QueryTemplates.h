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
extern const std::string queryTemplateForDrivePaths;
extern const std::string queryTemplateForCurrentDrivePaths;
extern const std::string queryTemplateForFeatures;
extern const std::string queryTemplateForMppFeatures;

// Helper function to instantiate query template with coordinates
std::string getQueryForPoint(std::string_view point);

// Helper function to get current drive path query with coordinates
std::string getCurrentDrivePathQuery(std::string_view point);

// Helper function to convert MPP IDs to IRIs
std::string mppIdToIri(uint64_t id);

// Helper function to generate VALUES clause from MPP IDs
std::string generateValuesClause(const std::vector<uint64_t>& mppIds);

// Helper function to get MPP-based query for features
std::string getMppFeaturesQuery(const std::vector<uint64_t>& mppIds);

}  // namespace qlever
