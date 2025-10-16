// Copyright 2025 The QLever Authors, in particular:
//
// 2025 Johannes Kalmbach <kalmbach@cs.uni-freiburg.de>, UFR
//
// UFR = University of Freiburg, Chair of Algorithms and Data Structures

#pragma once

#include <string>
#include <string_view>

namespace qlever {

// SPARQL query templates for drive path queries

extern const std::string payloadQuerySingleColumn;
extern const std::string payloadQuerySpeedProfiles;
extern const std::string geometryQuery;
extern const std::string queryTemplateForDrivePaths;
extern const std::string queryTemplateForCurrentDrivePaths;
extern const std::string queryTemplateForFeatures;

// Helper function to instantiate query template with coordinates
std::string getQueryForPoint(std::string_view point);

// Helper function to get current drive path query with coordinates
std::string getCurrentDrivePathQuery(std::string_view point);

}  // namespace qlever
