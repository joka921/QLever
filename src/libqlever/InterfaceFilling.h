// Copyright 2025 The QLever Authors, in particular:
//
// 2025 Johannes Kalmbach <kalmbach@cs.uni-freiburg.de>, UFR
//
// UFR = University of Freiburg, Chair of Algorithms and Data Structures

#pragma once

#include <cstdint>
#include <string>
#include <vector>

#include "engine/Result.h"
#include "engine/VariableToColumnMap.h"
#include "index/Index.h"
#include "util/HashMap.h"

namespace qlever {

struct StopLocation {
  std::string range_;
  bool virtual_;
};

struct SpeedProfile {
  int64_t start_;
  int64_t end_;
  int64_t maxSpeed_;
  int64_t minSpeed_;
};

struct DrivePath {
  int64_t id_;
  std::string shapePoints_;
  std::vector<int64_t> successors_;
  std::vector<int64_t> predecessors_;
  std::vector<StopLocation> stopLocations_;
  std::vector<SpeedProfile> speedProfiles_;
};

// Fill the interface for simple features from a query result
std::vector<DrivePath> fillInterfaceForSimpleFeatures(
    const Result& result, const Index& index,
    const VariableToColumnMap& variableColumns);

// Fill speed profiles for drive paths from a query result
// Returns a map from drive path ID to vector of speed profiles
ad_utility::HashMap<int64_t, std::vector<SpeedProfile>> fillSpeedProfiles(
    const Result& result, const Index& index,
    const VariableToColumnMap& variableColumns);

// Print drive paths to stdout (used for demonstration)
void printDrivePaths(const std::vector<DrivePath>& drivePaths,
                     size_t maxToPrint = 0);

// Print a single drive path with full details
void printDrivePathDetailed(const DrivePath& dp);

}  // namespace qlever
