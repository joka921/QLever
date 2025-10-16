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

namespace qlever {

struct DrivePath {
  int64_t id_;
  std::string shapePoints_;
  std::vector<int64_t> successors_;
  std::vector<int64_t> predecessors_;
};

// Fill the interface for simple features from a query result
std::vector<DrivePath> fillInterfaceForSimpleFeatures(
    const Result& result, const Index& index,
    const VariableToColumnMap& variableColumns);

// Print drive paths to stdout (used for demonstration)
void printDrivePaths(const std::vector<DrivePath>& drivePaths,
                     size_t maxToPrint = 0);

}  // namespace qlever
