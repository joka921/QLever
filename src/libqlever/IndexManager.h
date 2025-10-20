// Copyright 2025 The QLever Authors, in particular:
//
// 2025 Johannes Kalmbach <kalmbach@cs.uni-freiburg.de>, UFR
//
// UFR = University of Freiburg, Chair of Algorithms and Data Structures

#pragma once

#include <memory>
#include <string>

#include "libqlever/Qlever.h"

namespace qlever {

// Build and load a QLever index, returning a ready-to-use Qlever instance
std::unique_ptr<Qlever> buildAndRunQleverIndex(
    const std::string& indexBasename, const IndexBuilderConfig& config);

}  // namespace qlever
