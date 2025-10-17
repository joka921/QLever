// Copyright 2025 The QLever Authors, in particular:
//
// 2025 Johannes Kalmbach <kalmbach@cs.uni-freiburg.de>, UFR
//
// UFR = University of Freiburg, Chair of Algorithms and Data Structures

#pragma once

#include <optional>
#include <string>

#include "engine/LocalVocab.h"
#include "global/Id.h"
#include "index/Index.h"

namespace qlever {

// Convert an Id to an integer if it stores a numeric type
std::optional<int64_t> getInt(Id id);

// Convert an Id to a double if it stores a numeric type
std::optional<double> getDouble(Id id);

// Convert an Id to a boolean if it stores a boolean type
std::optional<bool> getBool(Id id);

// Convert an Id to a string representation
std::optional<std::string> getString(Id id, const Index& index,
                                     const LocalVocab& localVocab);

}  // namespace qlever
