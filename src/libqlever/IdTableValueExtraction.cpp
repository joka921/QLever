// Copyright 2025 The QLever Authors, in particular:
//
// 2025 Johannes Kalmbach <kalmbach@cs.uni-freiburg.de>, UFR
//
// UFR = University of Freiburg, Chair of Algorithms and Data Structures

#include "libqlever/IdTableValueExtraction.h"

#include <cmath>
#include <limits>

#include "engine/ExportQueryExecutionTrees.h"

namespace qlever {

// Convert an Id to an integer if it stores a numeric type
std::optional<int64_t> getInt(Id id) {
  using enum Datatype;
  switch (id.getDatatype()) {
    case Int:
      return id.getInt();
    case Double: {
      double d = id.getDouble();
      if (std::isfinite(d) && d >= std::numeric_limits<int64_t>::min() &&
          d <= std::numeric_limits<int64_t>::max()) {
        return static_cast<int64_t>(d);
      }
      return std::nullopt;
    }
    default:
      return std::nullopt;
  }
}

// Convert an Id to a double if it stores a numeric type
std::optional<double> getDouble(Id id) {
  using enum Datatype;
  switch (id.getDatatype()) {
    case Double:
      return id.getDouble();
    case Int:
      return static_cast<double>(id.getInt());
    default:
      return std::nullopt;
  }
}

// Convert an Id to a boolean if it stores a boolean type
std::optional<bool> getBool(Id id) {
  using enum Datatype;
  if (id.getDatatype() == Bool) {
    return id.getBool();
  }
  return std::nullopt;
}

// Convert an Id to a string representation
std::optional<std::string> getString(Id id, const Index& index,
                                     const LocalVocab& localVocab) {
  auto optionalStringAndType =
      ExportQueryExecutionTrees::idToStringAndType(index, id, localVocab);
  if (!optionalStringAndType.has_value()) {
    return std::nullopt;
  }
  return optionalStringAndType->first;
}

}  // namespace qlever
