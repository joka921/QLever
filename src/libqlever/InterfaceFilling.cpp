// Copyright 2025 The QLever Authors, in particular:
//
// 2025 Johannes Kalmbach <kalmbach@cs.uni-freiburg.de>, UFR
//
// UFR = University of Freiburg, Chair of Algorithms and Data Structures

#include "libqlever/InterfaceFilling.h"

#include <algorithm>
#include <iostream>

#include "libqlever/IdTableValueExtraction.h"
#include "util/Exception.h"
#include "util/Views.h"

namespace qlever {

std::vector<DrivePath> fillInterfaceForSimpleFeatures(
    const Result& result, const Index& index,
    const VariableToColumnMap& variableColumns) {
  AD_CONTRACT_CHECK(result.isFullyMaterialized(),
                    "Result must be fully materialized");

  const auto& table = result.idTable();
  const auto& localVocab = result.localVocab();

  // Get column indices for the required variables
  auto getDpCol = variableColumns.find(Variable{"?dp"});
  auto getTypeCol = variableColumns.find(Variable{"?type"});
  auto getC1Col = variableColumns.find(Variable{"?c1"});

  AD_CONTRACT_CHECK(getDpCol != variableColumns.end(),
                    "Variable ?dp not found in result");
  AD_CONTRACT_CHECK(getTypeCol != variableColumns.end(),
                    "Variable ?type not found in result");
  AD_CONTRACT_CHECK(getC1Col != variableColumns.end(),
                    "Variable ?c1 not found in result");

  ColumnIndex dpCol = getDpCol->second.columnIndex_;
  ColumnIndex typeCol = getTypeCol->second.columnIndex_;
  ColumnIndex c1Col = getC1Col->second.columnIndex_;

  std::vector<DrivePath> drivePaths;

  // Process the table in blocks of equal ?dp values
  size_t i = 0;
  while (i < table.numRows()) {
    Id currentDp = table(i, dpCol);

    // Find all rows with the same ?dp value
    size_t blockStart = i;
    size_t blockEnd = i;
    while (blockEnd < table.numRows() && table(blockEnd, dpCol) == currentDp) {
      ++blockEnd;
    }

    // Process this block to extract id, shapePoints, successors, and
    // predecessors
    std::optional<int64_t> drivePathId;
    std::optional<std::string> shapePoints;
    std::vector<int64_t> successors;
    std::vector<int64_t> predecessors;

    for (size_t j = blockStart; j < blockEnd; ++j) {
      Id typeId = table(j, typeCol);
      auto typeValue = getInt(typeId);

      AD_CONTRACT_CHECK(
          typeValue.has_value(),
          "Type value must be an integer at row " + std::to_string(j));

      if (typeValue.value() == 0) {
        // This row contains a successor
        Id c1Id = table(j, c1Col);
        auto successor = getInt(c1Id);
        AD_CONTRACT_CHECK(successor.has_value(),
                          "Successor (type=0) must be an integer at row " +
                              std::to_string(j));
        successors.push_back(successor.value());
      } else if (typeValue.value() == 1) {
        // This row contains a predecessor
        Id c1Id = table(j, c1Col);
        auto predecessor = getInt(c1Id);
        AD_CONTRACT_CHECK(predecessor.has_value(),
                          "Predecessor (type=1) must be an integer at row " +
                              std::to_string(j));
        predecessors.push_back(predecessor.value());
      } else if (typeValue.value() == 2) {
        // This row contains the drive path id
        AD_CONTRACT_CHECK(!drivePathId.has_value(),
                          "Multiple rows with type=2 for the same drive path");
        Id c1Id = table(j, c1Col);
        drivePathId = getInt(c1Id);
        AD_CONTRACT_CHECK(drivePathId.has_value(),
                          "Drive path id (type=2) must be an integer at row " +
                              std::to_string(j));
      } else if (typeValue.value() == 3) {
        // This row contains the shape points
        AD_CONTRACT_CHECK(!shapePoints.has_value(),
                          "Multiple rows with type=3 for the same drive path");
        Id c1Id = table(j, c1Col);
        shapePoints = getString(c1Id, index, localVocab);
        AD_CONTRACT_CHECK(shapePoints.has_value(),
                          "Shape points (type=3) must be a string at row " +
                              std::to_string(j));
      }
    }

    // Verify we found both required values
    AD_CONTRACT_CHECK(drivePathId.has_value(),
                      "No row with type=2 found for drive path");
    AD_CONTRACT_CHECK(shapePoints.has_value(),
                      "No row with type=3 found for drive path");

    // Create DrivePath object
    drivePaths.push_back(
        DrivePath{drivePathId.value(), std::move(shapePoints.value()),
                  std::move(successors), std::move(predecessors)});

    // Move to the next block
    i = blockEnd;
  }
  return drivePaths;
}

void printDrivePaths(const std::vector<DrivePath>& drivePaths,
                     size_t maxToPrint) {
  for (const auto& dp : drivePaths | ql::views::take(maxToPrint)) {
    std::cout << "Drive path " << dp.id_ << ":" << std::endl;
    std::cout << "  Shape points: "
              << std::string_view{dp.shapePoints_}.substr(0, 100) << std::endl;
    std::cout << "  Successors (" << dp.successors_.size() << "): ";
    for (size_t i = 0; i < std::min(dp.successors_.size(), size_t{5}); ++i) {
      if (i > 0) std::cout << ", ";
      std::cout << dp.successors_[i];
    }
    if (dp.successors_.size() > 5) std::cout << "...";
    std::cout << std::endl;
    std::cout << "  Predecessors (" << dp.predecessors_.size() << "): ";
    for (size_t i = 0; i < std::min(dp.predecessors_.size(), size_t{5}); ++i) {
      if (i > 0) std::cout << ", ";
      std::cout << dp.predecessors_[i];
    }
    if (dp.predecessors_.size() > 5) std::cout << "...";
    std::cout << std::endl;
  }
}

}  // namespace qlever
