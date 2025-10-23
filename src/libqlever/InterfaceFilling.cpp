// Copyright 2025 The QLever Authors, in particular:
//
// 2025 Johannes Kalmbach <kalmbach@cs.uni-freiburg.de>, UFR
//
// UFR = University of Freiburg, Chair of Algorithms and Data Structures

#include "libqlever/InterfaceFilling.h"

#include <algorithm>
#include <boost/qvm/map_mat_vec.hpp>
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
  auto getC2Col = variableColumns.find(Variable{"?c2"});

  AD_CONTRACT_CHECK(getDpCol != variableColumns.end(),
                    "Variable ?dp not found in result");
  AD_CONTRACT_CHECK(getTypeCol != variableColumns.end(),
                    "Variable ?type not found in result");
  AD_CONTRACT_CHECK(getC1Col != variableColumns.end(),
                    "Variable ?c1 not found in result");

  ColumnIndex dpCol = getDpCol->second.columnIndex_;
  ColumnIndex typeCol = getTypeCol->second.columnIndex_;
  ColumnIndex c1Col = getC1Col->second.columnIndex_;
  std::optional<ColumnIndex> c2Col;
  if (getC2Col != variableColumns.end()) {
    c2Col = getC2Col->second.columnIndex_;
  }

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

    // Process this block to extract id, shapePoints, successors, predecessors,
    // and stop locations
    std::optional<int64_t> drivePathId;
    std::optional<std::string> shapePoints;
    std::vector<int64_t> successors;
    std::vector<int64_t> predecessors;
    std::vector<StopLocation> stopLocations;

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
      } else if (typeValue.value() == 4) {
        // This row contains a stop location (range and virtual)
        AD_CONTRACT_CHECK(c2Col.has_value(),
                          "Variable ?c2 required for stop locations (type=4)");
        Id c1Id = table(j, c1Col);
        Id c2Id = table(j, c2Col.value());

        auto range = getString(c1Id, index, localVocab);
        AD_CONTRACT_CHECK(
            range.has_value(),
            "Stop location range (type=4) must be a string at row " +
                std::to_string(j));

        auto virtualVal = getBool(c2Id);
        AD_CONTRACT_CHECK(
            virtualVal.has_value(),
            "Stop location virtual (type=4) must be a bool at row " +
                std::to_string(j));

        stopLocations.push_back(
            StopLocation{std::move(range.value()), virtualVal.value()});
      }
    }

    // Verify we found both required values
    AD_CONTRACT_CHECK(drivePathId.has_value(),
                      "No row with type=2 found for drive path");
    AD_CONTRACT_CHECK(shapePoints.has_value(),
                      "No row with type=3 found for drive path");

    // Create DrivePath object
    drivePaths.push_back(DrivePath{drivePathId.value(),
                                   currentDp,  // Store the ?dp Id value
                                   std::move(shapePoints.value()),
                                   std::move(successors),
                                   std::move(predecessors),
                                   std::move(stopLocations),
                                   {}});

    // Move to the next block
    i = blockEnd;
  }
  return drivePaths;
}

ad_utility::HashMap<Id, std::vector<SpeedProfile>> fillSpeedProfiles(
    const Result& result, [[maybe_unused]] const Index& index,
    const VariableToColumnMap& variableColumns) {
  AD_CONTRACT_CHECK(result.isFullyMaterialized(),
                    "Result must be fully materialized");

  const auto& table = result.idTable();

  // Get column indices for the required variables
  auto getDpCol = variableColumns.find(Variable{"?dp"});
  auto getStartCol = variableColumns.find(Variable{"?start"});
  auto getEndCol = variableColumns.find(Variable{"?end"});
  auto getMaxSpeedCol = variableColumns.find(Variable{"?maxSpeed"});
  auto getMinSpeedCol = variableColumns.find(Variable{"?minSpeed"});

  AD_CONTRACT_CHECK(getDpCol != variableColumns.end(),
                    "Variable ?dp not found in result");
  AD_CONTRACT_CHECK(getStartCol != variableColumns.end(),
                    "Variable ?start not found in result");
  AD_CONTRACT_CHECK(getEndCol != variableColumns.end(),
                    "Variable ?end not found in result");
  AD_CONTRACT_CHECK(getMaxSpeedCol != variableColumns.end(),
                    "Variable ?maxSpeed not found in result");
  AD_CONTRACT_CHECK(getMinSpeedCol != variableColumns.end(),
                    "Variable ?minSpeed not found in result");

  ColumnIndex dpCol = getDpCol->second.columnIndex_;
  ColumnIndex startCol = getStartCol->second.columnIndex_;
  ColumnIndex endCol = getEndCol->second.columnIndex_;
  ColumnIndex maxSpeedCol = getMaxSpeedCol->second.columnIndex_;
  ColumnIndex minSpeedCol = getMinSpeedCol->second.columnIndex_;

  ad_utility::HashMap<Id, std::vector<SpeedProfile>> speedProfilesMap;

  // Process each row
  for (size_t i = 0; i < table.numRows(); ++i) {
    // Extract drive path ID directly from the ?dp variable
    Id dpId = table(i, dpCol);

    // Extract speed profile values
    auto start = getInt(table(i, startCol));
    auto end = getInt(table(i, endCol));
    auto maxSpeed = getInt(table(i, maxSpeedCol));
    auto minSpeed = getInt(table(i, minSpeedCol));

    AD_CONTRACT_CHECK(start.has_value(),
                      "Start must be an integer at row " + std::to_string(i));
    AD_CONTRACT_CHECK(end.has_value(),
                      "End must be an integer at row " + std::to_string(i));
    AD_CONTRACT_CHECK(
        maxSpeed.has_value(),
        "MaxSpeed must be an integer at row " + std::to_string(i));
    AD_CONTRACT_CHECK(
        minSpeed.has_value(),
        "MinSpeed must be an integer at row " + std::to_string(i));

    // Use the Id directly as the key
    speedProfilesMap[dpId].push_back(SpeedProfile{
        start.value(), end.value(), maxSpeed.value(), minSpeed.value()});
  }

  return speedProfilesMap;
}

void printDrivePathDetailed(const DrivePath& dp) {
  std::cout << "    Drive path ID: " << dp.id_ << std::endl;

  // Shape points
  std::cout << "      Shape points: "
            << std::string_view{dp.shapePoints_}.substr(0, 100);
  if (dp.shapePoints_.size() > 100) std::cout << "...";
  std::cout << std::endl;

  // Successors
  std::cout << "      Successors (" << dp.successors_.size() << "): ";
  for (size_t i = 0; i < std::min(dp.successors_.size(), size_t{5}); ++i) {
    if (i > 0) std::cout << ", ";
    std::cout << dp.successors_[i];
  }
  if (dp.successors_.size() > 5) std::cout << "...";
  std::cout << std::endl;

  // Predecessors
  std::cout << "      Predecessors (" << dp.predecessors_.size() << "): ";
  for (size_t i = 0; i < std::min(dp.predecessors_.size(), size_t{5}); ++i) {
    if (i > 0) std::cout << ", ";
    std::cout << dp.predecessors_[i];
  }
  if (dp.predecessors_.size() > 5) std::cout << "...";
  std::cout << std::endl;

  // Stop locations
  if (!dp.stopLocations_.empty()) {
    std::cout << "      Stop locations (" << dp.stopLocations_.size()
              << "):" << std::endl;
    for (size_t i = 0; i < std::min(dp.stopLocations_.size(), size_t{3}); ++i) {
      const auto& sl = dp.stopLocations_[i];
      std::cout << "        - Range: " << sl.range_
                << ", Virtual: " << (sl.virtual_ ? "true" : "false")
                << std::endl;
    }
    if (dp.stopLocations_.size() > 3) {
      std::cout << "        ... (" << (dp.stopLocations_.size() - 3) << " more)"
                << std::endl;
    }
  }

  // Speed profiles
  if (!dp.speedProfiles_.empty()) {
    std::cout << "      Speed profiles (" << dp.speedProfiles_.size()
              << "):" << std::endl;
    for (size_t i = 0; i < std::min(dp.speedProfiles_.size(), size_t{3}); ++i) {
      const auto& sp = dp.speedProfiles_[i];
      std::cout << "        - Start: " << sp.start_ << ", End: " << sp.end_
                << ", Min: " << sp.minSpeed_ << ", Max: " << sp.maxSpeed_
                << std::endl;
    }
    if (dp.speedProfiles_.size() > 3) {
      std::cout << "        ... (" << (dp.speedProfiles_.size() - 3) << " more)"
                << std::endl;
    }
  }
}

}  // namespace qlever
