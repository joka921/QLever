// Copyright 2025 The QLever Authors, in particular:
//
// 2025 Johannes Kalmbach <kalmbach@cs.uni-freiburg.de>, UFR
//
// UFR = University of Freiburg, Chair of Algorithms and Data Structures

#include <absl/strings/str_replace.h>

#include <iostream>

#include "./SimulationData.h"
#include "engine/ExplicitIdTableOperation.h"
#include "engine/ExportQueryExecutionTrees.h"
#include "engine/QueryExecutionTree.h"
#include "engine/VariableToColumnMap.h"
#include "libqlever/Qlever.h"
#include "util/Exception.h"
#include "util/Timer.h"

struct DrivePath {
  int64_t id_;
  std::string shapePoints_;
  std::vector<int64_t> successors_;
  std::vector<int64_t> predecessors_;
};

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

static const std::string payloadQuerySingleColumn = R"(
PREFIX lbm: <http://www.bmw-carit.de/Foresight/Map/Ontologies/Low/behaviorMap#>
PREFIX geo: <http://www.opengis.net/ont/geosparql#>
SELECT ?dp ?type ?c1 WHERE {
  ?dp a lbm:DrivePath .
  {
    BIND (0 AS ?type)
    ?dp lbm:hasSucc/lbm:featIdInt ?c1 .
  }
  UNION {
    BIND (1 AS ?type)
    ?dp lbm:hasPred/lbm:featIdInt ?c1 .
  }
  UNION {
    BIND (2 AS ?type)
    ?dp lbm:featIdInt ?c1 .
  }
  UNION {
    BIND (3 AS ?type)
    ?dp lbm:hasGeometry/geo:asWKT ?c1 .
  }
}
INTERNAL SORT BY ?dp ?type
)";

static const std::string payloadQuerySpeedProfiles = R"(
PREFIX lbm: <http://www.bmw-carit.de/Foresight/Map/Ontologies/Low/behaviorMap#>
SELECT ?dp ?start ?end ?minSpeed ?maxSpeed ?type WHERE {
  ?dp a lbm:DrivePath .
  ?dp lbm:hasSpeed ?speed .
  ?speed lbm:start ?start;
    lbm:end ?end;
    lbm:minSpeed ?minSpeed;
    lbm:maxSpeed ?maxSpeed;
}

)";

static const std::string geometryQuery = R"(
PREFIX geo: <http://www.opengis.net/ont/geosparql#>
PREFIX lbm: <http://www.bmw-carit.de/Foresight/Map/Ontologies/Low/behaviorMap#>

SELECT * WHERE {
 ?dp lbm:hasGeometry/geo:asWKT ?geom .
 ?dp a lbm:DrivePath
}
)";

static const std::string queryTemplateForDrivePaths = R"ab(
PREFIX qlss: <https://qlever.cs.uni-freiburg.de/spatialSearch/>
PREFIX geo: <http://www.opengis.net/ont/geosparql#>
SELECT ?dp ?type ?c1 WHERE {
  {
    SELECT ?dp {
      BIND ("POINT(#coordinates# )"^^geo:wktLiteral AS ?carPos)
      SERVICE qlss: {
        _:config qlss:algorithm <experimentalPointPolyline> ;
                 qlss:left ?carPos ;
                 qlss:right ?geom ;
                 <experimentalRightCacheName> "geos" ;
                 qlss:maxDistance 600 .
      }
    }
  }
  {
    SELECT ?dp ?type ?c1 {
      SERVICE ql:cached-result-with-name-payload {}
    }
  }
}
)ab";

static const std::string queryTemplateForCurrentDrivePaths = R"ab(
PREFIX qlss: <https://qlever.cs.uni-freiburg.de/spatialSearch/>
PREFIX geo: <http://www.opengis.net/ont/geosparql#>
    SELECT ?dp {
      BIND ("POINT(#coordinates# )"^^geo:wktLiteral AS ?carPos)
      SERVICE qlss: {
        _:config qlss:algorithm <experimentalPointPolyline> ;
                 qlss:left ?carPos ;
                 qlss:right ?geom ;
                 <experimentalRightCacheName> "geos" ;
                 qlss:maxDistance 600 .
      }
    } INTERNAL SORT BY ?dp
)ab";

static const std::string queryTemplateForFeatures = R"ab(
PREFIX qlss: <https://qlever.cs.uni-freiburg.de/spatialSearch/>
PREFIX geo: <http://www.opengis.net/ont/geosparql#>
SELECT ?dp ?type ?c1 WHERE {
  {
    SELECT ?dp {
      SERVICE ql:cached-result-with-name-currentDrivepaths {}
    }
  }
  {
    SELECT ?dp ?type ?c1 {
      SERVICE ql:cached-result-with-name-payload {}
    }
  }
}
)ab";

static const auto filenames = []() {
  std::vector<qlever::InputFileSpecification> res;
  res.push_back(qlever::InputFileSpecification{
      "ttl33588354_v32/full_33588354_v32.ttl", qlever::Filetype::Turtle});
  /*
  for (size_t i = 201; i < 456; ++i) {
    // We currently have all the tiles, except for 444 in an adjacent order.
    if (i == 444) continue;
    res.push_back(qlever::InputFileSpecification{
        absl::StrCat("ttl/545555", i, ".ttl"), qlever::Filetype::Turtle});
  }
  */
  return res;
};

// Add additional points here...
std::vector<std::string> queryPoints = []() {
  std::vector<std::string> res;
  for (const auto& [mpp, coords] : SIM_DATA | ql::views::drop(10)) {
    res.push_back(absl::StrCat(coords.longitude, " ", coords.latitude));
  }
  return res;
}();

std::string getQueryForPoint(std::string_view point) {
  return absl::StrReplaceAll(queryTemplateForDrivePaths,
                             {{std::string_view{"#coordinates#"}, point}});
}

std::string getCurrentDrivePathQuery(std::string_view point) {
  return absl::StrReplaceAll(queryTemplateForCurrentDrivePaths,
                             {{std::string_view{"#coordinates#"}, point}});
}

int main() {
  // Parse command line arguments.
  std::string indexBasename = "demo-v1";

  // Build index for the given input file and write index files to disk.
  std::cout << "\x1b[1mBuilding index"
            << " with basename \"" << indexBasename << "\x1b[0m" << std::endl;
  qlever::IndexBuilderConfig config;
  config.inputFiles_ = filenames();
  config.baseName_ = indexBasename;
  config.noPatterns_ = true;
  config.onlyPsoAndPos_ = true;
  try {
    qlever::Qlever::buildIndex(config);
  } catch (const std::exception& e) {
    std::cerr << "Building the index failed: " << e.what() << std::endl;
    return 1;
  }
  std::cout << std::endl;

  // Load index.
  std::cout << "\x1b[1mLoading index with basename \"" << indexBasename
            << "\"\x1b[0m" << std::endl;
  qlever::EngineConfig engineConfig{config};
  qlever::Qlever qlever{engineConfig};
  std::cout << std::endl;

  std::cout << "pinning the geometries" << std::endl;
  qlever.queryAndPinResultWithName({"geos", Variable{"?geom"}}, geometryQuery);

  std::cout << "pinning the payload" << std::endl;
  qlever.queryAndPinResultWithName({"payload", std::nullopt},
                                   payloadQuerySingleColumn);

  for (const auto& point : queryPoints) {
    // Execute query.
    std::cout << "\x1b[1mExecuting test query for point " << point << "\x1b[0m"
              << std::endl;
    std::string queryResult;
    ad_utility::Timer timer{ad_utility::Timer::Started};
    try {
      qlever.queryAndPinResultWithName({"currentDrivepaths", std::nullopt},
                                       getCurrentDrivePathQuery(point));
      auto plan = qlever.parseAndPlanQuery(queryTemplateForFeatures);
      qlever.clearCache();
      auto& [qet, qec, parsedQuery] = plan;
      auto result = qlever.getResult(plan, false);
      auto drivePaths = fillInterfaceForSimpleFeatures(
          *result, qec->getIndex(), qet->getVariableColumns());
      std::cout << "Found " << drivePaths.size() << " drive paths in "
                << timer.msecs().count() << "ms" << std::endl;
      for (const auto& dp : drivePaths | ql::views::take(0)) {
        std::cout << "Drive path " << dp.id_ << ":" << std::endl;
        std::cout << "  Shape points: "
                  << std::string_view{dp.shapePoints_}.substr(0, 100)
                  << std::endl;
        std::cout << "  Successors (" << dp.successors_.size() << "): ";
        for (size_t i = 0; i < std::min(dp.successors_.size(), size_t{5});
             ++i) {
          if (i > 0) std::cout << ", ";
          std::cout << dp.successors_[i];
        }
        if (dp.successors_.size() > 5) std::cout << "...";
        std::cout << std::endl;
        std::cout << "  Predecessors (" << dp.predecessors_.size() << "): ";
        for (size_t i = 0; i < std::min(dp.predecessors_.size(), size_t{5});
             ++i) {
          if (i > 0) std::cout << ", ";
          std::cout << dp.predecessors_[i];
        }
        if (dp.predecessors_.size() > 5) std::cout << "...";
        std::cout << std::endl;
      }
    } catch (const std::exception& e) {
      std::cerr << "Executing the query failed: " << e.what() << std::endl;
      return 1;
    }
    std::cout.imbue(std::locale(""));
    std::cout << std::endl;
  }
}
