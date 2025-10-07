// Copyright 2025 The QLever Authors, in particular:
//
// 2025 Johannes Kalmbach <kalmbach@cs.uni-freiburg.de>, UFR
//
// UFR = University of Freiburg, Chair of Algorithms and Data Structures

#include <absl/strings/str_replace.h>

#include <iostream>

#include "engine/ExplicitIdTableOperation.h"
#include "engine/QueryExecutionTree.h"
#include "libqlever/Qlever.h"
#include "util/Exception.h"
#include "util/Timer.h"

void fillInterfaceForSimpleFeatures(const Result& result) {
  const auto& table = result.idTable();
  std::cout << "Filling interface for result with " << table.numColumns()
            << " columns and " << table.numRows() << " rows" << std::endl;
}

static const std::string payloadQuerySingleColumn = R"(
PREFIX lbm: <http://www.bmw-carit.de/Foresight/Map/Ontologies/Low/behaviorMap#>
SELECT ?dp ?type ?c1 WHERE {
  ?dp a lbm:DrivePath .
  {
    BIND (0 AS ?type)
    ?dp lbm:hasSucc ?c1 .
  }
  UNION {
    BIND (1 AS ?type)
    ?dp lbm:hasPred ?c1 .
  }
  UNION {
    BIND (2 AS ?type)
    ?dp lbm:featIdInt ?c1 .
  }
  UNION {
    BIND (3 AS ?type)
    ?dp lbm:hasShapePoints ?c1 .
  }
}
INTERNAL SORT BY ?dp ?type
)";

static const std::string geometryQuery = R"(
PREFIX geo: <http://www.opengis.net/ont/geosparql#>
PREFIX lbm: <http://www.bmw-carit.de/Foresight/Map/Ontologies/Low/behaviorMap#>

SELECT * WHERE {
 ?dp geo:hasGeometry/geo:asWKT ?geom .
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
  for (size_t i = 201; i < 456; ++i) {
    // We currently have all the tiles, except for 444 in an adjacent order.
    if (i == 444) continue;
    res.push_back(qlever::InputFileSpecification{
        absl::StrCat("ttl/545555", i, ".ttl"), qlever::Filetype::Turtle});
  }
  return res;
};

// Add additional points here...
std::vector<std::string> queryPoints = {"11.729869 48.398452"};

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
    std::cout << "\x1b[1mExecuting test query"
              << "\x1b[0m" << std::endl;
    std::string queryResult;
    ad_utility::Timer timer{ad_utility::Timer::Started};
    try {
      qlever.queryAndPinResultWithName({"currentDrivepaths", std::nullopt},
                                       getCurrentDrivePathQuery(point));
      auto plan = qlever.parseAndPlanQuery(queryTemplateForFeatures);
      auto result = qlever.getResult(plan, false);
      fillInterfaceForSimpleFeatures(*result);
    } catch (const std::exception& e) {
      std::cerr << "Executing the query failed: " << e.what() << std::endl;
      return 1;
    }
    std::cout.imbue(std::locale(""));
    std::cout << "Query executed in " << timer.msecs().count() << "ms"
              << std::endl;
    std::cout << std::endl;

    // Show result.
    std::cout << "\x1b[1mPrefix of result string is:\x1b[0m" << std::endl;
    std::cout << std::string_view{queryResult}.substr(0, 1000) << std::endl;
    std::cout << std::endl;
  }
}
