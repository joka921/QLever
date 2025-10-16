// Copyright 2025 The QLever Authors, in particular:
//
// 2025 Johannes Kalmbach <kalmbach@cs.uni-freiburg.de>, UFR
//
// UFR = University of Freiburg, Chair of Algorithms and Data Structures

#include "libqlever/QueryTemplates.h"

#include <absl/strings/str_cat.h>
#include <absl/strings/str_join.h>
#include <absl/strings/str_replace.h>

namespace qlever {

const std::string payloadQuerySingleColumn = R"(
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

const std::string payloadQuerySpeedProfiles = R"(
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

const std::string geometryQuery = R"(
PREFIX geo: <http://www.opengis.net/ont/geosparql#>
PREFIX lbm: <http://www.bmw-carit.de/Foresight/Map/Ontologies/Low/behaviorMap#>

SELECT * WHERE {
 ?dp lbm:hasGeometry/geo:asWKT ?geom .
 ?dp a lbm:DrivePath
}
)";

const std::string queryTemplateForDrivePaths = R"ab(
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

const std::string queryTemplateForCurrentDrivePaths = R"ab(
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

const std::string queryTemplateForFeatures = R"ab(
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

const std::string queryTemplateForMppFeatures = R"ab(
PREFIX lbm: <http://www.bmw-carit.de/Foresight/Map/Ontologies/Low/behaviorMap#>
SELECT ?dp ?type ?c1 WHERE {
  {
    SELECT ?dp {
      #values#
    }
  }
  {
    SELECT ?dp ?type ?c1 {
      SERVICE ql:cached-result-with-name-payload {}
    }
  }
}
)ab";

std::string getQueryForPoint(std::string_view point) {
  return absl::StrReplaceAll(queryTemplateForDrivePaths,
                             {{std::string_view{"#coordinates#"}, point}});
}

std::string getCurrentDrivePathQuery(std::string_view point) {
  return absl::StrReplaceAll(queryTemplateForCurrentDrivePaths,
                             {{std::string_view{"#coordinates#"}, point}});
}

std::string mppIdToIri(uint64_t id) { return absl::StrCat("lbm:dp_", id); }

std::string generateValuesClause(const std::vector<uint64_t>& mppIds) {
  std::vector<std::string> iris;
  iris.reserve(mppIds.size());
  for (uint64_t id : mppIds) {
    iris.push_back(mppIdToIri(id));
  }
  return absl::StrCat("VALUES ?dp { ", absl::StrJoin(iris, " "), " }");
}

std::string getMppFeaturesQuery(const std::vector<uint64_t>& mppIds) {
  std::string valuesClause = generateValuesClause(mppIds);
  return absl::StrReplaceAll(queryTemplateForMppFeatures,
                             {{std::string_view{"#values#"}, valuesClause}});
}

}  // namespace qlever
