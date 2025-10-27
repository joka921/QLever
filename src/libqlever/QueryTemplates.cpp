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

// This query associates all the drive paths (?dp) with features that can be
// expressed as one or two values (the variables ?c1 and ?c2). The `?type`
// variable denotes the type of feature. For example, if `?type` is `2`, then
// `?c1` is the feature ID of a predecessor of the drive path etc.
const std::string payloadQuerySingleColumn = R"(
PREFIX lbm: <http://www.bmw-carit.de/Foresight/Map/Ontologies/Low/behaviorMap#>
PREFIX geo: <http://www.opengis.net/ont/geosparql#>
SELECT ?dp ?type ?c1 ?c2 WHERE {
# The nested subquery is so that qlever removes unneeded columns (will not be
# necessary in a future version).
SELECT ?dp ?type ?c1 ?c2 WHERE {
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
  UNION {
    BIND (4 AS ?type)
    ?dp lbm:hasStopLoc [ lbm:range ?c1; lbm:virtual ?c2]
  }
}
}
INTERNAL SORT BY ?dp ?type
)";

// For all drive paths, emit all speed profiles. Speed profiles symmetrically
// have four integer values `start, end, minSpeed, maxSpeed`.
const std::string payloadQuerySpeedProfiles = R"(
PREFIX lbm: <http://www.bmw-carit.de/Foresight/Map/Ontologies/Low/behaviorMap#>
SELECT ?dp ?start ?end ?minSpeed ?maxSpeed WHERE {
  ?dp a lbm:DrivePath .
  ?dp lbm:hasSpeed ?speed .
  ?speed lbm:start ?start;
    lbm:end ?end;
    lbm:minSpeed ?minSpeed;
    lbm:maxSpeed ?maxSpeed;
}
INTERNAL SORT BY ?dp ?speed
)";

// Associate all drive paths with their geometries. This query is used to build
// a spatial cached index, s.t. we can efficiently run geospatial queries on the
// `?geom` column.
const std::string geometryQuery = R"(
PREFIX geo: <http://www.opengis.net/ont/geosparql#>
PREFIX lbm: <http://www.bmw-carit.de/Foresight/Map/Ontologies/Low/behaviorMap#>

SELECT * WHERE {
 ?dp lbm:hasGeometry/geo:asWKT ?geom .
 ?dp a lbm:DrivePath
}
)";

// This query assumes, that the `geometryQuery` above has been cached with a
// spatial index on the `?geom` variable under the cache name `geos`. It outputs
// all drive paths around the car position in a radius of 200 metres. The
// Placeholder #coordinates#  has to be replaced by the current car position.
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
                 qlss:maxDistance 200 .
      }
    } INTERNAL SORT BY ?dp
)ab";

const std::string queryCurrentDrivePathsWithExternalValues = R"ab(
PREFIX qlss: <https://qlever.cs.uni-freiburg.de/spatialSearch/>
PREFIX qlet: <https://qlever.cs.uni-freiburg.de/external-values->
PREFIX geo: <http://www.opengis.net/ont/geosparql#>
    SELECT ?dp {
      SERVICE qlet:current-position {
        [] <variables> ?carPos
      }
      SERVICE qlss: {
        _:config qlss:algorithm <experimentalPointPolyline> ;
                 qlss:left ?carPos ;
                 qlss:right ?geom ;
                 <experimentalRightCacheName> "geos" ;
                 qlss:maxDistance 200 .
      }
    } INTERNAL SORT BY ?dp
)ab";

const std::string queryRoadRefToDpWithExternalValues = R"ab(
PREFIX lbm: <http://www.bmw-carit.de/Foresight/Map/Ontologies/Low/behaviorMap#>
PREFIX qlet: <https://qlever.cs.uni-freiburg.de/external-values->
    SELECT ?dp ?added (COUNT(?roadPart) as ?cnt) {
      SERVICE qlet:road-ref-values {
        [] <variables> ?roadPart, ?added
      }
    {SELECT ?roadPart ?dp {
      SERVICE ql:cached-result-with-name-road-ref-to-dp {}
    }}
    } GROUP BY ?dp ?added
)ab";

const std::string queryDpFeaturesFromIdsWithExternalValues = R"ab(
PREFIX lbm: <http://www.bmw-carit.de/Foresight/Map/Ontologies/Low/behaviorMap#>
PREFIX qlet: <https://qlever.cs.uni-freiburg.de/external-values->
SELECT ?dp ?type ?c1 ?c2 WHERE {
  {
    SELECT ?dp {
      SERVICE qlet:dp-ids {
        [] <variables> ?dp
      }
    }
  }
  {
    SELECT ?dp ?type ?c1 ?c2 {
      SERVICE ql:cached-result-with-name-payload {}
    }
  }
}
)ab";

const std::string queryDpSpeedFromIdsWithExternalValues = R"ab(
PREFIX lbm: <http://www.bmw-carit.de/Foresight/Map/Ontologies/Low/behaviorMap#>
PREFIX qlet: <https://qlever.cs.uni-freiburg.de/external-values->
SELECT ?dp ?start ?end ?minSpeed ?maxSpeed WHERE {
  {
    SELECT ?dp {
      SERVICE qlet:dp-ids {
        [] <variables> ?dp
      }
    }
  }
  {
    SELECT ?dp ?start ?end ?minSpeed ?maxSpeed {
      SERVICE ql:cached-result-with-name-speed {}
    }
  }
}
)ab";

// This query gets a set of road refs that are each either added or deleted
// to/from the MPP since the last step, via a `VALUES (?roadPart ?added)`, and
// returns all the drive paths that are on the road parts. For each drive path
// we get the information, how many road parts that have been added/deleted
// contain that drive path, s.t. we can maintain a diff of the drive paths from
// the diff of road segments.

const std::string queryDpToRoadRef = R"ab(
PREFIX lbm: <http://www.bmw-carit.de/Foresight/Map/Ontologies/Low/behaviorMap#>
    SELECT ?roadPart ?dp {
     ?roadPart lbm:hasDrivePaths ?dp
    } INTERNAL SORT BY ?roadPart
)ab";

const std::string queryTemplateForRoadRefToDp = R"ab(
PREFIX lbm: <http://www.bmw-carit.de/Foresight/Map/Ontologies/Low/behaviorMap#>
    SELECT ?dp ?added (COUNT(?roadPart) as ?cnt) {
      #values#
    {SELECT ?roadPart ?dp {
      SERVICE ql:cached-result-with-name-road-ref-to-dp {}
    }}
    } GROUP BY ?dp ?added
)ab";

// Given a set of drive paths (inside a VALUES ?dp clause), obtain all the
// features, by joining with the payload query (which is assumed as cached under
// the name `payload`.
const std::string queryTemplateForDpFeaturesFromIds = R"ab(
PREFIX lbm: <http://www.bmw-carit.de/Foresight/Map/Ontologies/Low/behaviorMap#>
SELECT ?dp ?type ?c1 ?c2 WHERE {
  {
    SELECT ?dp {
      #values#
    }
  }
  {
    SELECT ?dp ?type ?c1 ?c2 {
      SERVICE ql:cached-result-with-name-payload {}
    }
  }
}
)ab";

// Same as above but for the speedprofiles.
const std::string queryTemplateForDpSpeedFromIds = R"ab(
PREFIX lbm: <http://www.bmw-carit.de/Foresight/Map/Ontologies/Low/behaviorMap#>
SELECT ?dp ?start ?end ?minSpeed ?maxSpeed WHERE {
  {
    SELECT ?dp {
      #values#
    }
  }
  {
    SELECT ?dp ?start ?end ?minSpeed ?maxSpeed {
      SERVICE ql:cached-result-with-name-speed {}
    }
  }
}
)ab";

std::string getCurrentDrivePathQuery(std::string_view point) {
  return absl::StrReplaceAll(queryTemplateForCurrentDrivePaths,
                             {{std::string_view{"#coordinates#"}, point}});
}

std::string mppIdToIri(uint64_t id) {
  return absl::StrCat(
      "<http://www.bmw-carit.de/Foresight/Map/Ontologies/Low/"
      "behaviorMap#roadPartId_",
      id, ">");
}

std::string generateValuesClauseWithAdded(const std::vector<uint64_t>& mppIds,
                                          bool added) {
  std::vector<std::string> entries;
  entries.reserve(mppIds.size());
  std::string addedStr = added ? "true" : "false";
  for (uint64_t id : mppIds) {
    entries.push_back(absl::StrCat("(", mppIdToIri(id), " ", addedStr, ")"));
  }
  return absl::StrCat("VALUES (?roadPart ?added) { ",
                      absl::StrJoin(entries, " "), " }");
}

std::string getRoadRefToDpQuery(const std::vector<uint64_t>& mppIds,
                                bool added) {
  std::string valuesClause = generateValuesClauseWithAdded(mppIds, added);
  return absl::StrReplaceAll(queryTemplateForRoadRefToDp,
                             {{std::string_view{"#values#"}, valuesClause}});
}

}  // namespace qlever
