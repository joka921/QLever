// Copyright 2015, University of Freiburg,
// Chair of Algorithms and Data Structures.
// Author: Björn Buchhold (buchhold@informatik.uni-freiburg.de)
#pragma once

#include <array>
#include <utility>
#include <vector>

#include "../global/Id.h"
#include "../util/File.h"
#include "../util/HashMap.h"
#include "./MetaDataTypes.h"
#include <stdio.h>
#include <algorithm>
#include <cmath>
#include "../util/ReadableNumberFact.h"

using std::array;
using std::pair;
using std::vector;

// Check index_layout.md for explanations (expected comments).
// Removed comments here so that not two places had to be kept up-to-date.


// ______________________________________________________________________
template <class MapType>
class IndexMetaDataTemplated {
 public:
  IndexMetaDataTemplated();

  void add(const FullRelationMetaData& rmd,
           const BlockBasedRelationMetaData& bRmd);

  off_t getOffsetAfter() const;

  const RelationMetaData getRmd(Id relId) const;

  void createFromByteBuffer(unsigned char* buf);

  bool relationExists(Id relId) const;

  string statistics() const;

  size_t getNofTriples() const { return _nofTriples; }

  void setName(const string& name) { _name = name; }

  const string& getName() const { return _name; }

  size_t getNofDistinctC1() const;

 private:
  off_t _offsetAfter;
  size_t _nofTriples;

  string _name;

  MapType _data;
  ad_utility::HashMap<Id, BlockBasedRelationMetaData> _blockData;

  
  // this way all instantations will be friends with each other, but this should
  // not be an issue.
  template<class U>
  friend ad_utility::File& operator<<(ad_utility::File& f,
                                      const IndexMetaDataTemplated<U>& rmd);

  size_t getNofBlocksForRelation(const Id relId) const;

  size_t getTotalBytesForRelation(const FullRelationMetaData& frmd) const;
};

using IndexMetaData =IndexMetaDataTemplated<ad_utility::HashMap<Id, FullRelationMetaData>>;
