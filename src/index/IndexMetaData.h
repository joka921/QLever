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

using std::array;
using std::pair;
using std::vector;

// Check index_layout.md for explanations (expected comments).
// Removed comments here so that not two places had to be kept up-to-date.


// ______________________________________________________________________
class IndexMetaData {
 public:
  IndexMetaData();

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

  ad_utility::HashMap<Id, FullRelationMetaData> _data;
  ad_utility::HashMap<Id, BlockBasedRelationMetaData> _blockData;

  friend ad_utility::File& operator<<(ad_utility::File& f,
                                      const IndexMetaData& rmd);

  size_t getNofBlocksForRelation(const Id relId) const;

  size_t getTotalBytesForRelation(const FullRelationMetaData& frmd) const;
};

ad_utility::File& operator<<(ad_utility::File& f, const IndexMetaData& imd);
