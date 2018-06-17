// Copyright 2015, University of Freiburg,
// Chair of Algorithms and Data Structures.
// Author: Björn Buchhold (buchhold@informatik.uni-freiburg.de)

#include "./IndexMetaData.h"
#include "./MetaDataHandler.h"
#include <stdio.h>
#include <algorithm>
#include <cmath>
#include "../util/ReadableNumberFact.h"

// _____________________________________________________________________________
IndexMetaData::IndexMetaData() : _offsetAfter(0), _nofTriples(0), _name() {}

// _____________________________________________________________________________
void IndexMetaData::add(const FullRelationMetaData& rmd,
                        const BlockBasedRelationMetaData& bRmd) {
  _data[rmd._relId] = rmd;
  off_t afterExpected =
      rmd.hasBlocks() ? bRmd._offsetAfter
                      : static_cast<off_t>(rmd._startFullIndex +
                                           rmd.getNofBytesForFulltextIndex());
  if (rmd.hasBlocks()) {
    _blockData[rmd._relId] = bRmd;
  }
  if (afterExpected > _offsetAfter) {
    _offsetAfter = afterExpected;
  }
}

// _____________________________________________________________________________
off_t IndexMetaData::getOffsetAfter() const { return _offsetAfter; }

// _____________________________________________________________________________
void IndexMetaData::createFromByteBuffer(unsigned char* buf) {
  size_t nameLength = *reinterpret_cast<size_t*>(buf);
  size_t nofBytesDone = sizeof(size_t);
  _name.assign(reinterpret_cast<char*>(buf + nofBytesDone), nameLength);
  nofBytesDone += nameLength;
  size_t nofRelations = *reinterpret_cast<size_t*>(buf + nofBytesDone);
  nofBytesDone += sizeof(size_t);
  _offsetAfter = *reinterpret_cast<off_t*>(buf + nofBytesDone);
  nofBytesDone += sizeof(off_t);
  _nofTriples = 0;
  for (size_t i = 0; i < nofRelations; ++i) {
    FullRelationMetaData rmd;
    rmd.createFromByteBuffer(buf + nofBytesDone);
    _nofTriples += rmd.getNofElements();
    nofBytesDone += rmd.bytesRequired();
    if (rmd.hasBlocks()) {
      BlockBasedRelationMetaData bRmd;
      bRmd.createFromByteBuffer(buf + nofBytesDone);
      nofBytesDone += bRmd.bytesRequired();
      add(rmd, bRmd);
    } else {
      add(rmd, BlockBasedRelationMetaData());
    }
  }
}

// _____________________________________________________________________________
const RelationMetaData IndexMetaData::getRmd(Id relId) const {
  auto it = _data.find(relId);
  AD_CHECK(it != _data.end());
  RelationMetaData ret(it->second);
  if (it->second.hasBlocks()) {
    ret._rmdBlocks = &_blockData.find(it->first)->second;
  }
  return ret;
}

// _____________________________________________________________________________
bool IndexMetaData::relationExists(Id relId) const {
  return _data.count(relId) > 0;
}

// _____________________________________________________________________________
ad_utility::File& operator<<(ad_utility::File& f, const IndexMetaData& imd) {
  size_t nameLength = imd._name.size();
  f.write(&nameLength, sizeof(nameLength));
  f.write(imd._name.data(), nameLength);
  size_t nofElements = imd._data.size();
  f.write(&nofElements, sizeof(nofElements));
  f.write(&imd._offsetAfter, sizeof(imd._offsetAfter));
  for (auto it = imd._data.begin(); it != imd._data.end(); ++it) {
    f << it->second;
    if (it->second.hasBlocks()) {
      auto itt = imd._blockData.find(it->second._relId);
      AD_CHECK(itt != imd._blockData.end());
      f << itt->second;
    }
  }
  return f;
}

// _____________________________________________________________________________
string IndexMetaData::statistics() const {
  std::ostringstream os;
  std::locale loc;
  ad_utility::ReadableNumberFacet facet(1);
  std::locale locWithNumberGrouping(loc, &facet);
  os.imbue(locWithNumberGrouping);
  os << '\n';
  os << "-------------------------------------------------------------------\n";
  os << "----------------------------------\n";
  os << "Index Statistics:\n";
  os << "----------------------------------\n\n";
  os << "# Relations (_data.size()): " << _data.size() << '\n';
  os << "# Block Data: " << _blockData.size() << '\n';
  size_t totalElements = 0;
  size_t totalBytes = 0;
  size_t totalBlocks = 0;
  for (auto it = _data.begin(); it != _data.end(); ++it) {
    totalElements += it->second.getNofElements();
    totalBytes += getTotalBytesForRelation(it->second);
    totalBlocks += getNofBlocksForRelation(it->first);
  }
  size_t totalPairIndexBytes = totalElements * 2 * sizeof(Id);
  os << "# Elements:  " << totalElements << '\n';
  os << "# Blocks:    " << totalBlocks << "\n\n";
  os << "Theoretical size of Id triples: " << totalElements * 3 * sizeof(Id)
     << " bytes \n";
  os << "Size of pair index:             " << totalPairIndexBytes
     << " bytes \n";
  os << "Total Size:                     " << totalBytes << " bytes \n";
  os << "-------------------------------------------------------------------\n";
  return os.str();
}

// _____________________________________________________________________________
size_t IndexMetaData::getNofBlocksForRelation(const Id id) const {
  auto it = _blockData.find(id);
  if (it != _blockData.end()) {
    return it->second._blocks.size();
  } else {
    return 0;
  }
}

// _____________________________________________________________________________
size_t IndexMetaData::getTotalBytesForRelation(
    const FullRelationMetaData& frmd) const {
  auto it = _blockData.find(frmd._relId);
  if (it != _blockData.end()) {
    return static_cast<size_t>(it->second._offsetAfter - frmd._startFullIndex);
  } else {
    return frmd.getNofBytesForFulltextIndex();
  }
}

// ______________________________________________
size_t IndexMetaData::getNofDistinctC1() const {
    return _data.size();
}

