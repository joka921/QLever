// Copyright 2015, University of Freiburg,
// Chair of Algorithms and Data Structures.
// Author: Björn Buchhold (buchhold@informatik.uni-freiburg.de)

#include "./IndexMetaData.h"
#include "./MetaDataHandler.h"

// ____________________________________________________________________________
template <class MapType>
ad_utility::File& operator<<(ad_utility::File& f, const IndexMetaDataTemplated<MapType>& imd);

// Implementations are here because everything is templated
// _____________________________________________________________________________
template <class MapType>
IndexMetaDataTemplated<MapType>::IndexMetaDataTemplated(size_t numEls) : _offsetAfter(0), _nofTriples(0), _name(), _data(numEls) {}

// _____________________________________________________________________________
template <class MapType>
void IndexMetaDataTemplated<MapType>::add(const FullRelationMetaData& rmd,
                        const BlockBasedRelationMetaData& bRmd) {
  //_data[rmd._relId] = rmd;
  _data.set(rmd._relId, rmd);
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
template <class MapType>
off_t IndexMetaDataTemplated<MapType>::getOffsetAfter() const { return _offsetAfter; }

// _____________________________________________________________________________
template <class MapType>
void IndexMetaDataTemplated<MapType>::createFromByteBuffer(unsigned char* buf) {
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
template <class MapType>
const RelationMetaData IndexMetaDataTemplated<MapType>::getRmd(Id relId) const {
  /* ___________________________________
  auto it = _data.find(relId);
  AD_CHECK(it != _data.end());
  RelationMetaData ret(it->second);
  if (it->second.hasBlocks()) {
    ret._rmdBlocks = &_blockData.find(it->first)->second;
  }
  */
  // explicit typing since I'm still restructuring
  const FullRelationMetaData& full = _data.getAsserted(relId);
  RelationMetaData ret(full);
  if (full.hasBlocks()) {
    ret._rmdBlocks = &_blockData.find(relId)->second;
  }
  return ret;
}

// _____________________________________________________________________________
template <class MapType>
bool IndexMetaDataTemplated<MapType>::relationExists(Id relId) const {
  return _data.count(relId) > 0;
}

// _____________________________________________________________________________
template <class MapType>
ad_utility::File& operator<<(ad_utility::File& f, const IndexMetaDataTemplated<MapType>& imd) {
  size_t nameLength = imd._name.size();
  f.write(&nameLength, sizeof(nameLength));
  f.write(imd._name.data(), nameLength);
  size_t nofElements = imd._data.size();
  f.write(&nofElements, sizeof(nofElements));
  f.write(&imd._offsetAfter, sizeof(imd._offsetAfter));
  for (auto it = imd._data.cbegin(); it != imd._data.cend(); ++it) {
    const auto el = *it;
    f << el.second;
    if (el.second.hasBlocks()) {
      auto itt = imd._blockData.find(el.second._relId);
      AD_CHECK(itt != imd._blockData.end());
      f << itt->second;
    }
  }
  return f;
}

// _____________________________________________________________________________
template <class MapType>
string IndexMetaDataTemplated<MapType>::statistics() const {
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
  for (auto it = _data.cbegin(); it != _data.cend(); ++it) {
    auto el = *it;
    totalElements += el.second.getNofElements();
    totalBytes += getTotalBytesForRelation(el.second);
    totalBlocks += getNofBlocksForRelation(el.first);
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
template <class MapType>
size_t IndexMetaDataTemplated<MapType>::getNofBlocksForRelation(const Id id) const {
  auto it = _blockData.find(id);
  if (it != _blockData.end()) {
    return it->second._blocks.size();
  } else {
    return 0;
  }
}

// _____________________________________________________________________________
template <class MapType>
size_t IndexMetaDataTemplated<MapType>::getTotalBytesForRelation(
    const FullRelationMetaData& frmd) const {
  auto it = _blockData.find(frmd._relId);
  if (it != _blockData.end()) {
    return static_cast<size_t>(it->second._offsetAfter - frmd._startFullIndex);
  } else {
    return frmd.getNofBytesForFulltextIndex();
  }
}

// ______________________________________________
template <class MapType>
size_t IndexMetaDataTemplated<MapType>::getNofDistinctC1() const {
    return _data.size();
}

// explicit instatiations
template class IndexMetaDataTemplated<MetaDataWrapperExtVec>;
template ad_utility::File& operator<<(ad_utility::File& f, const IndexMetaDataTemplated<MetaDataWrapperExtVec>& imd);
