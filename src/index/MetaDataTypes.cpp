// Copyright 2015, University of Freiburg,
// Chair of Algorithms and Data Structures.
// Author: Björn Buchhold (buchhold@informatik.uni-freiburg.de)

#include "./MetaDataTypes.h"
#include <stdio.h>
#include <algorithm>
#include <cmath>
#include "../util/ReadableNumberFact.h"
#include "./MetaDataHandler.h"
#include "./IndexBuilderTypes.h"

const FullRelationMetaData FullRelationMetaData::empty({ID_NO_VALUE, Datatype::String}, -1, -1, 1.0, 1.0,
                                                       false, false, std::nullopt, std::nullopt);
// _____________________________________________________________________________
FullRelationMetaData::FullRelationMetaData()
    : _relId{0, Datatype::String}, _startFullIndex(0), _typeMultAndNofElements(0) {}

// _____________________________________________________________________________
FullRelationMetaData::FullRelationMetaData(IdWithDatatype relId, off_t startFullIndex,
                                           size_t nofElements, double col1Mult,
                                           double col2Mult, bool isFunctional,
                                           bool hasBlocks, std::optional<Datatype> firstColumnDatatype,
                                           std::optional<Datatype> secondColumnDatatype)
    : _relId(relId),
      _startFullIndex(startFullIndex),
      _firstColumnUniqueDatatype(firstColumnDatatype),
       _secondColumnUniqueDatatype(secondColumnDatatype),
      _typeMultAndNofElements(nofElements)
  {
  assert(col1Mult >= 1);
  assert(col2Mult >= 1);
  double c1ml = log2(col1Mult);
  double c2ml = log2(col2Mult);
  uint8_t c1 = c1ml > 255 ? uint8_t(255) : uint8_t(c1ml);
  uint8_t c2 = c2ml > 255 ? uint8_t(255) : uint8_t(c2ml);
  setIsFunctional(isFunctional);
  setHasBlocks(hasBlocks);
  setCol1LogMultiplicity(c1);
  setCol2LogMultiplicity(c2);
}

// _____________________________________________________________________________
size_t FullRelationMetaData::getNofBytesForFulltextIndex() const {
  return getNofElements() * 2 * sizeof(Id);
}

// _____________________________________________________________________________
bool FullRelationMetaData::isFunctional() const {
  return (_typeMultAndNofElements & IS_FUNCTIONAL_MASK) != 0;
}

// _____________________________________________________________________________
bool FullRelationMetaData::hasBlocks() const {
  return (_typeMultAndNofElements & HAS_BLOCKS_MASK) != 0;
}

// _____________________________________________________________________________
size_t FullRelationMetaData::getNofElements() const {
  return size_t(_typeMultAndNofElements & NOF_ELEMENTS_MASK);
}

// _____________________________________________________________________________
void FullRelationMetaData::setIsFunctional(bool isFunctional) {
  if (isFunctional) {
    _typeMultAndNofElements |= IS_FUNCTIONAL_MASK;
  } else {
    _typeMultAndNofElements &= ~IS_FUNCTIONAL_MASK;
  }
}

// _____________________________________________________________________________
void FullRelationMetaData::setHasBlocks(bool hasBlocks) {
  if (hasBlocks) {
    _typeMultAndNofElements |= HAS_BLOCKS_MASK;
  } else {
    _typeMultAndNofElements &= ~HAS_BLOCKS_MASK;
  }
}

// _____________________________________________________________________________
void FullRelationMetaData::setCol1LogMultiplicity(uint8_t mult) {
  // Reset a current value
  _typeMultAndNofElements &= 0xFF00FFFFFFFFFFFF;
  // Set the new one
  _typeMultAndNofElements |= (uint64_t(mult) << 48);
}

// _____________________________________________________________________________
void FullRelationMetaData::setCol2LogMultiplicity(uint8_t mult) {
  // Reset a current value
  _typeMultAndNofElements &= 0xFFFF00FFFFFFFFFF;
  // Set the new one
  _typeMultAndNofElements |= (uint64_t(mult) << 40);
}

// _____________________________________________________________________________
uint8_t FullRelationMetaData::getCol1LogMultiplicity() const {
  return uint8_t((_typeMultAndNofElements & 0x00FF000000000000) >> 48);
}

// _____________________________________________________________________________
uint8_t FullRelationMetaData::getCol2LogMultiplicity() const {
  return uint8_t((_typeMultAndNofElements & 0x0000FF0000000000) >> 40);
}


// _____________________________________________________________________________
FullRelationMetaData& FullRelationMetaData::createFromByteBuffer(
    unsigned char* buffer) {
  _relId = *reinterpret_cast<IdWithDatatype*>(buffer);
  _startFullIndex = *reinterpret_cast<off_t*>(buffer + sizeof(_relId));
  _typeMultAndNofElements =
      *reinterpret_cast<uint64_t*>(buffer + sizeof(_relId) + sizeof(_typeMultAndNofElements));

  auto offsetPtr = buffer + sizeof(_relId) + sizeof(_startFullIndex) + sizeof(_typeMultAndNofElements);

   _firstColumnUniqueDatatype = (*reinterpret_cast<decltype(_firstColumnUniqueDatatype)*>(offsetPtr));
   offsetPtr += sizeof(_firstColumnUniqueDatatype);
   _secondColumnUniqueDatatype = (*reinterpret_cast<decltype(_secondColumnUniqueDatatype)*>(offsetPtr));
  return *this;
}

// _____________________________________________________________________________
BlockBasedRelationMetaData& BlockBasedRelationMetaData::createFromByteBuffer(
    unsigned char* buffer) {
  _startRhs = *reinterpret_cast<off_t*>(buffer);
  buffer += sizeof(_startRhs);
  _startRhsTypes = *reinterpret_cast<off_t*>(buffer);
  buffer += sizeof(_startRhsTypes);
  _offsetAfter = *reinterpret_cast<off_t*>(buffer);
  buffer += sizeof(_offsetAfter);
  size_t nofBlocks = *reinterpret_cast<size_t*>(buffer);
  buffer += sizeof(nofBlocks);
  _blocks.resize(nofBlocks);
  memcpy(_blocks.data(),buffer, nofBlocks * sizeof(BlockMetaData));

  return *this;
}

// _____________________________________________________________________________
size_t FullRelationMetaData::bytesRequired() const {
  return sizeof(_relId) + sizeof(_startFullIndex) +
         sizeof(_typeMultAndNofElements) +  sizeof(_firstColumnUniqueDatatype)
       + sizeof(_secondColumnUniqueDatatype);


}

off_t FullRelationMetaData::getStartOfTypeData() const {
  return _startFullIndex + 2 * sizeof(Id) * getNofElements();

}

// _____________________________________________________________________________
off_t FullRelationMetaData::getStartOfLhs() const {
  AD_CHECK(hasBlocks());
  return getStartOfTypeData() + getSizeOfTypeData();
}

// ________________________________________________________________________
off_t FullRelationMetaData::getSizeOfTypeData() const {
  off_t res = 0;
  if (!_firstColumnUniqueDatatype) {
    res += getNofElements() * sizeof(Datatype);
  }
  if (!_secondColumnUniqueDatatype) {
    res += getNofElements() * sizeof(Datatype);
  }
  return res;
}

// _____________________________________________________________________________
size_t BlockBasedRelationMetaData::bytesRequired() const {
  return sizeof(_startRhs) + sizeof(_offsetAfter) + sizeof(size_t) +
         _blocks.size() * sizeof(BlockMetaData);
}

// _____________________________________________________________________________
BlockBasedRelationMetaData::BlockBasedRelationMetaData()
    : _startRhs(0), _offsetAfter(0), _blocks() {}

// _____________________________________________________________________________
BlockBasedRelationMetaData::BlockBasedRelationMetaData(
    off_t startRhs, off_t offsetAfter, const vector<BlockMetaData>& blocks)
    : _startRhs(startRhs), _offsetAfter(offsetAfter), _blocks(blocks) {}
