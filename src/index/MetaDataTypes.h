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
#include "./IndexBuilderTypes.h"

using std::array;
using std::pair;
using std::vector;

// Check index_layout.md for explanations (expected comments).
// Removed comments here so that not two places had to be kept up-to-date.
//
// (joka921): After restructuring this file only contains the basic single
// building blocks of the metaData for one relation.

static const uint64_t IS_FUNCTIONAL_MASK = 0x0100000000000000;
static const uint64_t HAS_BLOCKS_MASK = 0x0200000000000000;
static const uint64_t NOF_ELEMENTS_MASK = 0x000000FFFFFFFFFF;
static const uint64_t MAX_NOF_ELEMENTS = NOF_ELEMENTS_MASK;

struct DatatypeInfo {
  DatatypeInfo(size_t threshold, const std::string& basename):
    ids_{threshold, basename + "-ids"}, types0_{threshold, basename + "-types0"}, types1_{threshold, basename + "-types1"} {}

  ad_utility::BufferedVector<std::array<Id, 2>> ids_;
  ad_utility::BufferedVector<Datatype>types0_;
  ad_utility::BufferedVector<Datatype>types1_;
  std::array<std::optional<Datatype>, 2> uniqueTypes_;

  size_t  size() const {
    AD_CHECK(ids_.size() == types0_.size() || uniqueTypes_[0]) ;
    AD_CHECK(ids_.size() == types1_.size() || uniqueTypes_[1]);
    return ids_.size();
  }
};

struct DatatypeInfoMerged {
  DatatypeInfoMerged (size_t threshold, std::string fileBasename) : ids_{threshold, fileBasename}, fileBasename_{std::move(fileBasename)}, threshold_{threshold} {}
  ad_utility::BufferedVector<std::array<IdWithDatatype, 2>> ids_;
  std::string fileBasename_;
  size_t threshold_;

  size_t  size() const {
    return ids_.size();
  }

  DatatypeInfo splitColumns() const {
    DatatypeInfo res{threshold_, fileBasename_ + ".split"};
    if ( std::adjacent_find( ids_.begin(), ids_.end(), [](const auto& a, const auto& b) {return a[0].type_ != b[0].type_;} ) == ids_.end() ) {
      res.uniqueTypes_[0] = ids_[0][0].type_;
    } else {
      for (const auto& el : ids_) {
        res.types0_.push_back(el[0].type_);
      }
    }

    if ( std::adjacent_find( ids_.begin(), ids_.end(), [](const auto& a, const auto& b) {return a[1].type_ != b[1].type_;} ) == ids_.end() ) {
      res.uniqueTypes_[1] = ids_[0][1].type_;
    } else {
      for (const auto& el : ids_) {
        res.types0_.push_back(el[1].type_);
      }
    }

    for (const auto& el : ids_) {
      res.ids_.push_back({el[0].value_, el[1].value_});
    }
    return res;

    }
};

class BlockMetaData {
 public:
  BlockMetaData() : _firstLhs{0, Datatype::String}, _startOffset(0) {}

  BlockMetaData(IdWithDatatype lhs, off_t start, off_t startType0) : _firstLhs(lhs), _startOffset(start), _startOffsetType0{startType0}  {}

  IdWithDatatype _firstLhs;
  off_t _startOffset;
  off_t _startOffsetType0 = 0;
};

class FullRelationMetaData {
 public:
  FullRelationMetaData();

  FullRelationMetaData(IdWithDatatype relId, off_t startFullIndex, size_t nofElements,
                       double col1Mult, double col2Mult, bool isFunctional,
                       bool hasBlocks, std::optional<Datatype> firstColumnDatatype,
                       std::optional<Datatype> secondColumnDatatype);

  static const FullRelationMetaData empty;

  size_t getNofBytesForFulltextIndex() const;

  // Returns true if there is exactly one RHS for each LHS in the relation
  bool isFunctional() const;

  bool hasBlocks() const;

  // Handle the fact that those properties are encoded in the same
  // size_t as the number of elements.
  void setIsFunctional(bool isFunctional);

  void setHasBlocks(bool hasBlocks);

  void setCol1LogMultiplicity(uint8_t mult);
  void setCol2LogMultiplicity(uint8_t mult);
  uint8_t getCol1LogMultiplicity() const;
  uint8_t getCol2LogMultiplicity() const;

  size_t getNofElements() const;

  // Restores meta data from raw memory.
  // Needed when registering an index on startup.
  FullRelationMetaData& createFromByteBuffer(unsigned char* buffer);

  // The size this object will require when serialized to file.
  size_t bytesRequired() const;

  // I think this is only for blocks
  off_t getStartOfLhs() const;
  off_t getStartOfTypeData() const;
  off_t getSizeOfTypeData() const;

  off_t getStartOfBlockTypes() const;

  IdWithDatatype _relId;
  off_t _startFullIndex;
  std::optional<Datatype> _firstColumnUniqueDatatype;
  std::optional<Datatype> _secondColumnUniqueDatatype;


  friend ad_utility::File& operator<<(ad_utility::File& f,
                                      const FullRelationMetaData& rmd);

  // operators needed for checking of emptyness
  // inequality is the common case, so we implement this
  bool operator!=(const FullRelationMetaData& other) const {
    return _relId != other._relId ||
           _typeMultAndNofElements != other._typeMultAndNofElements ||
           _startFullIndex != other._startFullIndex;
  }

  // __________________________________________________________________
  bool operator==(const FullRelationMetaData& other) const {
    return !(*this != other);
  }

 private:
  // first byte: type
  // second byte: log(col1Multiplicity)
  // third byte: log(col2Multiplicity)
  // other 5 bytes: the nof elements.
  uint64_t _typeMultAndNofElements;
};

inline ad_utility::File& operator<<(ad_utility::File& f,
                                    const FullRelationMetaData& rmd) {
  f.write(&rmd._relId, sizeof(rmd._relId));
  f.write(&rmd._startFullIndex, sizeof(rmd._startFullIndex));
  f.write(&rmd._typeMultAndNofElements, sizeof(rmd._typeMultAndNofElements));

  f.write(&rmd._firstColumnUniqueDatatype, sizeof(rmd._firstColumnUniqueDatatype));
  f.write(&rmd._secondColumnUniqueDatatype, sizeof(rmd._secondColumnUniqueDatatype));

  return f;
}

class BlockBasedRelationMetaData {
 public:
  BlockBasedRelationMetaData();

  BlockBasedRelationMetaData(off_t startRhs, off_t offsetAfter,
                             const vector<BlockMetaData>& blocks);

  // The size this object will require when serialized to file.
  size_t bytesRequired() const;

  // Restores meta data from raw memory.
  // Needed when registering an index on startup.
  BlockBasedRelationMetaData& createFromByteBuffer(unsigned char* buffer);

  // Takes a LHS and returns the offset into the file at which the
  // corresponding block can be read as well as the nof bytes to read.
  // If the relation is functional, this offset will be located in the
  // range of the FullIndex, otherwise it will be reference into the lhs list.
  // Reading nofBytes from the offset will yield a block which contains
  // the desired lhs if such a block exists at all.
  // If the lhs does not exists at all, this will only be clear after reading
  // said block.
  pair<off_t, size_t> getBlockStartAndNofBytesForLhs(IdWithDatatype lhs) const;

  // Gets the block after the one returned by getBlockStartAndNofBytesForLhs.
  // This is necessary for finding rhs upper bounds for the last item in a
  // block.
  // If this is equal to the block returned by getBlockStartAndNofBytesForLhs,
  // it means it is the last block and the offsetAfter can be used.
  pair<off_t, size_t> getFollowBlockForLhs(IdWithDatatype lhs) const;

  off_t _startRhs;
  off_t _startLhsTypes;
  off_t _startRhsTypes;
  off_t _offsetAfter;
  vector<BlockMetaData> _blocks;
};

inline ad_utility::File& operator<<(ad_utility::File& f,
                                    const BlockBasedRelationMetaData& rmd) {
  f.write(&rmd._startRhs, sizeof(rmd._startRhs));
  f.write(&rmd._startLhsTypes, sizeof(rmd._startLhsTypes));
  f.write(&rmd._startRhsTypes, sizeof(rmd._startRhsTypes));
  f.write(&rmd._offsetAfter, sizeof(rmd._offsetAfter));
  auto nofBlocks = rmd._blocks.size();
  f.write(&nofBlocks, sizeof(nofBlocks));
  f.write(rmd._blocks.data(), nofBlocks * sizeof(BlockMetaData));
  return f;
}

class RelationMetaData {
 public:
  explicit RelationMetaData(const FullRelationMetaData& rmdPairs)
      : _rmdPairs(rmdPairs), _rmdBlocks(nullptr) {}

  off_t getStartOfLhs() const { return _rmdPairs.getStartOfLhs(); }

  size_t getNofBytesForFulltextIndex() const {
    return _rmdPairs.getNofBytesForFulltextIndex();
  }

  // Returns true if there is exactly one RHS for each LHS in the relation
  bool isFunctional() const { return _rmdPairs.isFunctional(); }

  bool hasBlocks() const { return _rmdPairs.hasBlocks(); }

  size_t getNofElements() const { return _rmdPairs.getNofElements(); }

  uint8_t getCol1LogMultiplicity() const {
    return _rmdPairs.getCol1LogMultiplicity();
  }

  uint8_t getCol2LogMultiplicity() const {
    return _rmdPairs.getCol2LogMultiplicity();
  }

  const FullRelationMetaData& _rmdPairs;
  const BlockBasedRelationMetaData* _rmdBlocks;
};

inline ad_utility::File& operator<<(ad_utility::File& f,
                                    const RelationMetaData& rmd) {
  f << rmd._rmdPairs << *rmd._rmdBlocks;
  return f;
}
