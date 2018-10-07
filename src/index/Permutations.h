// Copyright 2018, University of Freiburg,
// Chair of Algorithms and Data Structures.
// Author: Johannes Kalmbach<joka921> (johannes.kalmbach@gmail.com)
#pragma once

#include <array>
#include <string>
#include "./StxxlSortFunctors.h"
#include "../global/Constants.h"

namespace Permutation {
using std::array;
using std::string;

typedef vector<array<Id, 1>> WidthOneList;
typedef vector<array<Id, 2>> WidthTwoList;
typedef vector<array<Id, 3>> WidthThreeList;
typedef vector<array<Id, 4>> WidthFourList;
typedef vector<array<Id, 5>> WidthFiveList;
typedef vector<vector<Id>> VarWidthList;

// helper class to store static properties of the different permutations
// to avoid code duplication
// The template Parameter is a STXXL search functor
template <class Comparator>
class PermutationImpl {
 public:
  PermutationImpl() = default;
  PermutationImpl(const Comparator& comp, string name, string suffix,
                  array<unsigned short, 3> order)
      : _comp(comp),
        _readableName(std::move(name)),
        _fileSuffix(std::move(suffix)),
        _keyOrder(order) {}

  // stxxl comparison functor
  const Comparator _comp;
  // for Log output, e.g. "POS"
  const std::string _readableName;
  // e.g. ".pos"
  const std::string _fileSuffix;
  // order of the 3 keys S(0), P(1), and O(2) for which this permutation is
  // sorted. Needed for the createPermutation function in the Index class
  // e.g. {1, 0, 2} for PsO
  const array<unsigned short, 3> _keyOrder;
};

template <class MetaData, class Impl>
class Permutation : public Impl {
 public:
  Permutation(const Impl& impl) : Impl(impl) {}
  void initialize(const string& onDiskBase) {
    auto filename = onDiskBase + ".index" + this->_fileSuffix;
    _file.open(filename, "r");
    AD_CHECK(_file.isOpen());
    if constexpr (MetaData::_isMmapBased) {
      _metaData.setup(filename + MMAP_FILE_SUFFIX,
                     ad_utility::ReuseTag(), ad_utility::AccessPattern::Random);
    }
    _metaData.readFromFile(&_file);

  }
  // ____________________________________________________________
  void scan(Id key, WidthTwoList* result) const {
    if (_metaData.relationExists(key)) {
      const FullRelationMetaData& rmd = _metaData.getRmd(key)._rmdPairs;
      result->reserve(rmd.getNofElements() + 2);
      result->resize(rmd.getNofElements());
      _file.read(result->data(), rmd.getNofElements() * 2 * sizeof(Id),
                    rmd._startFullIndex);
    }
  }
  void scan(Id keyFirst, Id keySecond,
                      WidthOneList* result) const {
      if (_metaData.relationExists(keyFirst)) {
        auto rmd = _metaData.getRmd(keyFirst);
        if (rmd.hasBlocks()) {
          pair<off_t, size_t> blockOff =
                  rmd._rmdBlocks->getBlockStartAndNofBytesForLhs(keySecond);
          // Functional relations have blocks point into the pair index,
          // non-functional relations have them point into lhs lists
          if (rmd.isFunctional()) {
            scanFunctionalRelation(blockOff, keySecond, _file, result);
          } else {
            pair<off_t, size_t> block2 =
                    rmd._rmdBlocks->getFollowBlockForLhs(keySecond);
            scanNonFunctionalRelation(blockOff, block2, keySecond, _file,
                                      rmd._rmdBlocks->_offsetAfter, result);
          }
        } else {
          // If we don't have blocks, scan the whole relation and filter /
          // restrict.
          WidthTwoList fullRelation;
          fullRelation.resize(rmd.getNofElements());
          _file.read(fullRelation.data(),
                        rmd.getNofElements() * 2 * sizeof(Id),
                        rmd._rmdPairs._startFullIndex);
          getRhsForSingleLhs(fullRelation, keySecond, result);
        }
      } else {
        LOG(DEBUG) << "No such relation.\n";

    }
    LOG(DEBUG) << "Scan done, got " << result->size() << " elements.\n";
  }

  // _____________________________________________________________________________
  void scanFunctionalRelation(const pair<off_t, size_t>& blockOff,
                                     Id lhsId, ad_utility::File& indexFile,
                                     WidthOneList* result) const {
    LOG(TRACE) << "Scanning functional relation ...\n";
    WidthTwoList block;
    block.resize(blockOff.second / (2 * sizeof(Id)));
    indexFile.read(block.data(), blockOff.second, blockOff.first);
    auto it = std::lower_bound(
            block.begin(), block.end(), lhsId,
            [](const array<Id, 2>& elem, Id key) { return elem[0] < key; });
    if ((*it)[0] == lhsId) {
      result->push_back(array<Id, 1>{(*it)[1]});
    }
    LOG(TRACE) << "Read " << result->size() << " RHS.\n";
  }

  // _____________________________________________________________________________
  void getRhsForSingleLhs(const WidthTwoList& in, Id lhsId,
                           WidthOneList* result) const {
    LOG(DEBUG) << "Getting only rhs from a relation with " << in.size()
               << " elements by an Id key.\n";
    AD_CHECK(result);
    AD_CHECK_EQ(0, result->size());

    auto it = std::lower_bound(
            in.begin(), in.end(), array<Id, 2>{{lhsId, 0}},
            [](const array<Id, 2>& a, const array<Id, 2>& b) { return a[0] < b[0]; });

    while (it != in.end() && it->operator[](0) == lhsId) {
      result->emplace_back(array<Id, 1>{{it->operator[](1)}});
      ++it;
    }

    LOG(DEBUG) << "Done. Matching right-hand-side EntityList now has "
               << result->size() << " elements."
               << "\n";
  }

private:
  mutable ad_utility::File _file {};
  MetaData _metaData {};
};

// instantiations for the 6 Permutations used in QLever
// They simplify the creation of permutations in the index class
const PermutationImpl<SortByPOS> Pos(SortByPOS(), "POS", ".pos", {1, 2, 0});
const PermutationImpl<SortByPSO> Pso(SortByPSO(), "PSO", ".pso", {1, 0, 2});
const PermutationImpl<SortBySOP> Sop(SortBySOP(), "SOP", ".sop", {0, 2, 1});
const PermutationImpl<SortBySPO> Spo(SortBySPO(), "SPO", ".spo", {0, 1, 2});
const PermutationImpl<SortByOPS> Ops(SortByOPS(), "OPS", ".ops", {2, 1, 0});
const PermutationImpl<SortByOSP> Osp(SortByOSP(), "OSP", ".osp", {2, 0, 1});
}  // namespace Permutation
