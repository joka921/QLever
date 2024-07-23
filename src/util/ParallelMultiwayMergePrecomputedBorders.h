//  Copyright 2024, University of Freiburg,
//                  Chair of Algorithms and Data Structures.
//  Author: Johannes Kalmbach <kalmbach@cs.uni-freiburg.de>

#pragma once
#include "util/AsyncStream.h"
#include "util/EnumerateView.h"
#include "util/Generator.h"
#include "util/TypeTraits.h"
#include "util/ValueSizeGetters.h"
#include "util/Views.h"

namespace ad_utility::parallelMultiwayMergeBorders {

template <typename T>
struct BlockMetadata {
  T first_;
  T last_;
  size_t numElements_;
  size_t totalSize_;
};

template <typename T>
struct MergeRange {
  struct Blocks {
    size_t firstBlockIdx_{};
    size_t endBlockIdx_{};
  };
  std::vector<Blocks> blocks_;
  std::optional<T> first_;
  std::optional<T> last_;
};

template <typename T>
std::vector<MergeRange<T>> getMergeParts(
    std::vector<std::vector<BlockMetadata<T>>> input,
    size_t sizeLimitPerBlock) {
  std::vector<size_t> firstUntouchedBlock(input.size(), 0);
  std::vector<size_t> firstUnfinishedBlock(input.size(), 0);

  auto pairIsValid = [&](const auto& pair) {
    return std::get<1>(pair) < input.at(std::get<0>(pair)).size();
  };

  auto pairToBlock = [&](const auto& pair) -> decltype(auto) {
    AD_CORRECTNESS_CHECK(pairIsValid(pair));
    return input.at(std::get<0>(pair)).at(std::get<1>(pair));
  };

  auto enumerate = ad_utility::ranges::views::enumerate;

  auto unfinishedBlockGenerator = [&]() {
    return enumerate(firstUnfinishedBlock) | std::views::filter(pairIsValid);
  };

  auto findNextBlock = [&]() {
    return std::ranges::min_element(unfinishedBlockGenerator(), {},
                                    [&](const auto& p) {
                                      const auto& [a, b] = p;
                                      return input.at(a).at(b).last_;
                                    });
  };

  auto addAllRequiredBlocks = [&](MergeRange<T> result, const T& highestElement,
                                  size_t filteredIdx) {
    for (const auto& [idx, blocks] : enumerate(input)) {
      if (idx == filteredIdx || firstUnfinishedBlock[idx] == blocks.size()) {
        continue;
      }

      auto relevantBlocks =
          enumerate(blocks) | std::views::drop(firstUnfinishedBlock[idx]);
      auto itFinished = std::ranges::lower_bound(
          relevantBlocks, [&](const auto& idxAndBlock) {
            return std::get<1>(idxAndBlock).last_ >= highestElement;
          });
      auto itLarger = std::ranges::find_if(
          itFinished, relevantBlocks.end(), [&](const auto& idxAndBlock) {
            return std::get<1>(idxAndBlock).first_ > highestElement;
          });

      if (itLarger != relevantBlocks.end()) {
        result.blocks_[idx].endBlockIdx_ = std::get<0>(*itLarger);
      } else {
        result.blocks_[idx].endBlockIdx_ = blocks.size();
      }

      if (itLarger != relevantBlocks.begin()) {
      }
    }
  };

  MergeRange<T> r;
  r.blocks_.resize(input.size());
  const auto [a, b] = *findNextBlock();
  const auto& block = input.at(a).at(b);

  return {};
}

}  // namespace ad_utility::parallelMultiwayMergeBorders
