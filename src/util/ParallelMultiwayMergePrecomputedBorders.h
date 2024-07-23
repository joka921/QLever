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
  size_t numElements_{};
  size_t totalSize_{};
};

template <typename T>
struct MergeRange {
  struct Blocks {
    size_t firstBlockIdx_{};
    size_t endBlockIdx_{};
    bool operator==(const Blocks&) const = default;
    friend std::ostream& operator<<(std::ostream& str, const Blocks& block) {
      str << '(' << block.firstBlockIdx_ << ", " << block.endBlockIdx_ << ']';
      return str;
    }
  };
  std::vector<Blocks> blocks_;
  std::optional<T> first_;
  std::optional<T> last_;
};

template <typename T>
std::vector<MergeRange<T>> getMergeParts(
    std::vector<std::vector<BlockMetadata<T>>> input,
    [[maybe_unused]] size_t sizeLimitPerBlock) {
  std::vector<size_t> firstUntouchedBlock(input.size(), 0);
  std::vector<size_t> firstUnfinishedBlock(input.size(), 0);

  auto pairIsValid = [&](const auto& pair) {
    return std::get<1>(pair) < input.at(std::get<0>(pair)).size();
  };

  [[maybe_unused]] auto pairToBlock = [&](const auto& pair) -> decltype(auto) {
    AD_CORRECTNESS_CHECK(pairIsValid(pair));
    return input.at(std::get<0>(pair)).at(std::get<1>(pair));
  };

  auto enumerate = ad_utility::ranges::views::enumerate;

  auto unfinishedBlockGenerator = [&]() {
    return enumerate(firstUnfinishedBlock) | std::views::filter(pairIsValid);
  };

  auto findNextBlock = [&]() {
    auto unfinishedBlocks = unfinishedBlockGenerator();
    auto it =
        std::ranges::min_element(unfinishedBlocks, {}, [&](const auto& p) {
          const auto& [a, b] = p;
          return input.at(a).at(b).last_;
        });
    return it == unfinishedBlocks.end() ? std::nullopt : std::optional{*it};
  };

  auto addAllRequiredBlocks = [&](MergeRange<T>& result,
                                  const T& highestElement,
                                  size_t& numFinishedBlocks) {
    for (const auto& [idx, blocks] : enumerate(input)) {
      if (firstUnfinishedBlock[idx] == blocks.size()) {
        continue;
      }

      auto relevantBlocks =
          enumerate(blocks) | std::views::drop(firstUnfinishedBlock[idx]);
      auto itFinished = std::upper_bound(
          relevantBlocks.begin(), relevantBlocks.end(), highestElement,
          [](const T& value, const auto& idxAndBlock) {
            return value < std::get<1>(idxAndBlock).last_;
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

      auto diff = itFinished - relevantBlocks.begin();
      numFinishedBlocks += diff;
      if (itFinished != relevantBlocks.end()) {
        firstUnfinishedBlock[idx] = std::get<0>(*itFinished);
      } else {
        firstUnfinishedBlock[idx] = blocks.size();
      }
    }
  };

  std::vector<MergeRange<T>> result;
  while (true) {
    MergeRange<T> r;
    r.blocks_.resize(input.size());
    size_t numFinishedBlocks = 0;
    while (numFinishedBlocks < 3) {
      auto opt = findNextBlock();
      if (!opt.has_value()) {
        if (numFinishedBlocks > 0) {
          result.push_back(std::move(r));
        }
        return result;
      }
      const auto& [a, b] = *opt;
      const auto& block = input.at(a).at(b);
      addAllRequiredBlocks(r, block.last_, numFinishedBlocks);
      r.last_ = block.last_;
    }
    result.push_back(std::move(r));
  }
  return result;
}

}  // namespace ad_utility::parallelMultiwayMergeBorders
