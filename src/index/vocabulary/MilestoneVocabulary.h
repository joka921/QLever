// Copyright 2024
// University of Freiburg
// Chair of Algorithms and Data Structures
//
// Author: Johannes Kalmbach <kalmbach@cs.uni-freiburg.de>

#pragma once

#include <string>
#include <string_view>

#include "global/Id.h"
#include "index/vocabulary/VocabularyTypes.h"
#include "util/Iterators.h"

// TODO<joka921> We need to handle the externalization also in this module.
template <size_t distanceBetweenMilestones, typename InternalVocabulary,
          typename ExternalVocabulary>
class MilestoneVocabulary {
 public:
  using CharType = char;
  using StringView = std::basic_string_view<CharType>;
  using String = std::basic_string<CharType>;

 private:
  InternalVocabulary internalVocabulary_;
  ExternalVocabulary externalVocabulary_;
  using MilestoneManager =
      ad_utility::MilestoneIdManager<distanceBetweenMilestones>;

 public:
  /// Construct an empty vocabulary
  MilestoneVocabulary();

  /// Construct the vocabulary.
  MilestoneVocabulary(InternalVocabulary internal, ExternalVocabulary external)
      : internalVocabulary_{std::move(internal)},
        externalVocabulary_{std::move(external)} {}

  // Vocabularies are movable
  MilestoneVocabulary& operator=(MilestoneVocabulary&&) noexcept = default;
  MilestoneVocabulary(MilestoneVocabulary&&) noexcept = default;

  /// Read the vocabulary from a file. The file must have been created by a call
  /// to `writeToFile` or using a `WordWriter`.
  void open(const std::string& fileName) {
    internalVocabulary_.open(fileName);
    externalVocabulary_.open(fileName);
  }

  /// Return the total number of words
  [[nodiscard]] size_t size() const {
    return internalVocabulary_.size() + externalVocabulary_.size();
  }

  /// Return the word that corresponds to the milestone ID `i`.
  /// The behavior is undefined if no such word is contained in the vocabulary.
  auto operator[](uint64_t i) const {
    return MilestoneManager::isMilestoneId(i)
               ? internalVocabulary_[MilestoneManager::milestoneIdToLocal(i)]
               : externalVocabulary_[i];
  }

  template <typename InternalStringType, typename Comparator>
  WordAndIndex lower_bound(const InternalStringType& word,
                           Comparator comparator) const {
    auto lowerInternal = internalVocabulary_.lowerBound(word, comparator);
    auto lowerIdx = lowerInternal - internalVocabulary_.begin();
    auto lowerIdxExternal =
        std::min(externalVocabulary_.size(),
                 MilestoneManager::milestoneIdFromLocal(lowerIdx));
    auto upperIdxExternal =
        std::min(externalVocabulary_.size(),
                 MilestoneManager::milestoneIdFromLocal(lowerIdx + 1));

    auto lowerItExternal = externalVocabulary_.lowerBound(
        lowerIdxExternal, upperIdxExternal, word, comparator);
    WordAndIndex result;
    result._index = lowerItExternal - externalVocabulary_.begin();
    result._word = result._index < externalVocabulary_.size()
                       ? std::optional{*lowerItExternal}
                       : std::nullopt;
    return result;
  }

  // Same as `lower_bound`, but compares an `iterator` and a `value` instead of
  // two values. Required by the `CompressedVocabulary`.
  template <typename InternalStringType, typename Comparator>
  WordAndIndex lower_bound_iterator(const InternalStringType& word,
                                    Comparator comparator) const {
    auto lowerInternal =
        internalVocabulary_.lower_bound_iterator(word, comparator);
    auto lowerIdx = lowerInternal - internalVocabulary_.begin();
    auto lowerIdxExternal =
        std::min(externalVocabulary_.size(),
                 MilestoneManager::milestoneIdFromLocal(lowerIdx));
    auto upperIdxExternal =
        std::min(externalVocabulary_.size(),
                 MilestoneManager::milestoneIdFromLocal(lowerIdx + 1));

    auto lowerItExternal = externalVocabulary_.lower_bound_iterator(
        lowerIdxExternal, upperIdxExternal, word, comparator);
    WordAndIndex result;
    result._index = lowerItExternal - externalVocabulary_.begin();
    result._word = result._index < externalVocabulary_.size()
                       ? std::optional{*lowerItExternal}
                       : std::nullopt;
    return result;
  }

  /// Return a `WordAndIndex` that points to the first entry that is greater
  /// than `word` wrt. to the `comparator`. Only works correctly if the `_words`
  /// are sorted according to the comparator (exactly like in
  /// `std::upper_bound`, which is used internally).
  template <typename InternalStringType, typename Comparator>
  WordAndIndex upper_bound(const InternalStringType& word,
                           Comparator comparator) const {
    AD_FAIL();
    // TODO<joka921> Implement in analogy to lowerBound.
  }

  // Same as `upper_bound`, but compares a `value` and an `iterator` instead of
  // two values. Required by the `CompressedVocabulary`.
  template <typename InternalStringType, typename Comparator>
  WordAndIndex upper_bound_iterator(const InternalStringType& word,
                                    Comparator comparator) const {
    AD_FAIL();
    // TODO<joka921> Implement in analogy to lowerBound.
  }

  /// A helper type that can be used to directly write a vocabulary to disk
  /// word-by-word, without having to materialize it in RAM first. See the
  /// documentation of `CompactVectorOfStrings` for details.
  struct WordWriter {
    explicit WordWriter(const std::string& filename) { AD_FAIL(); }
    void operator()(std::string_view str, bool isExternal) { AD_FAIL(); }

    void finish() { AD_FAIL(); }
  };

  // Return a `WordWriter` that directly writes the words to the given
  // `filename`. The words are not materialized in RAM, but the vocabulary later
  // has to be explicitly initialized via `open(filename)`.
  WordWriter makeDiskWriter(const std::string& filename) const {
    return WordWriter{filename};
  }

  /// Clear the vocabulary.
  void close() {
    internalVocabulary_.close();
    externalVocabulary_.close();
  }

  // Const random access iterators, implemented via the
  // `IteratorForAccessOperator` template.
  using const_iterator =
      ad_utility::IteratorForAccessOperator<MilestoneVocabulary>;
  const_iterator begin() const { return {this, 0}; }
  const_iterator end() const { return {this, size()}; }
};
