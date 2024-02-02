// Copyright 2022, University of Freiburg,
// Chair of Algorithms and Data Structures.
// Author: Johannes Kalmbach <johannes.kalmbach@gmail.com>

#include "index/VocabularyOnDisk.h"

#include <fstream>

#include "util/Generator.h"
#include "util/StringUtils.h"

using OffsetAndSize = VocabularyOnDisk::OffsetAndSize;

// ____________________________________________________________________________
std::optional<OffsetAndSize> VocabularyOnDisk::getOffsetAndSize(
    uint64_t idx) const {
  IndexAndOffset idAndDummyOffset{idx, 0};
  auto it = std::lower_bound(_idsAndOffsets.begin(), _idsAndOffsets.end(),
                             idAndDummyOffset);
  if (it >= _idsAndOffsets.end() - 1 || it->_idx != idx) {
    return std::nullopt;
  }
  return getOffsetAndSizeForIthElement(it - _idsAndOffsets.begin());
}

// ____________________________________________________________________________
VocabularyOnDisk::OffsetSizeId VocabularyOnDisk::getOffsetSizeIdForIthElement(
    uint64_t i) const {
  AD_CONTRACT_CHECK(i < size());
  const auto offset = _idsAndOffsets[i]._offset;
  const auto nextOffset = _idsAndOffsets[i + 1]._offset;
  return OffsetSizeId{offset, nextOffset - offset, _idsAndOffsets[i]._idx};
}

// _____________________________________________________________________________
std::optional<string> VocabularyOnDisk::operator[](uint64_t idx) const {
  auto optionalOffsetAndSize = getOffsetAndSize(idx);
  if (!optionalOffsetAndSize.has_value()) {
    return std::nullopt;
  }

  string result(optionalOffsetAndSize->_size, '\0');
  _file.read(result.data(), optionalOffsetAndSize->_size,
             optionalOffsetAndSize->_offset);
  return result;
}

// _____________________________________________________________________________
ad_utility::ConsumerImpl<std::string_view> VocabularyOnDisk::wordWriterImpl(
    std::string outFileName) {
  _file.open(outFileName.c_str(), "w");
  ad_utility::MmapVector<IndexAndOffset> idsAndOffsets(
      outFileName + _offsetSuffix, ad_utility::CreateTag{});
  uint64_t currentOffset = 0;
  std::optional<uint64_t> previousId = std::nullopt;
  uint64_t currentIndex = 0;
  while (co_await ad_utility::valueWasPushedTag) {
    const auto& word = co_await ad_utility::nextValueTag;
    AD_CONTRACT_CHECK(!previousId.has_value() ||
                      previousId.value() < currentIndex);
    idsAndOffsets.push_back(IndexAndOffset{currentIndex, currentOffset});
    currentOffset += _file.write(word.data(), word.size());
    previousId = currentIndex;
    ++currentIndex;
  }

  // End offset of last vocabulary entry, also consistent with the empty
  // vocabulary.
  auto endIndex = previousId.value_or(_highestIdx);
  idsAndOffsets.push_back(IndexAndOffset{endIndex, currentOffset});
  _file.close();
  idsAndOffsets.close();
  open(outFileName);
}

// _____________________________________________________________________________
template <typename Iterable>
void VocabularyOnDisk::buildFromIterable(Iterable&& it,
                                         const string& fileName) {
  {
    _file.open(fileName.c_str(), "w");
    ad_utility::MmapVector<IndexAndOffset> idsAndOffsets(
        fileName + _offsetSuffix, ad_utility::CreateTag{});
    uint64_t currentOffset = 0;
    std::optional<uint64_t> previousId = std::nullopt;
    for (const auto& [word, id] : it) {
      AD_CONTRACT_CHECK(!previousId.has_value() || previousId.value() < id);
      idsAndOffsets.push_back(IndexAndOffset{id, currentOffset});
      currentOffset += _file.write(word.data(), word.size());
      previousId = id;
    }

    // End offset of last vocabulary entry, also consistent with the empty
    // vocabulary.
    if (previousId.has_value()) {
      idsAndOffsets.push_back(
          IndexAndOffset{previousId.value() + 1, currentOffset});
    } else {
      idsAndOffsets.push_back(IndexAndOffset{_highestIdx + 1, currentOffset});
    }
    _file.close();
  }  // After this close, the destructor of MmapVector is called, whoch dumps
     // everything to disk.
  open(fileName);
}

// _____________________________________________________________________________
void VocabularyOnDisk::buildFromVector(const vector<string>& words,
                                       const string& fileName) {
  // Note: Using a reference-capture for `words` will segfault in GCC11.
  // TODO<joka921> This is a bug in the compiler, report it if still unknown or
  // post reference link here.
  auto generator = [](const auto& words)
      -> cppcoro::generator<std::pair<std::string_view, uint64_t>> {
    uint64_t index = 0;
    for (const auto& word : (words)) {
      // Note: Yielding the temporary directly would segfault in GCC, this is a
      // bug in GCC, see similar places in the `streamable_generator` class.
      std::pair<std::string, uint64_t> tmp{word, index};
      co_yield tmp;
      index++;
    }
  }(words);
  buildFromIterable(std::move(generator), fileName);
}

// _____________________________________________________________________________
void VocabularyOnDisk::buildFromStringsAndIds(
    const vector<std::pair<std::string, uint64_t>>& wordsAndIds,
    const string& fileName) {
  return buildFromIterable(wordsAndIds, fileName);
}

// _____________________________________________________________________________
void VocabularyOnDisk::open(const string& filename) {
  _file.open(filename.c_str(), "r");
  _idsAndOffsets.open(filename + _offsetSuffix);
  AD_CONTRACT_CHECK(_idsAndOffsets.size() > 0);
  _size = _idsAndOffsets.size() - 1;
  if (_size > 0) {
    _highestIdx = (*(end() - 1))._index;
  }
}

// ____________________________________________________________________________
WordAndIndex VocabularyOnDisk::getIthElement(size_t n) const {
  AD_CONTRACT_CHECK(n < _idsAndOffsets.size());
  auto offsetSizeId = getOffsetSizeIdForIthElement(n);

  string result(offsetSizeId._size, '\0');
  _file.read(result.data(), offsetSizeId._size, offsetSizeId._offset);

  return {std::move(result), offsetSizeId._id};
}
