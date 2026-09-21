// Copyright 2026 The QLever Authors, in particular:
//
// 2026 Johannes Kalmbach <kalmbach@cs.uni-freiburg.de>, UFR
//
// UFR = University of Freiburg, Chair of Algorithms and Data Structures
//
// You may not use this file except in compliance with the Apache 2.0 License,
// which can be found in the `LICENSE` file at the root of the QLever project.

#ifndef QLEVER_SRC_BLOBCONVERTER_LEGACYBLOBREADER_H
#define QLEVER_SRC_BLOBCONVERTER_LEGACYBLOBREADER_H

#include <array>
#include <cstdint>
#include <optional>
#include <string>
#include <utility>
#include <variant>
#include <vector>

#include "backports/span.h"
#include "engine/SpatialJoinCachedIndex.h"
#include "engine/VariableToColumnMap.h"
#include "index/vocabulary/CompressedVocabulary.h"
#include "index/vocabulary/VocabularyInMemory.h"
#include "libqlever/NamedCachedQueryBlobManager.h"
#include "util/BlankNodeManager.h"
#include "util/json.h"

// Reading of the legacy blob format that is written by
// `Qlever::serializeToUncompressedBlob` of the `demo-v1-c++17_unimodel` branch
// of the `qlever-bmw` fork. A legacy blob is a single ZSTD frame that is
// followed by the size of the uncompressed data (8 bytes, little endian). The
// uncompressed data consists of:
//
// 1. The magic header `"QLVUBLOB"` (serialized as a `std::string`) and the
//    format version `1` (a `uint32_t`).
// 2. The index metadata JSON (a `std::string`).
// 3. The vocabulary, in the format that the value of `"vocabulary-type"` in the
//    metadata determines (see `LegacyVocabulary`).
// 4. The `NamedResultCache` (see `LegacyNamedCacheEntry`), without any header.
//
// The byte-level rules of the legacy serializer are the same as those of the
// current `AlignedByteBufferWriteSerializer` (no padding for single values,
// padding to the alignment of the element type after the size of a vector,
// span, or string), with one caveat: the fork changed where alignment padding
// is inserted while the blob format was already at version 1 (see
// `LegacyPaddingConvention`). The reader therefore tries the known padding
// conventions one after the other and accepts the first one under which the
// complete blob parses consistently.
namespace qlever::blobConverter {

// Where the legacy writer inserted alignment padding (zero bytes). Two places
// are affected:
//
// 1. Inside the serialization of a vector, span, or string of trivially
//    serializable elements, directly after the `uint64_t` size: padding to the
//    alignment of the element type (`padInsideVectors_`). This is what the
//    final version of the fork (commit `56172e855`, and the versions from
//    commit `f176c255e` of 2026-01-20 on) did, via `alignForType` in its
//    `SerializeVector.h`.
// 2. Explicitly before the `NamedResultCache` (that is, before its number of
//    entries) and before each column of an `IdTable` (that is, before the size
//    of the column): padding to `alignof(Id)`
//    (`explicitAlignmentBeforeCacheAndColumns_`). This is what the first
//    versions of the fork that wrote the `QLVUBLOB` format did (commits
//    `42161cd92` and `e34f1bc14` of 2026-01-19), which at the same time did
//    NOT pad inside vectors. Those versions also aligned to `alignof(char)`
//    before the vocabulary, which is a no-op.
//
// In both places the padding fills up to the next multiple of the alignment
// and is empty if the position is already aligned.
struct LegacyPaddingConvention {
  bool padInsideVectors_ = true;
  bool explicitAlignmentBeforeCacheAndColumns_ = false;

  // A human-readable description of the convention.
  std::string description() const;

  bool operator==(const LegacyPaddingConvention& other) const {
    return padInsideVectors_ == other.padInsideVectors_ &&
           explicitAlignmentBeforeCacheAndColumns_ ==
               other.explicitAlignmentBeforeCacheAndColumns_;
  }
  bool operator!=(const LegacyPaddingConvention& other) const {
    return !(*this == other);
  }
};

// The padding conventions that `readLegacyBlob` tries, in this order. The
// first one is the convention of the final version of the fork (and of all the
// blobs that are known to exist), so that blob is parsed at the first attempt.
inline constexpr std::array<LegacyPaddingConvention, 4>
    legacyPaddingConventions{LegacyPaddingConvention{true, false},
                             LegacyPaddingConvention{true, true},
                             LegacyPaddingConvention{false, true},
                             LegacyPaddingConvention{false, false}};

// The vocabulary implementations of the legacy format that a blob can hold.
// Their byte layout is identical to that of the current implementations of the
// same name, so the current types are used directly.
using LegacyVocabulary =
    std::variant<VocabularyInMemory, CompressedVocabulary<VocabularyInMemory>>;

// One entry of the legacy `NamedResultCache`. The `Id`s of the result are kept
// as raw bits, because their datatype bits have to be reinterpreted for the
// current format (see `LegacyDatatype.h`).
struct LegacyNamedCacheEntry {
  std::string name_;
  std::vector<
      ad_utility::BlankNodeManager::LocalBlankNodeManager::OwnedBlocksEntry>
      blankNodeBlocks_;
  // The words of the `LocalVocab` of the entry, each preceded by the bits of
  // the legacy `Id` that referred to it.
  std::vector<std::pair<uint64_t, std::string>> localVocabWords_;
  size_t numRows_ = 0;
  size_t numColumns_ = 0;
  // `numColumns_` columns with `numRows_` legacy `Id`s each.
  std::vector<std::vector<uint64_t>> columns_;
  // The variable names (including the leading `?`) and their columns.
  std::vector<std::pair<std::string, ColumnIndexAndTypeInfo>> variables_;
  std::vector<ColumnIndex> resultSortedOn_;
  std::string cacheKey_;
  // The layout of the geo index has not changed, and it only refers to row
  // indices of the result, so it is read directly into the current type.
  std::optional<SpatialJoinCachedIndex> geoIndex_;
};

// The complete contents of a legacy blob.
struct LegacyBlob {
  nlohmann::json metadata_;
  LegacyVocabulary vocabulary_;
  std::vector<LegacyNamedCacheEntry> entries_;
  // The padding convention under which the blob was parsed (see
  // `readLegacyBlob`).
  LegacyPaddingConvention paddingConvention_;

  // The number of words in the vocabulary, and the word at a given index.
  size_t numWords() const;
  std::string word(uint64_t index) const;

  // Find the entry with the given `name`, or return `nullptr`.
  const LegacyNamedCacheEntry* findEntry(std::string_view name) const;
};

// The buffer for the decompressed blob. It is aligned to the maximal possible
// alignment, which the aligned read serializer requires.
using DecompressedBuffer =
    std::vector<char, NamedCachedQueryBlobManager::BlobAllocator>;

// Decompress a legacy blob (a ZSTD frame followed by the 8-byte uncompressed
// size). Throw a `std::runtime_error` with a descriptive message if the input
// is not a legacy blob.
DecompressedBuffer decompressLegacyBlob(ql::span<const char> compressedBlob);

// Read the decompressed contents of a legacy blob (see `decompressLegacyBlob`)
// under the given padding `convention`. Throw a `std::runtime_error` if the
// header is wrong, if the vocabulary type is not supported, if the contents
// are inconsistent (this includes non-zero padding bytes), or if not all the
// bytes of the input are consumed. NOTE: The `decompressedBlob` has to be
// aligned to `alignof(std::max_align_t)` (which a `DecompressedBuffer`
// guarantees).
LegacyBlob readLegacyBlob(ql::span<const char> decompressedBlob,
                          const LegacyPaddingConvention& convention);

// Same as above, but try all the `legacyPaddingConventions` in their order and
// return the result of the first one under which the blob parses completely
// and consistently (recorded in `LegacyBlob::paddingConvention_`). If none
// does, rethrow the error of the first convention, with a note that all the
// conventions were tried.
LegacyBlob readLegacyBlob(ql::span<const char> decompressedBlob);

// The combination of `decompressLegacyBlob` and `readLegacyBlob`.
LegacyBlob readLegacyBlobFromCompressed(ql::span<const char> compressedBlob);

}  // namespace qlever::blobConverter

#endif  // QLEVER_SRC_BLOBCONVERTER_LEGACYBLOBREADER_H
