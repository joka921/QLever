// Copyright 2020, University of Freiburg,
// Chair of Algorithms and Data Structures.
// Author: Johannes Kalmbach (johannes.kalmbach@gmail.com)

#ifndef QLEVER_COMPRESSEDDEQUE_H
#define QLEVER_COMPRESSEDDEQUE_H

/**
 * @brief externally behaves like an array, but the contents are
 *        blockwise encrypted
 */

#include <vector>
#include "../global/Pattern.h"
#include "../util/Log.h"
#include "ByteBuffer.h"
#include "zstd.h"
#include "./Exception.h"

using std::vector;

class ZstdCompressor {
public:
  static ad_utility::ByteBuffer compress(const ad_utility::ByteBuffer& in) {
    const size_t bufSize = ZSTD_compressBound(in.size());
    std::unique_ptr<uint8_t[]> buf{new uint8_t[bufSize]};
    constexpr int compressionLevel = 3;
    const auto sz = ZSTD_compress(buf.get(), bufSize, in.data(), in.size(), compressionLevel);
    LOG(INFO) << "Compressed from " << in.size() << " to " << sz << " bytes\n";
    return {buf.get(), sz};
  }
  static ad_utility::ByteBuffer decompress(const ad_utility::ByteBuffer& in) {
    unsigned long long const bufSize = ZSTD_getFrameContentSize(in.data(), in.size());
    AD_CHECK(bufSize != ZSTD_CONTENTSIZE_ERROR); //, "%s: not compressed by zstd!", fname);
    AD_CHECK(bufSize != ZSTD_CONTENTSIZE_UNKNOWN);  //, "%s: original size unknown!", fname);
    ad_utility::ByteBuffer buf{bufSize};
    const auto sz = ZSTD_decompress(buf.data(), bufSize, in.data(), in.size());
    AD_CHECK(sz == bufSize); // this is guaranteed by zstd
    LOG(INFO) << "Decompressed from " << in.size() << " to " << sz << " bytes\n";
    return buf;
  }
};

template <size_t Blocksize>
class CompressedQueue {
public:
  struct It {
    using difference_type = size_t;
    using value_type = std::string;
    using pointer = std::string*;
    using reference = std::string&;
    using iterator_category = std::random_access_iterator_tag;
    const CompressedQueue* _q;
    size_t _idx;
    bool operator==(const It& rhs) {return _idx = rhs._idx;}

    It(const CompressedQueue* q, size_t idx): _q(q), _idx(idx) {}

    It& operator++() {
      ++_idx;
      return *this;
    }

    It operator++(int) {
      auto cpy = *this;
      ++_idx;
      return cpy;
    }

    It& operator+=(size_t n) {
      _idx += n;
      return *this;
    }

    It operator+(size_t n) {
      auto cpy = *this;
      cpy._idx += n;
      return cpy;
    }

    It operator-(size_t n) {
      auto cpy = *this;
      cpy._idx -= n;
      return cpy;
    }

    It& operator-=(size_t n) {
      _idx -= n;
      return *this;
    }

    It operator--(int) {
      auto cpy = *this;
      --_idx;
      return cpy;
    }

    It& operator--() {
      --_idx;
      return *this;
    }

    std::string operator*() {
      return _q->operator[](_idx);
    }

    size_t operator-(It rhs) const {
      return _idx - rhs._idx;
    }

  };
  // obtain a string
  const std::string operator[] (size_t idx) const {
    size_t blockIdx = idx / Blocksize;
    size_t idxInBlock = idx % Blocksize;
    auto block = getBlock(blockIdx);
    const auto& [ptr, size] = block[idxInBlock];
    return {ptr, size};
  }

  It begin() const {return {this, 0};}
  It end() const {return {this, size()};}

  const std::string back() {
    // should we throw on empty?
    return operator[](size() - 1);
  }

  constexpr size_t blockSize() const {return Blocksize;}


  void appendBlock(const std::vector<std::string>& content) {
    AD_CHECK(!_finished);
    AD_CHECK(content.size() <= Blocksize && content.size() > 0);
    if (content.size()< Blocksize) {
      _finished = true;
      LOG(INFO) << "Pushed an incomplete block to the CompressedDeque. This must have been the last block or else we'll throw\n";
    }
    _size += content.size();
    CompactStringVector<size_t, char> compact;
    compact.build(content);
    _data.emplace_back(ZstdCompressor::compress(std::move(compact).moveToBuffer()));
  }

  void clear() {
    _data.clear();
    _size = 0;
  }

  [[nodiscard]] const size_t& size() const {return _size;}

  // only for unit tests of the vocabulary class
  void buildFromVector(const std::vector<std::string>& content) {
    clear();
    auto it = content.begin();
    while (it != content.end()) {
      auto itEnd = it + Blocksize <= content.end() ? it + Blocksize : content.end();
      std::vector<std::string> block{it, itEnd};
      appendBlock(block);
      // process
      it = itEnd;
    }

  }

  // _____________________________________________________
  [[nodiscard]] bool isFinished() const {return _finished;}

private:
  size_t _size; // the actual number of elements
  const size_t& numBlocks() const {return _data.size();}

  CompactStringVector<size_t, char> getBlock(size_t idx) const {
    // todo implement compression
    auto cpy = ZstdCompressor::decompress(_data[idx]);
    return {std::move(cpy)};
  }
  bool _finished = false; // no more write access allowed if true
  vector<ad_utility::ByteBuffer> _data;

};

#endif //QLEVER_COMPRESSEDDEQUE_H
