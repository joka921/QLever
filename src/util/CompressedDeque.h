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

using std::vector;

template <size_t Blocksize>
class CompressedQueue {
public:
  // obtain a string
  std::string operator[] (size_t idx) {
    size_t blockIdx = idx / Blocksize;
    size_t idxInBlock = idx % Blocksize;
    auto block = getBlock(blockIdx);
    const auto& [ptr, size] = block[idxInBlock];
    return {ptr, size};
  }

  /*
  void appendBlock(const std::array<std::string, Blocksize>& content) {
    AD_CHECK(!_finished);
    auto totalBytes = std::accumulate(content.begin(), content.end(), 0, [](const auto& a, const auto& b){return a + b.size();});
    CompactStringVector<size_t, char> compact;
    compact.build(content);
    _data.emplace_back(std::move(compact).moveToBuffer());
  }
   */

  void appendBlock(const std::vector<std::string>& content) {
    AD_CHECK(!_finished);
    AD_CHECK(content.size() <= Blocksize && content.size() > 0);
    if (content.size()< Blocksize) {
      _finished = true;
      LOG(INFO) << "Pushed an incomplete block to the CompressedDeque. This must have been the last block or else we'll throw\n";
    }
    CompactStringVector<size_t, char> compact;
    compact.build(content);
    _data.emplace_back(std::move(compact).moveToBuffer());
  }

  // _____________________________________________________
  [[nodiscard]] bool isFinished() const {return _finished;}

private:
  size_t _size; // the actual number of elements
  const size_t& numBlocks() const {return _data.size();}

  CompactStringVector<size_t, char> getBlock(size_t idx) {
    // todo implement compression
    auto cpy = _data[idx];
    return {std::move(cpy)};
  }
  bool _finished = false; // no more write access allowed if true
  vector<ad_utility::ByteBuffer> _data;

};

#endif //QLEVER_COMPRESSEDDEQUE_H
