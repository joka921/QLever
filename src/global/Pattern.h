// Copyright 2018, University of Freiburg,
// Chair of Algorithms and Data Structures.
// Author: Florian Kramer (florian.kramer@mail.uni-freiburg.de)

#pragma once
#include <cstdint>
#include <cstdlib>
#include <cstring>
#include <limits>
#include <stdexcept>
#include <string>
#include <vector>
#include "../util/File.h"
#include "Id.h"
#include "../util/ByteBuffer.h"

typedef uint32_t PatternID;

static const PatternID NO_PATTERN = std::numeric_limits<PatternID>::max();

/**
 * @brief This represents a set of relations of a single entity.
 *        (e.g. a set of books that all have an author and a title).
 *        This information can then be used to efficiently count the relations
 *        that a set of entities has (e.g. for autocompletion of relations
 *        while writing a query).
 */
struct Pattern {
  Id& operator[](const size_t pos) { return _data[pos]; }

  const Id& operator[](const size_t pos) const { return _data[pos]; }

  bool operator==(const Pattern& other) const {
    if (size() != other.size()) {
      return false;
    }
    for (size_t i = 0; i < size(); i++) {
      if (other._data[i] != _data[i]) {
        return false;
      }
    }
    return true;
  }

  bool operator!=(const Pattern& other) const {
    if (size() != other.size()) {
      return true;
    }
    for (size_t i = 0; i < size(); i++) {
      if (other._data[i] != _data[i]) {
        return true;
      }
    }
    return false;
  }

  bool operator<(const Pattern& other) const {
    if (size() == 0) {
      return true;
    }
    if (other.size() == 0) {
      return false;
    }
    return _data[0] < other._data[0];
  }

  bool operator>(const Pattern& other) const {
    if (other.size() == 0) {
      return true;
    }
    if (size() == 0) {
      return false;
    }
    return _data[0] > other._data[0];
  }

  size_t size() const { return _data.size(); }

  void push_back(const Id i) { _data.push_back(i); }

  void clear() { _data.clear(); }

  std::vector<Id> _data;
};

// The type of the index used to access the data, and the type of the data
// stored in the strings.
template <typename IndexT, typename DataT>
/**
 * @brief Stores a list of variable length data of a single type (e.g.
 *        c-style strings). The data is stored in a single contiguous block
 *        of memory.
 */
class CompactStringVector {
 public:
  CompactStringVector()
      : _size(0), _indexEnd(0), _dataSize(0) {}

  CompactStringVector(const std::vector<std::vector<DataT>>& data) {
    build(data);
  }

  CompactStringVector(ad_utility::File& file, off_t offset = 0) {
    load(file, offset);
  }

  virtual ~CompactStringVector() {}

  /**
   * @brief Fills this CompactStringVector with data.
   * @param The data from which to build the vector.
   */
   template <typename NestedContainer>
  void build(const NestedContainer& input) {
    _size = input.size();
    _indexEnd = (_size + 1) * sizeof(IndexT);
    size_t dataCount = 0;
    for (size_t i = 0; i < _size; i++) {
      dataCount += input[i].size();
    }
    if (dataCount > std::numeric_limits<IndexT>::max()) {
      throw std::runtime_error(
          "To much data for index type. (" + std::to_string(dataCount) + " > " +
          std::to_string(std::numeric_limits<IndexT>::max()));
    }
    _dataSize = _indexEnd + sizeof(DataT) * dataCount;
    resize(_dataSize);
    IndexT currentLength = 0;
    size_t indPos = 0;
    for (IndexT i = 0; i < _size; i++) {
      // add an entry to the index
      std::memcpy(data() + (indPos * sizeof(IndexT)), &currentLength,
                  sizeof(IndexT));
      // copy the vectors actual data
      std::memcpy(data() + (_indexEnd + currentLength * sizeof(DataT)),
                  input[i].data(), input[i].size() * sizeof(DataT));
      indPos++;
      currentLength += input[i].size();
    }
    // add a final entry that stores the end of the data field
    std::memcpy(data() + (indPos * sizeof(IndexT)), &currentLength,
                sizeof(IndexT));
  }

  void load(ad_utility::File& file, off_t offset = 0) {
    file.read(&_size, sizeof(size_t), offset);
    file.read(&_dataSize, sizeof(size_t), offset + sizeof(size_t));
    _indexEnd = (_size + 1) * sizeof(IndexT);
    resize(_dataSize);
    file.read(data(), _dataSize, offset + 2 * sizeof(size_t));
  }

  CompactStringVector& operator=(const CompactStringVector&) = delete;

  size_t size() const { return _size; }

  /**
   * @brief Stores the vector in the file at the current seek position.
   * @param The file to write into
   * @return The number of bytes written.
   */
  size_t write(ad_utility::File& file) {
    file.write(&_size, sizeof(size_t));
    file.write(&_dataSize, sizeof(size_t));
    file.write(data(), _dataSize);
    return _dataSize + 2 * sizeof(size_t);
  }
  /**
   * @brief return a serialized representation, making this element invalid
   * @return
   */
   ad_utility::ByteBuffer moveToBuffer() && {
     assert(_data.size() == _dataSize + sizeof(_size) + sizeof(_dataSize));
     std::memcpy(data() + _dataSize, &_size, sizeof(_size));
    std::memcpy(data() + _dataSize + sizeof(_size), &_dataSize, sizeof(_dataSize));
    return std::move(_data);
  }

  /// Construct from a ByteBuffer that war created by a call to moveToBuffer
  CompactStringVector(ad_utility::ByteBuffer&& buf) : _data(std::move(buf)) {
     _dataSize = _data.size() - controlBlockSize();
    std::memcpy(&_size, data() + _dataSize, sizeof(_size));
    std::memcpy(&_dataSize, data() + _dataSize + sizeof(_size), sizeof(_dataSize));
    AD_CHECK(_dataSize == _data.size() - controlBlockSize());
    _indexEnd = (_size + 1) * sizeof(IndexT);
  }


  bool ready() const { return data() != nullptr; }

  /**
   * @brief operator []
   * @param i
   * @return A std::pair containing a pointer to the data, and the number of
   *         elements stored at the pointers target.
   */
  const std::pair<const DataT*, size_t> operator[](size_t i) const {
    IndexT ind, nextInd;
    std::memcpy(&ind, data() + (i * sizeof(IndexT)), sizeof(IndexT));
    std::memcpy(&nextInd, data() + ((i + 1) * sizeof(IndexT)), sizeof(IndexT));
    return {
        reinterpret_cast<const DataT*>(data() + (_indexEnd + sizeof(DataT) * ind)),
        nextInd - ind};
  }

 private:
  ad_utility::ByteBuffer _data;
  uint8_t* data() {return _data.data();}
  const uint8_t* data() const {return _data.data();}
  void resize(size_t n) {_data.resize(n + controlBlockSize());}
  size_t _size;
  size_t _indexEnd;
  size_t _dataSize;
  constexpr size_t controlBlockSize() const {return sizeof(_size) + sizeof(_dataSize);}
};

namespace std {
template <>
struct hash<Pattern> {
  std::size_t operator()(const Pattern& p) const {
    std::string_view s = std::string_view(
        reinterpret_cast<const char*>(p._data.data()), sizeof(Id) * p.size());
    return hash<std::string_view>()(s);
  }
};
}  // namespace std

inline std::ostream& operator<<(std::ostream& o, const Pattern& p) {
  for (size_t i = 0; i + 1 < p.size(); i++) {
    o << p[i] << ", ";
  }
  if (p.size() > 0) {
    o << p[p.size() - 1];
  }
  return o;
}
