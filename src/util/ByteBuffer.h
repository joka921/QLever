//
// Created by johannes on 17.01.20.
//

#ifndef QLEVER_BYTEBUFFER_H
#define QLEVER_BYTEBUFFER_H

#include <memory>

namespace ad_utility {
/**
 * @brief simple byte buffer that allows resizing and does not
 * initialize any values
 */
class ByteBuffer {
public:
  using It = uint8_t*;
  using ConstIt = const uint8_t *;
  // _______________________________________
  ByteBuffer() = default;
  ByteBuffer(ByteBuffer&&) = default;

  ByteBuffer(const uint8_t* arr, size_t sz) {
    resize(sz);
    std::memcpy(data(), arr, sz);
  }

  // __________________________________________
  ByteBuffer(const ByteBuffer& other) {
    resize(other.size());
    std::memcpy(data(), other.data(), size());
  }

  ByteBuffer& operator=(const ByteBuffer& other) {
    resize(other.size());
    std::memcpy(data(), other.data(), size());
    return *this;
  }

  // ________________________________________
  ByteBuffer(size_t size) {
    resize(size);
  }

  // ______________________________________
  void resize(size_t n) {
    _data.reset(new uint8_t[n]);
    _size = n;
  }

  // ______________________________________
  const size_t& size() const {return _size;}

  // ________________________________________
  It data() {return _data.get();}
  ConstIt data() const {return _data.get();}


private:
  size_t _size = 0;
  std::unique_ptr<uint8_t[]> _data;

};
}

#endif //QLEVER_BYTEBUFFER_H
