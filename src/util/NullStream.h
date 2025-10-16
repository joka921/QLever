// Copyright 2025 The QLever Authors, in particular:
//
// 2025 Johannes Kalmbach <kalmbach@cs.uni-freiburg.de>, UFR
//
// UFR = University of Freiburg, Chair of Algorithms and Data Structures

#pragma once

#include <ostream>
#include <streambuf>

namespace ad_utility {

// Null output stream that discards all input
class NullStream : public std::ostream {
 private:
  class NullBuffer : public std::streambuf {
   public:
    int overflow(int c) override { return c; }
  };
  NullBuffer buffer_;

 public:
  NullStream() : std::ostream(&buffer_) {}
};

}  // namespace ad_utility
