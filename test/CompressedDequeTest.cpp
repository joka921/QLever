// Copyright 2020, University of Freiburg,
// Chair of Algorithms and Data Structures.
// Author: Johannes Kalmbach (johannes.kalmbach@gmail.com)

#include <gtest/gtest.h>
#include "../src/util/CompressedDeque.h"

using namespace std::literals;

TEST(CompressedDequeTest, Simple) {
  CompressedQueue<2> c;
  ASSERT_FALSE(c.isFinished());
  std::vector<std::string> block{"alpha", "beta"};
  c.appendBlock(block);
  ASSERT_FALSE(c.isFinished());
  block[0] = "gamma";
  block[1] = "delta";
  c.appendBlock(block);
  ASSERT_FALSE(c.isFinished());
  block.resize(1);
  block[0] = "last";
  c.appendBlock(block);
  ASSERT_TRUE(c.isFinished());

  ASSERT_EQ(c[0], "alpha"s);
  ASSERT_EQ(c[1], "beta"s);
  ASSERT_EQ(c[2], "gamma"s);
  ASSERT_EQ(c[3], "delta"s);
  ASSERT_EQ(c[4], "last"s);

}


