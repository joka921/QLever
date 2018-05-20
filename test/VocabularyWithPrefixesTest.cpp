// Copyright 2018, University of Freiburg,
// Chair of Algorithms and Data Structures.
// Author: Johannes Kalmbach <johannes.kalmbach@gmail.com>
//
#include <gtest/gtest.h>

#include <vector>
#include <string>
#include <iostream>

#include "../src/index/VocabularyWithPrefixes.h"
#include "../src/parser/ParsedQuery.h"

class VocabularyWithPrefixesTest : public ::testing::Test {
 public:
  std::vector<SparqlPrefix> p;
  VocabularyWithPrefixes v;
  virtual void SetUp() {
    std::cout << "within SetUp:" << std::endl;
    p.emplace_back("t1", "www.test.de/");
    p.emplace_back("tn", "www.testn.de/");
    std::cout << "before const:" << std::endl;
    v = VocabularyWithPrefixes(p);
    v.push_back("<noprefix>");
    v.push_back("<www.test.de/test1>");
    v.push_back("<www.testn.de/test2>");
    v.push_back("<www.testn.de/test3>");
    std::cout << "leaving" << std::endl;
  }
};

TEST_F(VocabularyWithPrefixesTest, removeAndGetPrefix) {
  std::string item("<noprefix>");
  auto res = v.removeAndGetPrefix(item);
  ASSERT_EQ(res, std::string());
  ASSERT_EQ(item, std::string("<noprefix>"));

  item = "<www.test.de/mytest>";
  res = v.removeAndGetPrefix(item);
  ASSERT_EQ(res, std::string("t1"));
  ASSERT_EQ(item, std::string("mytest"));

}


TEST_F(VocabularyWithPrefixesTest, getId) {
  Id id;
  ASSERT_TRUE(v.getId("<noprefix>", &id));
  ASSERT_EQ(id, Id(0));
}
