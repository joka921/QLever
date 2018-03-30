// Copyright 2016, University of Freiburg,
// Chair of Algorithms and Data Structures.
// Authors: Björn Buchhold <buchholb@informatik.uni-freiburg.de>,
//          Patrick Brosi <brosi@informatik.uni-freiburg.de>.

#include <gtest/gtest.h>
#include <vector>
#include <algorithm>
#include <sstream>
#include <locale>
#include <codecvt>

#include "./QGramIndex.h"

using std::string;
using std::vector;
using std::pair;

// Setup the test. Can be used together with TEST_F.
// Instead of reading the strings from file,
// the member is filled here.
class QGramIndexTest: public ::testing::Test {
 public:
  std::wstring_convert<std::codecvt_utf8<wchar_t>> converterTest;
};


// _____________________________________________________________________________
TEST_F(QGramIndexTest, readStringsFromFile) {
  QGramIndex ets(3);
  ets.buildFromFile("example.txt");

  ASSERT_EQ(size_t(23), ets.getIndex().size());
  ASSERT_EQ(size_t(4),
      ets.getIndex().find(converterTest.from_bytes("$fo"))->second.size());
  ASSERT_EQ(size_t(3),
      ets.getIndex().find(converterTest.from_bytes("oot"))->second.size());
  ASSERT_EQ(size_t(2),
      ets.getIndex().find(converterTest.from_bytes("tba"))->second.size());
  ASSERT_EQ(size_t(2),
      ets.getIndex().find(converterTest.from_bytes("l$$"))->second.size());
}

// _____________________________________________________________________________
TEST_F(QGramIndexTest, getQGrams) {
  QGramIndex ets(3);
  auto qgrams = ets.getQGrams(converterTest.from_bytes("lirum"));
  ASSERT_EQ(qgrams.size(), 7);
  ASSERT_EQ(qgrams[0], converterTest.from_bytes("$$l"));
  ASSERT_EQ(qgrams[1], converterTest.from_bytes("$li"));
  ASSERT_EQ(qgrams[2], converterTest.from_bytes("lir"));
  ASSERT_EQ(qgrams[3], converterTest.from_bytes("iru"));
  ASSERT_EQ(qgrams[4], converterTest.from_bytes("rum"));
  ASSERT_EQ(qgrams[5], converterTest.from_bytes("um$"));
  ASSERT_EQ(qgrams[6], converterTest.from_bytes("m$$"));
}

// _____________________________________________________________
TEST_F(QGramIndexTest, checkPrefixEditDistanceTest) {
  QGramIndex ets(3);

  std::wstring foo = converterTest.from_bytes("foo");
  std::wstring foot = converterTest.from_bytes("foot");
  std::wstring fotbal = converterTest.from_bytes("fotbal");
  std::wstring bar = converterTest.from_bytes("bar");
  std::wstring perf = converterTest.from_bytes("perf");
  std::wstring perv = converterTest.from_bytes("perv");
  std::wstring peff = converterTest.from_bytes("peff");
  ASSERT_EQ(0, ets.checkPrefixEditDistance(foo, foo, 0));
  ASSERT_EQ(0, ets.checkPrefixEditDistance(foo, foo, 10));
  ASSERT_EQ(0, ets.checkPrefixEditDistance(foo, foot, 0));
  ASSERT_EQ(1, ets.checkPrefixEditDistance(foot, foo, 1));
  ASSERT_EQ(1, ets.checkPrefixEditDistance(foo, fotbal, 1));
  ASSERT_EQ(3, ets.checkPrefixEditDistance(foo, bar, 3));

  ASSERT_EQ(1, ets.checkPrefixEditDistance(perf, perv, 1));
  ASSERT_EQ(1, ets.checkPrefixEditDistance(perv, perf, 1));
  ASSERT_EQ(1, ets.checkPrefixEditDistance(perf, peff, 1));

  ASSERT_EQ(1, ets.checkPrefixEditDistance(foot, foo, 0));
  ASSERT_EQ(1, ets.checkPrefixEditDistance(foo, fotbal, 0));

  std::wstring tuebi = converterTest.from_bytes("tübi");
  std::wstring tubi = converterTest.from_bytes("tubi");
  std::wstring tobi = converterTest.from_bytes("tøbi");

  ASSERT_EQ(1, ets.checkPrefixEditDistance(tuebi, tubi, 1));
  ASSERT_EQ(1, ets.checkPrefixEditDistance(tuebi, tobi, 1));
  ASSERT_EQ(0, ets.checkPrefixEditDistance(tuebi, tuebi, 0));
  ASSERT_EQ(3, ets.checkPrefixEditDistance(foo, bar, 2));
}

// ____________________________________________________________
TEST_F(QGramIndexTest, computeUnionTest) {
  QGramIndex ets(3);
  vector<unsigned int> v1{1, 4, 6};
  vector<unsigned int> v2{2, 4, 6, 9, 9};
  vector<vector<unsigned int>> toMerge;
  toMerge.push_back(v1);
  toMerge.push_back(v2);
  vector<int> expected{1, 2, 4, 4, 6, 6, 9, 9};
  auto res = ets.computeUnion(toMerge);
  ASSERT_EQ(expected.size(), res.size());
  bool tested = std::equal(res.begin(), res.end(), expected.begin());
  ASSERT_TRUE(tested);
}

// ___________________________________________________________
TEST_F(QGramIndexTest, findMatchesTest) {
  QGramIndex ets(3);
  ets.buildFromFile("example.txt");
  using vecType = std::vector<std::array<size_t, 3>>;

  vecType res = ets.findMatches(converterTest.from_bytes("foot"), 1);
  std::stringstream compStream;
  for (auto& i : res) {
    compStream <<i[0] <<" " <<i[1] <<" " << i[2] << std::endl;
  }
  ASSERT_STREQ(compStream.str().c_str(), "0 0 3\n1 1 1\n2 0 2\n3 0 1\n");

  res = ets.findMatches(converterTest.from_bytes("woob"), 1);
  ASSERT_EQ(res.size(), 1);
  ASSERT_EQ(res[0][0], 1);
  ASSERT_EQ(res[0][1], 1);
  ASSERT_EQ(res[0][2], 1);

  // for Raghu, empty requests will no longer crash anything
  ASSERT_EQ(ets.findMatches(converterTest.from_bytes(""), 2).size(), 0);
}

// ____________________________________________________________
TEST_F(QGramIndexTest, sortResultTest) {
  QGramIndex ets(3);
  using vecType = std::vector<std::array<size_t, 3>>;
  using arrType = std::array<size_t, 3>;
  vecType inVec;
  inVec.emplace_back(arrType{1, 3, 3});
  inVec.emplace_back(arrType{2, 2, 4});
  inVec.emplace_back(arrType{3, 6, 1});
  inVec.emplace_back(arrType{4, 3, 2});
  inVec.emplace_back(arrType{5, 6, 5});

  vecType outVec = ets.sortResult(inVec);
  std::stringstream compStream;
  for (auto& i : outVec) {
    compStream <<i[0] <<" " <<i[1] <<" " << i[2] << std::endl;
  }
  ASSERT_STREQ(compStream.str().c_str(), "2 2 4\n1 3 3\n4 3 2\n5 6 5\n3 6 1\n");
}

// _______________________________________________________________
TEST_F(QGramIndexTest, getJson) {
  QGramIndex ets(3);
  ets.buildFromFile("example.txt");
  std::vector<std::array<size_t, 3>> inVec;
  inVec.emplace_back(std::array<size_t, 3>{1, 3, 3});
  auto res = ets.getJson(inVec, 1);
  std::string comp = "[1, {\"word\": \"foobar\", \"id\":1, \"score\":3}]";
  ASSERT_STREQ(res.c_str(), comp.c_str());
}

// ________________________________________________________________
TEST_F(QGramIndexTest, escapeJson) {
  QGramIndex ets(3);
  ASSERT_STREQ(ets.escapeJson("Hello\\Goodbye").c_str(), "Hello\\\\Goodbye");
  ASSERT_STREQ(ets.escapeJson("\"Hello\"").c_str(), "\\\"Hello\\\"");
}
