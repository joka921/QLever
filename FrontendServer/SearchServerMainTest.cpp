// Copyright 2016 Johannes Kalmbach
// <johannes.kalmbach@gmail.com
//
// Test for helper functions of Search server in utils.h


#include <string>

#include <gtest/gtest.h>
#include "./utils.h"

// ________________________________________________________
TEST(SearchServerUtils, detectContentType) {
  std::string filename = "test.txt";
  auto res = detectContentType(filename);
  ASSERT_TRUE(res.first);
  ASSERT_STREQ(res.second.c_str(), "text/plain");

  filename = "text.js";
  res = detectContentType(filename);
  ASSERT_TRUE(res.first);
  ASSERT_STREQ(res.second.c_str(), "application/javascript");

  filename = "invalid.exe";
  res = detectContentType(filename);
  ASSERT_FALSE(res.first);

  filename = "nodot";
  res = detectContentType(filename);
  ASSERT_FALSE(res.first);
}

// ___________________________________________________________
TEST(SearchServerUtils, readFile) {
  std::string filename = "noexist.txt";
  auto res = readFile(filename);
  ASSERT_FALSE(res.first);

  filename = "example.txt";
  res = readFile(filename);
  ASSERT_TRUE(res.first);
  std::string comp = "Football\t3\t4.0\t4.0\nfoobar\t1\t4.0\t4.0\n";
  comp.append("Footsal\t2\t4.0\t4.0\nFoot Barca\t1\t4.0\t4.0\n");

  ASSERT_STREQ(res.second.c_str(), comp.c_str());
}

TEST(SearchServerUtils, decodeURL) {
  ASSERT_STREQ(decodeURL("z%C3%BCrich").c_str(), "zürich");
  ASSERT_STREQ(decodeURL("L%C3%B8kken").c_str(), "Løkken");
  ASSERT_STREQ(decodeURL("a+o").c_str(), "a o");
  ASSERT_STREQ(decodeURL("%C3%A1+%C3%A9").c_str(), "á é");
}
