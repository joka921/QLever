// Copyright 2016, University of Freiburg,
// Chair of Algorithms and Data Structures.
// Authors: Björn Buchhold <buchholb@informatik.uni-freiburg.de>,
//          Patrick Brosi <brosi@informatik.uni-freiburg.de>.

#ifndef QGRAMINDEX_H_
#define QGRAMINDEX_H_

#include <gtest/gtest.h>
#include <unordered_map>
#include <string>
#include <vector>
#include <array>

using std::string;
using std::vector;
using std::pair;
using std::wstring;

struct Result {
  size_t wordId;
  size_t p;
  size_t s;
  Result(size_t wordId, size_t p, size_t s) : wordId(wordId), p(p), s(s) {}
};

inline bool resComp(const Result& lh, const Result& rh) {
  if (lh.p < rh.p) return true;
  if (lh.p == rh.p) return lh.s > rh.s;

  return false;
}

typedef std::pair<size_t, size_t> Tuple;
typedef std::vector<Tuple> TupleList;
typedef std::vector<Result> ResultList;
typedef std::vector<uint32_t> InvList;
typedef std::unordered_map<wstring, InvList> Index;

// Class that realizes error-tolerant prefix search.
class QGramIndex {
 public:
  friend class QGramIndexTest;
  FRIEND_TEST(QGramIndexTest, readStringsFromFile);

  explicit QGramIndex(size_t q) : _q(q) {}

  // Read input strings from file and build index
  void buildFromFile(const string& fileName);

  // sort a list of (word, ped, score) triples, primary key is ped (ascending),
  // secondary key is score (ascending)
  static vector<std::array<size_t, 3>> sortResult(
      vector<std::array<size_t, 3>> res);

  // get all the arrays {wordId, score, Ped} where PED(prefix, word) <= delta
  vector<std::array<size_t, 3>> findMatches(
         const wstring& prefix, int delta) const;

  // Normalize a string (do this before calculating the q grams)
  static wstring normalizeString(const wstring& str);

  vector<wstring> getQGrams(const wstring& word, bool PaddingEnd = true) const;

  // Return the index
  const Index& getIndex() const { return _qGramIndex; }

  // Get the raw (as parsed from the file) word for a given wordId
  const wstring& getWord(uint32_t wordId) const { return _words[wordId]; }

  // Convert an array of matches to a JSON string. Include at mos
  // maxDisplay matches
  string getJson(
      const vector<std::array<size_t, 3>>& matches, size_t maxDisplay) const;

  // Get the coordinates of a given word_id as google maps compatible json
  string getCoordinates(uint32_t wordId) const;



 private:
  // check if PED of (prefix, string) is less or equal than delta.
  // Return PED if that is the case, delta + 1 otherwise
  FRIEND_TEST(QGramIndexTest, checkPrefixEditDistanceTest);
  int checkPrefixEditDistance(
      const wstring& prefix, const wstring& compString, int delta) const;

  // compute the union of given q-gram list (result is sorted, and multiple
  // appearances of word id will result in multiple vector entries
  vector<int> computeUnion(vector<vector<unsigned int>> lists) const;
  FRIEND_TEST(QGramIndexTest, computeUnionTest);

  // escape Json strings ( I have put it here because the json method was here)
  string escapeJson(const string& word) const;
  FRIEND_TEST(QGramIndexTest, escapeJson);

  // The strings and their scores.
  vector<size_t> _scores;

  // The inverted lists of the k-gram index. For each k-gram that occurs in one
  // of the input strings, contains the list of ids of all input strings
  // containing that k-gram.
  Index _qGramIndex;

  // the raw words input
  vector<std::wstring> _words;
  // the latitude values of words
  vector<double> _lat;
  // longitude values of words
  vector<double> _long;

  // the total number of words
  size_t _numWords;

  size_t _q;

  // converter for utf8 string to wstring conversion
  mutable std::wstring_convert<std::codecvt_utf8<wchar_t>> _converter;
};

#endif  // QGRAMINDEX_H_
