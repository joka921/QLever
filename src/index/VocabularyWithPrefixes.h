// Copyright 2018, University of Freiburg,
// Chair of Algorithms and Data Structures.
// Author: Johannes Kalmbach <johannes.kalmbach@gmail.com>

#pragma once

#include <string>
#include <vector>
#include <unordered_map>
#include <iostream>

#include "../parser/ParsedQuery.h"
#include "Vocabulary.h"

using std::string;
using std::vector;

class VocabularyWithPrefixes {
 public:
  string removeAndGetPrefix(string& item) const;

  VocabularyWithPrefixes() {
   _prefixedVocabulary[""] = std::make_pair(0, Vocabulary()); }

  VocabularyWithPrefixes(vector<SparqlPrefix> prefixes)  {
    PreparePrefixes(std::move(prefixes));

  }

  void PreparePrefixes(vector<SparqlPrefix>&& prefixes);

  void readFromPrefixedFile(const string& filename);

  void readFromFile(const string& fileName, const string& extLitsFileName);

  void writeToFile(const string& fileName) const;

  size_t size() {return _size};

  // _________________________________________
  bool getId(const string& word, Id* id) const;

  // for testing: entries must be sorted
  // not actually efficient
  void push_back(const string& word) {
    auto wordCopy = word;
    auto prefix = removeAndGetPrefix(wordCopy);
    _prefixedVocabulary[prefix].second.push_back(wordCopy);
    updatePrefixOffsets();
  }

  // ___________________________________________________
  void externalizeLiterals(const string& fileName) {
    _prefixedVocabulary[""].second.externalizeLiterals(fileName);
  }

 private:
  std::pair<string, string> splitPrefixed(const string& word);
  void updatePrefixOffsets();
  using vecMap = std::unordered_map<string, std::pair<size_t, Vocabulary>>;

  // key is prefix, value is uri
  vector<SparqlPrefix> _prefixes;
  vecMap _prefixedVocabulary;
  size_t _size;
  
};
