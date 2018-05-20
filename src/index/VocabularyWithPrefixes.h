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

  VocabularyWithPrefixes() = default;
  VocabularyWithPrefixes(vector<SparqlPrefix> prefixes) : _prefixes(std::move(prefixes)) {
   for (const auto& p : _prefixes) {
     if (_prefixedVocabulary.find(p._prefix) != _prefixedVocabulary.end()) {
       // TODO: throw proper exception
       std::cerr << "Error, no duplicate or empty prefixes allowd in constructor of VocabularyWithPrefixes. Terminating"
	 << std::endl;
       std::terminate();
     }
     std::cout << "trying to construct" << std::endl;
     _prefixedVocabulary[p._prefix] = std::make_pair(0, Vocabulary());
     std::cout << "neverReaching here" << std::endl;
   }
  }

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

 private:
  void updatePrefixOffsets();
  using vecMap = std::unordered_map<string, std::pair<size_t, Vocabulary>>;

  // key is prefix, value is uri
  vector<SparqlPrefix> _prefixes;
  vecMap _prefixedVocabulary;
  
};
