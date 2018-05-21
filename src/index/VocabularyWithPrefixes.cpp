// Copyright 2018, University of Freiburg,
// Chair of Algorithms and Data Structures.
// Author: Johannes Kalmbach <johannes.kalmbach@gmail.com>

#include "./VocabularyWithPrefixes.h"

#include <fstream>

#include "../util/StringUtils.h"

string VocabularyWithPrefixes::removeAndGetPrefix(string& item) const {
  // default case, no entity
  if (!ad_utility::startsWith(item, "<")) {
    return std::string();
  }

  if (item[item.size() - 1] != '>') {
    // TODO: throw or report error;
    return std::string();
  }

  // better use string_view
  // strip the beginning and terminating <>
  auto relevantSubstr = item.substr(1, item.size() - 2);
  
  for (const auto& p : _prefixes) {
    if (ad_utility::startsWith(relevantSubstr, p._uri)) {
      item = relevantSubstr.substr(p._uri.size(), item.size() -  p._uri.size());
      return p._prefix;
    }
  }
  return std::string();
}

// ___________________________________________________________
void VocabularyWithPrefixes::updatePrefixOffsets() {
  size_t curOffset = 0;
  for (auto& p : _prefixes) {
    _prefixedVocabulary[p._prefix].first = curOffset;
    curOffset += _prefixedVocabulary[p._prefix].second.size();
  }
}

// ______________________________________________________________
bool VocabularyWithPrefixes::getId(const string& word, Id* id) const {
  // copy needed for constness of input parameter
  auto wordCopy = word;
  auto prefix = removeAndGetPrefix(wordCopy);
  const auto& p = *(_prefixedVocabulary.find(prefix));
  bool success =  p.second.second.getId(wordCopy, id);
  if (success) {
    auto offset = p.second.first;
    *id += offset;
  }
  return success;
}

// _______________________________________________________________
void VocabularyWithPrefixes::readFromPrefixedFile(const string& filename) {
  for (auto& p : _prefixedVocabulary) {
    // clear all Vocabulary
    p.second.second.clear();
  }
  std::ifstream file(filename);
  string line;
  size_t count = 0;
  while (std::getline(file, line)) {
    string prefix;
    string remainder;
    std::tie(prefix, remainder) = splitPrefixed(line);
    _prefixedVocabulary[prefix].second.push_back(remainder);
    count++;
    if (count % 10000000 == 0) {
      LOG(INFO) << "inserted " << count << "words to splitVocabulary\n";
    }
  }
  updatePrefixOffsets();
}

// _____________________________________________________________________
void VocabularyWithPrefixes::writeToFile(const string& fileName) const {
  LOG(INFO) << "Writing vocabulary to file " << fileName << "\n";
  for (const auto& p : _prefixedVocabulary) {
    p.second.second.writeToFile(fileName + p.first);
  }
  LOG(INFO) << "Completely done writing vocabulary to all prefix files.\n";
  // write file containing prefix information
  std::ofstream prefixFile(fileName + "#prefixes");
  for (const auto& pref : _prefixes) {
    prefixFile << pref._prefix << '\t' << pref._uri << 'n';
  }
}

// _____________________________________________________________________
void VocabularyWithPrefixes::readFromFile(const string& fileName,
                              const string& extLitsFileName) {
  std::ifstream prefixFile(fileName + "#prefixes");
  string line;
  vector<SparqlPrefix> prefixes;
  while (std::getline(prefixFile, line)) {
    auto pos = line.find('\t');
    prefixes.emplace_back(line.substr(0, pos), line.substr(pos + 1));
  }
  PreparePrefixes(std::move(prefixes));

  for (auto& p : _prefixedVocabulary) {
    p.second.second.readFromFile(fileName + p.first);
  }

}

// _______________________________________________________________
std::pair<string, string> VocabularyWithPrefixes::splitPrefixed(const string& word) {
  if (!word.size() || word[0] == '"' || word[0] == '<') {
    // handle cases empty, literal and unprefixed entity
    return std::make_pair(std::string(), word);
  }
  auto pos = word.find(":");
  // TODO : no sanity check because of only internal usage!
  // Actually should check for npos
  return std::make_pair(word.substr(0, pos), word.substr(pos + 1));
}

// _____________________________________________________________________
void VocabularyWithPrefixes::PreparePrefixes(vector<SparqlPrefix>&& prefixes) {
  _prefixes = std::move(prefixes);
  _prefixedVocabulary.clear();

  _prefixedVocabulary[""] = std::make_pair(0, Vocabulary());
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
