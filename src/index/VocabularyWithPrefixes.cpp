// Copyright 2018, University of Freiburg,
// Chair of Algorithms and Data Structures.
// Author: Johannes Kalmbach <johannes.kalmbach@gmail.com>

#include "./VocabularyWithPrefixes.h"

#include <fstream>

#include "../util/StringUtils.h"

string VocabularyWithPrefixes::removeAndGetPrefix(string& item) const {
  // default case, no entity
  if (!ad_utility::startsWith(item, "<")) {
    if (ad_utility::startsWith(item, string({EXTERNALIZED_LITERALS_PREFIX}))) {
      return string({EXTERNALIZED_LITERALS_PREFIX});
    } else {
      return std::string();
    }
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
  // "" is not in _prefixes but has to come first
  _prefixedVocabulary[""].first = 0;
  size_t curOffset = _prefixedVocabulary[""].second.size();
  for (auto& p : _prefixes) {
    _prefixedVocabulary[p._prefix].first = curOffset;
    curOffset += _prefixedVocabulary[p._prefix].second.size();
  }
}

// ______________________________________________________________
bool VocabularyWithPrefixes::getId(const string& word, Id* id) const {
  LOG(INFO) << "getting ID for " << word << '\n';
  // copy needed for constness of input parameter
  auto wordCopy = word;
  if (word[0] == '\"' && _prefixedVocabulary.at("").second.shouldBeExternalized(word)) {
    const auto& p = *(_prefixedVocabulary.find(string({EXTERNALIZED_LITERALS_PREFIX})));
    bool success =  p.second.second.getId(wordCopy, id);
    if (success) {
      auto offset = p.second.first;
      *id += offset;
    }
    return success;
  }
    
  auto prefix = removeAndGetPrefix(wordCopy);
  const auto& p = *(_prefixedVocabulary.find(prefix));
  bool success =  p.second.second.getId(wordCopy, id);
  LOG(INFO) << "Id is " << *id << '\n';
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
  LOG(INFO) << _prefixedVocabulary.size() << std::endl;
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
  auto prefix = word.substr(0, pos);

  // filter out blank nodes
  if (prefix != "_") {
    return std::make_pair(word.substr(0, pos), word.substr(pos + 1));
  } else {
    return std::make_pair(std::string(), word);
  }
}

// _____________________________________________________________________
void VocabularyWithPrefixes::PreparePrefixes(vector<SparqlPrefix>&& prefixes) {
  _prefixes = std::move(prefixes);
  _prefixedVocabulary.clear();

  _prefixedVocabulary[""] = std::make_pair(0, Vocabulary());
  std::string extPref(1, EXTERNALIZED_LITERALS_PREFIX);
  _prefixedVocabulary[extPref] = std::make_pair(0, Vocabulary());
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
  _prefixes.emplace_back(std::string(extPref), std::string(extPref));
  auto pred = [](const auto& p1, const auto& p2) {return p1._prefix < p2._prefix;};

  std::sort(_prefixes.begin(), _prefixes.end(), pred);
}

// ___________________________________________________________________________
string VocabularyWithPrefixes::operator[](Id id) const {
  if (id == ID_NO_VALUE) {
    return "";
  } else {
    size_t offset = 0;
    auto nxt = _prefixes.cbegin();
    auto crOffset = offset + _prefixedVocabulary.at((*nxt)._prefix).first;
    if (id < crOffset) {
      return _prefixedVocabulary.at("").second[id];
    }
    offset = crOffset;
    for (auto it = _prefixes.cbegin(); it != _prefixes.cend(); ++it) {
      auto next = it + 1;
      if (next != _prefixes.end()) {
	auto curOffset =  _prefixedVocabulary.at((*next)._prefix).first;
	if (id < curOffset) {
	  return "<" + (*it)._uri + _prefixedVocabulary.at((*it)._prefix).second[id - offset] + ">";
	}
	offset = curOffset;
      } else { // this means we have an external literal
	//TODO implement external
	id = id - offset;
	const auto& ext = _prefixedVocabulary.at((*it)._prefix).second.getExternalVocab();
	AD_CHECK(id < ext.size());
	return ext[id];
      }
    }

  }
}

// ___________________________________________________--
void VocabularyWithPrefixes::outputForDebugging() {
  size_t i = 0;
  std::ofstream f("debuggin.txt");
  while(true) {
    Id id;
    getId((*this)[i], &id);
    f << (*this)[i] << " " << i << " " << id << std::endl;
    i++;
  }
}
