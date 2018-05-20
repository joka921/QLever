// Copyright 2018, University of Freiburg,
// Chair of Algorithms and Data Structures.
// Author: Johannes Kalmbach <johannes.kalmbach@gmail.com>

#include "./VocabularyWithPrefixes.h"

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
