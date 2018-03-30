// Copyright 2016, University of Freiburg,
// Chair of Algorithms and Data Structures.
// Authors: Björn Buchhold <buchholb@informatik.uni-freiburg.de>,
//          Patrick Brosi <brosi@informatik.uni-freiburg.de>.
//          Johannes Kalmbach <johannes.kalmbach@gmail.com>

#include <string>
#include <vector>
#include <fstream>
#include <algorithm>
#include <array>
#include <locale>
#include <codecvt>

#include "./QGramIndex.h"

using std::string;
using std::vector;
using std::pair;

// _____________________________________________________________________________
void QGramIndex::buildFromFile(const string& fileName) {
  // unmodified from template
  std::ifstream in(fileName.c_str(), std::ios_base::in);
  string line;

  if (!in.is_open()) {
    std::cout << "Input file could not be opened, exiting" << std::endl;
    exit(1);
  }

  _qGramIndex.clear();

  while (std::getline(in, line)) {
    size_t posOfTab = line.find('\t');
    string word = line.substr(0, posOfTab);
    auto wordWide = _converter.from_bytes(word.c_str());
    _words.push_back(wordWide);
    _scores.push_back(atoi(line.substr(posOfTab + 1).c_str()));
    vector<wstring> qGrams = getQGrams(wordWide);

    for (const auto& qGram : qGrams) {
      _qGramIndex[qGram].push_back(_words.size() - 1);
    }

    // get next tab for latitude values
    posOfTab = line.find('\t', posOfTab + 1);
    std::string::size_type sz;
    _lat.push_back(std::stod(line.substr(posOfTab + 1), &sz));
    _long.push_back(std::stod(line.substr(posOfTab + 1 + sz)));
  }
  _numWords = _words.size();
}

// _____________________________________________________________________________
vector<wstring> QGramIndex::getQGrams(
          const wstring& word, bool PaddingEnd) const {
  vector<wstring> ret;

  wstring s;
  for (size_t i = 0; i < _q - 1; ++i) { s += '$'; }

  s += normalizeString(word);

  // no padding on the right for fuzzy search
  if (PaddingEnd) {
    for (size_t i = 0; i < _q - 1; ++i) { s += '$'; }
  }

  for (size_t i = 0; i < s.size() - _q + 1; ++i) {
    ret.push_back(s.substr(i, _q));
  }

  return ret;
}

// _____________________________________________________________________________
wstring QGramIndex::normalizeString(const wstring& str) {
  wstring s;
  for (size_t i = 0; i < str.size(); ++i) {
    if (!std::iswalnum(str[i])) {
      continue;
    }
    s += static_cast<char>(towlower(str[i]));
  }

  return s;
}
// __________________________________________________________________
int QGramIndex::checkPrefixEditDistance(
    const wstring& prefix, const wstring& compString, int delta)
            const {
  std::vector<std::vector<int>> pedTable;

  // if the compString is too small, we get in trouble, so let's limit
  // delta
  unsigned int numCols = delta + prefix.length() + 1;
  if (compString.length() + 1 < numCols) numCols = compString.length() + 1;
  pedTable.resize(prefix.length() + 1);  // additional row for epsilon
  for (unsigned int i = 0; i < prefix.length() + 1; i++) {
    pedTable[i].resize(numCols);
  }

  // fill first row
  for (unsigned int i = 0; i < numCols; i++) {
    pedTable[0][i] = i;
  }

  // fill first col
  for (unsigned int i = 0; i < prefix.length() + 1; i++) {
    pedTable[i][0] = i;
  }

  // fill table row by row
  for (unsigned int i = 1; i < prefix.length() + 1; i++) {
    for (unsigned int j = 1; j < numCols; j++) {
      int diag = pedTable[i - 1][j - 1];
      // col 1 represents char at pos 0 of string, same for prefix
      if (prefix[i - 1] != compString[j - 1]) diag++;
      pedTable[i][j] = std::min(
          {diag, pedTable[i][j - 1] + 1, pedTable[i - 1][j] + 1});
    }
  }
  int minVal = *std::min_element(
      pedTable[prefix.length()].begin(), pedTable[prefix.length()].end());
  if (minVal > delta) minVal = delta + 1;
  return minVal;
}

// _____________________________________________________________
vector<int> QGramIndex::computeUnion(vector<vector<unsigned int>> lists)const {
  vector<int> res;
  for (auto& vec : lists) {
    for (auto& el : vec) {
      res.push_back(el);
    }
  }
  std::sort(res.begin(), res.end());
  return res;
}

// _____________________________________________________________
vector<std::array<size_t, 3>> QGramIndex::findMatches(
        const wstring& prefix, int delta) const {
  // no padding at the end for fuzzy search
  vector<wstring> qGrams = getQGrams(prefix, false);
  vector<vector<unsigned int>> idVec;
  for (const auto& qg : qGrams) {
    const auto it = _qGramIndex.find(qg);
    if (it != _qGramIndex.end()) {
      idVec.push_back(it->second);
    }
  }
  vector<std::array<size_t, 3>> resVec;
  auto lenPr = prefix.length();
  auto unionized = computeUnion(idVec);
  int idxMin = 0;
  int idxMax = 0;
  int unionSize = unionized.size();
  auto itStart = unionized.begin();
  auto itEnd = unionized.end();
  size_t pedCnt = 0;

  // check for empty union to avoid segfault
  if (!unionized.size()) return resVec;

  while (true) {
    // how many times does the same entry id appear in the
    // unionized list? This is
    // the number of qgrams that entry has in common with the prefix
    idxMax = std::upper_bound(
            itStart + idxMin, itEnd, unionized[idxMin]) - itStart;

    // how many qrams do they need to have in common
    std::wstring curWord = _words[unionized[idxMin]];
    int minCom = lenPr - _q * delta;

    // only check PED for Relevant entries
    if (idxMax - idxMin >= minCom) {
      size_t ped = checkPrefixEditDistance(
                        prefix, normalizeString(curWord), delta);
      pedCnt++;
      if (ped <= static_cast<size_t>(delta)) {
        resVec.push_back(std::array<size_t, 3>
                        {static_cast<size_t>(unionized[idxMin]),
                        ped, _scores[unionized[idxMin]]});
      }
    }

    if (idxMax == unionSize) break;
    // jump to the next entry in union list
    idxMin = idxMax;
  }
  std::cout << pedCnt << " PED values were computed" << std::endl;
  return resVec;
}

// ________________________________________________________________
vector<std::array<size_t, 3>> QGramIndex::sortResult
        (vector<std::array<size_t, 3>> res) {
  auto sortPed = [](std::array<size_t, 3> a, std::array<size_t, 3> b)
        { return a[1] < b[1];};
  auto sortScore = [](std::array<size_t, 3> a, std::array<size_t, 3> b)
        { return a[2] > b[2];};

  // first sort for the scores and then for the PED. Second search
  // is stable, so entries with the same PED will be in order of their
  // scores
  std::sort(res.begin(), res.end(), sortScore);
  std::stable_sort(res.begin(), res.end(), sortPed);
  return res;
  }

// _______________________________________________________________

string QGramIndex::escapeJson(const string& wordNarrow) const {
  string output = wordNarrow;
  // escape "\"
  auto illegalPos = output.find("\\");
  while (illegalPos != output.npos) {
    output.insert(illegalPos, "\\");
    illegalPos = output.find("\\", illegalPos + 2);
  }

  // escape '"'
  illegalPos = output.find("\"");
  while (illegalPos != output.npos) {
    output.insert(illegalPos, "\\");
    illegalPos = output.find("\"", illegalPos + 2);
  }

  return output;
}

// ___________________________________________________________________________
string QGramIndex::getJson(
    const vector<std::array<size_t, 3>>& matches, size_t maxDisplay) const {

  maxDisplay = matches.size() < maxDisplay ? matches.size() : maxDisplay;
  string contentString = "[";
  // append word_id of best match
  if (maxDisplay) {
    contentString.append(std::to_string(matches[0][0]));
    contentString.append(", ");
  }
  for (size_t i = 0; i < maxDisplay; i++) {
    contentString.append("{\"word\": \"");
    wstring tmp = getWord(matches[i][0]);
    std::wstring_convert<std::codecvt_utf8<wchar_t>> converter;
    string wordNarrow = converter.to_bytes(tmp);
    wordNarrow = escapeJson(wordNarrow);
    contentString.append(wordNarrow);
    contentString.append("\", \"id\":");
    contentString.append(std::to_string(matches[i][0]));
    contentString.append(", \"score\":");
    contentString.append(std::to_string(matches[i][2]));
    contentString.append("},");
  }
  // remove last comma if results were found (at least it consists of
  // an opening [
  if (contentString.length() > 1) {
    contentString = contentString.substr(0, contentString.length() - 1);
  }
  contentString.append("]");
  return contentString;
}

// __________________________________________________________________
string QGramIndex::getCoordinates(uint32_t wordId) const {
  // check for invalid index, neccessary since this can be inflenced via
  // web query
  if (wordId >= _numWords) {
    return std::string("[]");
  } else {
    std::string result = "[";
    result.append(std::to_string(_lat[wordId]));
    result.append(", ");
    result.append(std::to_string(_long[wordId]));
    result.append("]");
    return result;
  }
}
