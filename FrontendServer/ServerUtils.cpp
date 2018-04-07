#include "ServerUtils.h"
#include "utils.h"
#include "EntityFinder.h"

// _______________________________________________________________
std::string ServerUtils::entitiesToJson(const std::vector<WikidataEntityShort>& entities, size_t num) {

  string contentString = "[";
  // append word_id of best match
  size_t cnt = 0;
  for (const auto& el : entities) {
    if (cnt >= num) break;
    contentString.append("{\"name\": \"");
    contentString.append(escapeJson(el.name));
    contentString.append("\", \"desc\": \"");
    contentString.append(ServerUtils::escapeJson(el.description));
    contentString.append("\", \"wdName\": \"");
    contentString.append(ServerUtils::escapeJson(el.wdName));
    contentString.append("\", \"type\": \"");
    string typeString = el.type == EntityType::Property ? "P" : "Q";
    contentString.append(typeString);
    contentString.append("\"},");
    cnt++;
  }
  // remove last comma if results were found (at least it consists of
  // an opening [
  if (contentString.length() > 1) {
    contentString = contentString.substr(0, contentString.length() - 1);
  }
  contentString.append("]");
  return contentString;
}

// _______________________________________________________________
std::string ServerUtils::escapeJson(const std::string& wordNarrow) {
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

  // escape '\t'
  illegalPos = output.find("\t");
  while (illegalPos != output.npos) {
    output.replace(illegalPos, 1,  "\\t");
    illegalPos = output.find("\"", illegalPos + 2);
  }

  return output;
}

// ___________________________________________________________________
std::pair<std::string, SearchMode> ServerUtils::parseQuery(const std::string& query) {
  // we already have checked that string starts with "?t="
  auto searchMode = SearchMode::Invalid;
  // searchtype always has three chars
  auto searchtypeString = query.substr(3, 3);
  if (searchtypeString == "all") {
    searchMode = SearchMode::All;
  } else if (searchtypeString == "obj") {
    searchMode = SearchMode::Subjects;
  } else if (searchtypeString == "prd") {
    searchMode = SearchMode::Properties;
  }

  if (query.substr(6, 3) != "?q=") {
    searchMode = SearchMode::Invalid;
  }
  auto searchQuery = decodeURL(query.substr(9));
  return std::make_pair(searchQuery, searchMode);
}

// Decode URLS from URL-Encoding to valid strings
std::string ServerUtils::decodeURL(std::string encoded) {
  std::string output = "";
  auto curPos = encoded.find("%");
  output.append(encoded.substr(0, curPos));
  while (curPos != encoded.npos) {
    // we have found another "%", decode
    int nextByte = 0;
    // two chars after "%" are hex string of byte
    std::string nextByteStr = encoded.substr(curPos + 1, 2);
    // we have to go through an int, or the results will be wrong
    std::istringstream(nextByteStr) >> std::hex >> nextByte;
    output.push_back(static_cast<char>(nextByte));
    auto posStart = curPos + 3;
    curPos = encoded.find("%", posStart);
    output.append(encoded.substr(posStart, curPos - posStart));
  }

  // decode "+" to " "
  curPos = 0;
  while (true) {
    curPos = output.find("+", curPos);
    if (curPos == output.npos) break;
    output = output.replace(curPos, 1, " ");
  }
  return output;
}


// read the file specified by filename arg and return a
// pair<bool, string> with
// <successful?, contentsOfFile>
std::pair<bool, std::string> ServerUtils::readFile(std::string filename) {
  std::ifstream infile(filename.c_str());
  if (!infile.is_open()) {
    return std::make_pair(false, std::string(
        "The requested file does not exist"));
  } else {
    std::stringstream lineStream;
    lineStream << infile.rdbuf();
    return std::make_pair(true, lineStream.str());
  }
}


// detect the file ending of the given file name and return the appropriate
// MIME type. only html, css and js are correct.
// Returns: pair<bool, string> with <successful?, MIME-type>
std::pair<bool, std::string> ServerUtils::detectContentType(std::string filename) {
  // for raghu, search for the end to also parse
  // a.b.c.d.e.f.g.txt
  auto posDot = filename.find_last_of(".");
  bool reqValid = true;
  std::string contentType = "text/html";
  if (posDot == filename.npos) {
    // invalid file without extension
    reqValid = false;
  } else {
    auto suffix = filename.substr(posDot, filename.length() - posDot);
    std::cout << "suffix is " << suffix << std::endl;
    if (suffix == std::string(".txt")) {
      contentType = "text/plain";
    } else if (suffix == std::string(".html")
        || suffix == std::string(".htm")) {
      contentType = "text/html";
    }  else if (suffix == std::string(".css")) {
      contentType = "text/css";
    }  else if (suffix == std::string(".js")) {
      contentType = "application/javascript";
    } else {
      std::cout << "no valid extension" << std::endl;
      reqValid = false;
    }
  }
  return std::make_pair(reqValid, contentType);
}

// ___________________________________________________________________________________
std::vector<std::string> ServerUtils::split(const std::string& s, char delim) {
  std::vector<std::string> ret;
  std::stringstream sstream(s);
  std::string item;
  while (std::getline(sstream, item, delim)) {
    ret.push_back(item);
  }
  return ret;
}
