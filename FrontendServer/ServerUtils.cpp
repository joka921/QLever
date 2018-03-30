#include "ServerUtils.h"

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
