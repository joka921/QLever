#include "WikidataEntity.h"

// _____________________________________________________________________
WikidataEntity::WikidataEntity(const std::string& line) {

  auto pos = line.find("\t");
  name = line.substr(0, pos);
  auto newpos = line.find("\t", pos + 1);
  while (newpos != std::string::npos) {
    auto alias = line.substr(pos + 1, newpos - pos);
    aliases.push_back(alias);
    pos = newpos;
    newpos = line.find("\t", pos + 1);
  }
}

