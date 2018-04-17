#include "WikidataEntity.h"

// _____________________________________________________________________
WikidataEntity::WikidataEntity(const std::string& line) {

  auto pos = line.find("\t");
  name = line.substr(0, pos);
  auto newpos = pos;
  while (newpos != std::string::npos) {
    newpos = line.find("\t", pos + 1);
    auto alias = line.substr(pos + 1, newpos - pos);
    aliases.push_back(alias);
    pos = newpos;
  }
}

// ________________________________________________
picojson::object WikidataEntityShort::ConvertToPicojsonObject() {
  picojson::value val;
  picojson::object tempOb;

  tempOb["wdName"] = picojson::value(wdName);
  tempOb["name"] = picojson::value(name);
  tempOb["description"] = picojson::value(description);
  //tempOb["type"] = picojson::value(type);

  //val.set<picojson::object>(tempOb);
  return tempOb;
}

// ____________________________________________________________________--
/*
template<class Archive>
void WikidataEntityShort::serialize(Archive& ar, std::uint32_t const version) {
  ar(wdName, name, description, type);
}
*/
