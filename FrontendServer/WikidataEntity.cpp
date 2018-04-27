#include "WikidataEntity.h"

// _____________________________________________________________________
WikidataEntity::WikidataEntity(const std::string& line) {

  auto pos = line.find("\t");
  name = line.substr(0, pos);
  auto newpos = line.find("\t", pos + 1);
  numSitelinks = std::stoi(line.substr(pos + 1, newpos - pos));
  pos = newpos;
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
  tempOb["type"] = picojson::value(EntityTypeToString(type));

  //val.set<picojson::object>(tempOb);
  return tempOb;
}

// Converter Function
std::string EntityTypeToString(const EntityType& type) {
  if (type == EntityType::Subject) {
    return "0";
  }
  return "1";
}

// ____________________________________________________________________--
/*
template<class Archive>
void WikidataEntityShort::serialize(Archive& ar, std::uint32_t const version) {
  ar(wdName, name, description, type);
}
*/
