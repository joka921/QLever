#ifndef _WIKIDATA_ENTITY_H
#define _WIKIDATA_ENTITY_H

#include <string>
#include <vector>
#include <iostream>
#include <cereal/cereal.hpp>
#include <cereal/archives/json.hpp>
#include "../picojson/picojson.h"


using std::string;

// _________________________________________________________________________-
enum class EntityType {
  Subject, Property};

// a simple class handling metadata of a wikidata entity
class WikidataEntity {
 public:
  string name;
  string description;
  std::vector<string> aliases;
    
  
  // read line that python preprocessor outputs
  // TODO: document format when finished
  WikidataEntity(const string& line);


  // _____________________________________
  static bool IsPropertyName(const std::string& name) {
    return name.substr(0, 2) == std::string("<P");
  }

  // _________________________________________________________________________
  static bool IsSubjectName(const std::string& name) {
    return name.substr(0, 2) == std::string("<Q");
  }
};

// for search results, only index, name and description
class WikidataEntityShort {
 public:
  string wdName;
  string name;
  string description;
  EntityType type;

  WikidataEntityShort(const string& wd, const string& nameT, const string& desc)
    : wdName(wd), name(nameT), description(desc) {
    type = WikidataEntity::IsPropertyName(wdName) ? EntityType::Property : EntityType::Subject;}


  string toString() {return wdName + "\t" + name + "\t" + description;}
  picojson::object ConvertToPicojsonObject();

  template<class Archive>
    void serialize(Archive& ar, std::uint32_t const version) {
      ar(CEREAL_NVP(wdName), CEREAL_NVP(name), CEREAL_NVP(description), CEREAL_NVP(type));
    }

};

#endif  // _WIKIDATA_ENTITY_H
