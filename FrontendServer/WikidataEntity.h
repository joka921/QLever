#ifndef _WIKIDATA_ENTITY_H
#define _WIKIDATA_ENTITY_H

#include <string>
#include <vector>
#include <iostream>

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
    type = wdName[0] =='P' ? EntityType::Property : EntityType::Subject;}


  string toString() {return wdName + "\t" + name + "\t" + description;}
};

#endif  // _WIKIDATA_ENTITY_H
