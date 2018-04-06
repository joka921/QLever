#ifndef _ENTITY_FINDER_H
#define _ENTITY_FINDER_H

#include <vector>
#include <utility>
#include <string>
#include <iostream>

#include "WikidataEntity.h"

enum class SearchMode {
  All, Subjects, Properties, Invalid};


// ___________________________________________________________
class EntityFinder {
 private:
  std::vector<std::string> wdNameVec; // Name in Wikidata, e.gl "Q23"

  // in wikidata dumps, the entries are not ordered, so we have to keep track of
  // their indices
  //  if EntityToIdxVec[x] = y then entity "Qx" can be found at idx y, same for
  //  properties
  std::vector<size_t> EntityToIdxVec;
  std::vector<size_t> PropertyToIdxVec;
  std::string descriptionFilename;
  // offsets of descriptions in description text file
  std::vector<std::streampos> descOffsetVec;
  std::vector<std::pair<std::string, std::string>> nameDescVec; // Name and description

  // sorted vector where aliases are assigned to indices in the 2 vectors above
  std::vector<std::pair<std::string, std::vector<unsigned>>> aliasVec;


 public:
  // Construct from file prepared by Preprocessor
  EntityFinder(const std::string& filename);
  std::vector<WikidataEntityShort> findEntitiesByPrefix(const std::string& prefix, SearchMode mode = SearchMode::All);



};

#endif  // _ENTITY_FINDER_H
