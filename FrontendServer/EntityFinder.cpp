#include "EntityFinder.h"

#include <fstream>
#include <sstream>
#include <unordered_map>
#include <algorithm>

// ______________________________________________________________________
EntityFinder::EntityFinder(const std::string& filename) {
  std::ifstream file(filename);
  descriptionFilename = filename + ".desc";
  std::ifstream fileDesc(descriptionFilename);
  std::string line;
  std::unordered_map<std::string, std::vector<unsigned>> wordMap;
  while (std::getline(file, line)) {
    auto entity = WikidataEntity(line);
    wdNameVec.push_back(entity.name);

    if (entity.aliases.size() > 0) {
      nameDescVec.push_back(std::make_pair(entity.aliases[0], entity.description));
    } else {
      nameDescVec.push_back(std::make_pair("no name", entity.description));
      //std::cout << "No Name!!" << std::endl;
    }

    for ( auto& el : entity.aliases) {
      std::transform(el.begin(), el.end(), el.begin(), ::tolower);
      wordMap[el].push_back(wdNameVec.size() - 1);
    }

    // for each line in alias file there must exist exactly one line in
    // description file. We don't want the description but only the offsets
    // TODO: is there a way to do this without actually reading the file?
    descOffsetVec.push_back(fileDesc.tellg());
    std::string tempDesc;
    std::getline(fileDesc, tempDesc);


  }
  aliasVec.reserve(wordMap.size());
  for (const auto& entry : wordMap) {
    aliasVec.push_back(entry);
  }
  auto sortPred = [](const std::pair<std::string, std::vector<unsigned>>&  p1,
                     const std::pair<std::string, std::vector<unsigned>>&  p2) {
     return p1.first < p2.first;};
  std::sort(aliasVec.begin(), aliasVec.end(), sortPred);

  // Set up the "translation indices"
  size_t maxIdxEntity = 0;
  size_t maxIdxProperty = 0;
  for (const auto& el : wdNameVec) {
    size_t idx = getIdxFromWdName(el);
    if (WikidataEntity::IsSubjectName(el)) {
      maxIdxEntity = std::max(maxIdxEntity, idx);
    } else if (WikidataEntity::IsPropertyName(el)) {
      maxIdxProperty = std::max(maxIdxProperty, idx);
    }

  }
  EntityToIdxVec.resize(maxIdxEntity);
  PropertyToIdxVec.resize(maxIdxProperty);
  std::fill(EntityToIdxVec.begin(), EntityToIdxVec.end(), -1);
  std::fill(PropertyToIdxVec.begin(), PropertyToIdxVec.end(), -1);

  size_t num = 0;
  for (const auto& el : wdNameVec) {
    size_t idx = getIdxFromWdName(el);
    if (WikidataEntity::IsSubjectName(el)) {
      EntityToIdxVec[idx] = num;
    } else if (WikidataEntity::IsPropertyName(el)) {
      PropertyToIdxVec[idx] = num;
    }
    num += 1;
  }
}

// __________________________________________________________________
std::vector<WikidataEntityShort> EntityFinder::findEntitiesByPrefix( const std::string& prefixA, SearchMode mode)
     {
  std::string prefix = prefixA;
  std::ifstream descFile(descriptionFilename);
  std::transform(prefix.begin(), prefix.end(), prefix.begin(), ::tolower);
  auto boundPred = [](const std::pair<std::string, std::vector<unsigned>>&  p1,
                     const std::string& p2) { return p1.first < p2;};

   auto res = std::lower_bound(aliasVec.begin(), aliasVec.end(), prefix, boundPred);
   std::vector<WikidataEntityShort> ret;
   while (res != aliasVec.end() && std::equal(prefix.begin(), prefix.end(), (*res).first.begin())) {
     auto indices = (*res).second;
     for (const auto idx : indices) {
       if(mode == SearchMode::Properties && WikidataEntity::IsSubjectName(wdNameVec[idx])) continue;
       if(mode == SearchMode::Subjects && WikidataEntity::IsPropertyName(wdNameVec[idx])) continue;
       descFile.seekg(descOffsetVec[idx]);
       std::string desc;
       std::getline(descFile, desc);
       ret.emplace_back(wdNameVec[idx], nameDescVec[idx].first, desc);
     }
     res++;
   }
   std::cout << "found " << ret.size() << std::endl;
   return ret;
 }

// ________________________________________________________________________
size_t EntityFinder::getIdxFromWdName(const std::string& wdName) {
  // start with "<Q" or "<P", then number
  std::stringstream s(wdName.substr(2));
  size_t idx;
  s >> idx;
  return idx;
}

// ___________________________________________________________________
std::vector<WikidataEntityShort> EntityFinder::wdNamesToEntities(std::vector<string> wdNames) {
  std::vector<WikidataEntityShort> ret;
  for (const auto& el : wdNames) {
    auto idx = getIdxFromWdName(el);
    auto& vec = EntityToIdxVec;
    if (WikidataEntity::IsPropertyName(el)) {
      vec = PropertyToIdxVec;
    }
    std::string name = "";
    std::string desc = "";
    if (idx < vec.size()) {
      // convert from the "wikidata-name-idx" to the internal (unique) index
      idx = vec[idx];
      if (idx >= 0) {
        // if there is an entity matching, then also include name and description
        name = nameDescVec[idx].first;
        desc = nameDescVec[idx].second;
      }
    }
    ret.emplace_back(el, nameDescVec[idx].first, nameDescVec[idx].second);
  }
  return ret;
}
