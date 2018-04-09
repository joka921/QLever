#include "EntityFinder.h"

#include <fstream>
#include <ios>
#include <sstream>
#include <unordered_map>
#include <algorithm>

// ______________________________________________________________________
void EntityFinder::InitializeFromTextFile(const std::string& filename) {
  std::ifstream file(filename);
  descriptionFilename = filename + ".desc";
  std::ifstream fileDesc(descriptionFilename);
  std::string line;
  while (std::getline(file, line)) {
    auto entity = WikidataEntity(line);
    //seperate Subject/Objects from Properties
    auto* wdVec = &wdNameVec;
    auto* descVec = &descOffsetVec;
    auto* nameVec = &nameDescVec;
    auto* aVec = &aliasVec;
    if (WikidataEntity::IsPropertyName(entity.name)) {
      wdVec = &wdNameVecPred;
      descVec = &descOffsetVecPred;
      nameVec = &nameDescVecPred;
      aVec = &aliasVecPred;
    }

    wdVec->push_back(entity.name);
    if (entity.aliases.size() > 0) {
      nameVec->push_back(entity.aliases[0]);
    } else {
      nameVec->push_back("no name");
      //std::cout << "No Name!!" << std::endl;
    }

    for ( auto& el : entity.aliases) {
      std::transform(el.begin(), el.end(), el.begin(), ::tolower);
      // TODO: substring memory consumption test
      aVec->push_back(std::make_pair(el, wdVec->size() -1));
      //std::cout << el << " "  << wdVec.size() <<  std::endl;
    }

    // for each line in alias file there must exist exactly one line in
    // description file. We don't want the description but only the offsets
    // TODO: is there a way to do this without actually reading the file?
    descVec->push_back(fileDesc.tellg());
    std::string tempDesc;
    std::getline(fileDesc, tempDesc);
  }
  auto sortPred = [](const std::pair<std::string, unsigned>&  p1,
                     const std::pair<std::string, unsigned>&  p2) {
     return p1.first < p2.first;};
  std::cout << aliasVec.size() << "entities" << std::endl;
  std::cout << aliasVecPred.size() << "Predicates" << std::endl;
  std::sort(aliasVec.begin(), aliasVec.end(), sortPred);
  std::sort(aliasVecPred.begin(), aliasVecPred.end(), sortPred);
  

  // Set up the "translation indices" for the sub/objects
  size_t maxIdxEntity = 0;
  for (const auto& el : wdNameVec) {
    size_t idx = getIdxFromWdName(el);
    maxIdxEntity = std::max(maxIdxEntity, idx);
  }
  EntityToIdxVec.resize(maxIdxEntity);
  std::fill(EntityToIdxVec.begin(), EntityToIdxVec.end(), -1);

  size_t num = 0;
  for (const auto& el : wdNameVec) {
    size_t idx = getIdxFromWdName(el);
    EntityToIdxVec[idx] = num;
    num += 1;
  }

  // same for the properties
  maxIdxEntity = 0;
  for (const auto& el : wdNameVecPred) {
    size_t idx = getIdxFromWdName(el);
    maxIdxEntity = std::max(maxIdxEntity, idx);
  }

  PropertyToIdxVec.resize(maxIdxEntity);
  std::fill(PropertyToIdxVec.begin(), PropertyToIdxVec.end(), -1);

  num = 0;
  for (const auto& el : wdNameVecPred) {
    size_t idx = getIdxFromWdName(el);
    PropertyToIdxVec[idx] = num;
    num += 1;
  }
}

// __________________________________________________________________
std::vector<WikidataEntityShort> EntityFinder::findEntitiesByPrefix( const std::string& prefixA, SearchMode mode)
     {
  std::string prefix = prefixA;
  std::ifstream descFile(descriptionFilename);
  std::transform(prefix.begin(), prefix.end(), prefix.begin(), ::tolower);
  auto boundPred = [](const std::pair<std::string, unsigned>&  p1,
                     const std::string& p2) { return p1.first < p2;};

  //TODO: some meaningful mixing of subjects and properties for searchmode "all"
  auto* wdVec = &wdNameVec;
  auto* vec = &aliasVec;
  auto* descVec = &descOffsetVec;
  auto* nameVec = &nameDescVec;
  if (mode == SearchMode::Properties) {
    vec = &aliasVecPred;
    descVec = &descOffsetVecPred;
    wdVec = &wdNameVecPred;
    nameVec = &nameDescVecPred;
  }
   auto res = std::lower_bound(vec->begin(), vec->end(), prefix, boundPred);
   std::vector<WikidataEntityShort> ret;
   while (res != vec->end() && std::equal(prefix.begin(), prefix.end(), (*res).first.begin())) {
     auto idx = (*res).second;
     if (descFile.is_open()) {
       descFile.seekg((*descVec)[idx]);
     }
     std::string desc;
     std::getline(descFile, desc);
     ret.push_back(WikidataEntityShort((*wdVec)[idx], (*nameVec)[idx], desc));
     std::cout << ret.size() << std::endl;
     if (ret.size() > 20) return ret;
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
    auto* vec = &EntityToIdxVec;
    auto* nameVec = &nameDescVec;
    if (WikidataEntity::IsPropertyName(el)) {
      vec = &PropertyToIdxVec;
      nameVec = &nameDescVecPred;
    }
    std::string name = "";
    std::string desc = "";
    if (idx < vec->size()) {
      // convert from the "wikidata-name-idx" to the internal (unique) index
      idx = (*vec)[idx];
      if (idx >= 0) {
        // if there is an entity matching, then also include name and description
        name = (*nameVec)[idx];
      }
    }
    //TODO: read from descfile
    ret.emplace_back(el, (*nameVec)[idx], (*nameVec)[idx]);
  }
  return ret;
}

// ______________________________________________________________________________
void EntityFinder::WriteToFile(const std::string& filename) {
  std::cout << "Writing to file " << filename << std::endl;
  std::ofstream os(filename, std::ios::binary);
  boost::archive::binary_oarchive oar(os);
  oar << *this;
  std::cout << "Done." << std::endl;
}

// ______________________________________________________________________________
EntityFinder EntityFinder::ReadFromFile(const std::string& filename) {
  std::cout << "Reading from file " << filename << std::endl;
  std::ifstream os(filename, std::ios::binary);
  boost::archive::binary_iarchive oar(os);
  EntityFinder ent;
  oar >> ent;
  std::cout << "Done." << std::endl;
  return ent;
}

// ________________________________________________________________
template<class Archive>
void EntityFinder::serialize(Archive& ar, const unsigned int version){
  ar & wdNameVec;
  ar & wdNameVecPred;
  ar & EntityToIdxVec;
  ar & PropertyToIdxVec;
  ar & descriptionFilename;
  //ar & descOffsetVec;
  //ar & descOffsetVecPred;
  ar & nameDescVec;
  ar & nameDescVecPred;
  ar & aliasVec;
  ar & aliasVecPred;
}
