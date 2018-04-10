#ifndef _ENTITY_FINDER_H
#define _ENTITY_FINDER_H

#include <vector>
#include <utility>
#include <string>
#include <iostream>
#include <unordered_map>

#include <boost/archive/binary_oarchive.hpp>
#include <boost/archive/binary_iarchive.hpp>
#include <boost/serialization/vector.hpp>
#include <boost/serialization/string.hpp>
#include <boost/serialization/utility.hpp>

#include "WikidataEntity.h"

enum class SearchMode {
  All, Subjects, Properties, Invalid};


BOOST_SERIALIZATION_SPLIT_FREE(std::streampos)
namespace boost {
  namespace serialization {
    template<class Archive>
      void serialize(Archive & ar, std::mbstate_t& s, const unsigned int version) {
        ar & s.__count;
        ar & s.__value.__wch;
      }

    template<class Archive>
      void save(Archive& ar, const std::streampos &s, const unsigned int version) 
      {
        std::streamoff off = s;
        ar & off;
        std::mbstate_t state = s.state();
        ar & state;
      }

    template<class Archive>
      void load(Archive& ar, std::streampos &s, const unsigned int version)
      {
        std::streamoff off;
        ar & off;
        std::mbstate_t state;
        ar & state;
        s += off;
        s.state(state);
      }
  }
}

// ___________________________________________________________
class EntityFinder {
 private:
  friend class boost::serialization::access;
  template<class Archive>
  void serialize(Archive & ar, const unsigned int version);
  std::vector<std::string> wdNameVec; // Name in Wikidata, e.gl "Q23"
  std::vector<std::string> wdNameVecPred; // Name in Wikidata, e.gl "Q23"

  // in wikidata dumps, the entries are not ordered, so we have to keep track of
  // their indices
  //  if EntityToIdxVec[x] = y then entity "Qx" can be found at idx y, same for
  //  properties
  std::vector<size_t> EntityToIdxVec;
  std::vector<size_t> PropertyToIdxVec;
  std::string descriptionFilename;
  // offsets of descriptions in description text file
  std::vector<std::streampos> descOffsetVec;
  std::vector<std::streampos> descOffsetVecPred;
  // only used during serialization
  std::vector<std::string> nameDescVec; // Name and description
  std::vector<std::string> nameDescVecPred; // Name and description

  // sorted vector where aliases are assigned to indices in the 2 vectors above
  // TODO: public actually only here for debugging
 public:
  std::vector<std::pair<std::string, unsigned>> aliasVec;
  std::vector<std::pair<std::string, unsigned>> aliasVecPred;
//  std::unordered_map<std::string, std::vector<unsigned>> wordMap;
  size_t getIdxFromWdName(const std::string& wdName);


// public:
  // Construct from file prepared by Preprocessor
  void InitializeFromTextFile(const std::string& filename);
  void WriteToFile(const std::string& filename);
  static EntityFinder ReadFromFile(const std::string& filename);
 
  std::vector<WikidataEntityShort> findEntitiesByPrefix(const std::string& prefix, SearchMode mode = SearchMode::All);

  std::vector<WikidataEntityShort> wdNamesToEntities(std::vector<string> wdNames);




};



#endif  // _ENTITY_FINDER_H
