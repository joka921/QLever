// Copyright 2018, University of Freiburg,
// Chair of Algorithms and Data Structures.
// Author: Johannes Kalmbach (johannes.kalmbach@gmail.com)
//
#pragma once

#include <cassert>
#include <stxxl/vector>
#include "../global/Id.h"
#include "../util/Exception.h"
#include "../util/HashMap.h"
#include "../util/Log.h"
#include "./MetaDataTypes.h"

// _____________________________________________________________
// hidden in implementation namespace
namespace VecWrapperImpl {
template <class Vec>
// iterator class for a unordered_map interface based on an array with the size
// of the key space. access is done by the index, and when iterating, we have
// to skip the empty entries. Needs an empty key to work properly
class Iterator {
 public:
  using iterator_category = std::forward_iterator_tag;
  using value_type = std::pair<Id, FullRelationMetaData>;

  // _________________________________________________
  std::pair<Id, const FullRelationMetaData&> operator*() const {
    // make sure that we do not conflict with the empty key
    AD_CHECK(_id != size_t(-1));
    return std::make_pair(_id, std::cref(*_it));
  }

  // _________________________________________________
  std::pair<Id, std::reference_wrapper<const FullRelationMetaData>>*
  operator->() const {
    // make sure that we do not conflict with the empty key
    AD_CHECK(_id != size_t(-1));
    _accessPair = **this;
    return &_accessPair;
  }

  // _____________________________________________________________
  Iterator(Id id, const typename Vec::const_iterator& it, const Vec* const vec)
      : _id(id), _it(it), _accessPair(**this), _vec(vec) {}

  // ________________________________________________
  Iterator& operator++() {
    ++_id;
    ++_it;
    goToNexValidEntry();

    return *this;
  }

  // ___________________________________________________
  void goToNexValidEntry() {
    while (_it != _vec->cend() && (*_it) == emptyMetaData) {
      ++_id;
      ++_it;
    }
  }

  // ________________________________________________
  Iterator operator++(int) {
    Iterator old(*this);
    ++_id;
    ++_it;
    goToNexValidEntry();
    return old;
  }

  // _______________________________________________
  bool operator==(const Iterator& other) { return _it == other._it; }

  // _______________________________________________
  bool operator!=(const Iterator& other) { return _it != other._it; }

 private:
  Id _id;
  typename Vec::const_iterator _it;
  // here we store the pair needed for operator->()
  // will be updated before each access
  mutable std::pair<Id, std::reference_wrapper<const FullRelationMetaData>>
      _accessPair;
  const Vec* const _vec;

  const FullRelationMetaData emptyMetaData = FullRelationMetaData::empty;
};
}  // namespace VecWrapperImpl

// _____________________________________________________________________
template <class M>
class MetaDataWrapperDense {
 public:
  using Key = IdWithDatatype;
  using Value = FullRelationMetaData;

  using Iterator = typename M::iterator;
  using ConstIterator = typename M::const_iterator;

  // _________________________________________________________
  MetaDataWrapperDense() = default;
  // ________________________________________________________
  MetaDataWrapperDense(MetaDataWrapperDense<M>&& other) noexcept = default;
  // ______________________________________________________________
  MetaDataWrapperDense& operator=(MetaDataWrapperDense<M>&& other) noexcept = default;

  // Templated setup version
  // Arguments are passsed through to template argument M.
  // TODO<joka921>: enable_if  for better error messages
  void setup(const std::string& vecFilename) {
    // size has to be set correctly by a call to setSize(), this is done
    // in IndexMetaData::createFromByteBuffer
    _vec = M(vecFilename);
  }

  // setup for the readonly version
  void setup(const std::string& filename, ad_utility::ReuseTag t, ad_utility::AccessPattern pattern) {
    _vec = M(filename, t, pattern);
  }

  // ___________________________________________________________
  size_t size() const { return _vec.size(); }

  // __________________________________________________________________
  ConstIterator cbegin() const {
    return _vec.cbegin();
  }

  // __________________________________________________________________
  ConstIterator begin() const {
    return _vec.begin();
  }
  // __________________________________________________________________
  Iterator begin() {
    return _vec.begin();
  }

  // __________________________________________________________________________
  ConstIterator cend() const { return _vec.cend(); }

  // __________________________________________________________________________
  ConstIterator end() const { return _vec.end(); }
  Iterator end(){ return _vec.end(); }

  // ____________________________________________________________
  void add(IdWithDatatype id, const FullRelationMetaData& value) {
    AD_CHECK(id == value._relId); // this redundancy is useful for the calling interface
    AD_CHECK(_vec.size() == 0 || _highestValue < id);
    _highestValue = id;

    _vec.push_back(value);
  }

  // __________________________________________________________
  const FullRelationMetaData& getAsserted(Key id) const {
    auto res = binarySearch(id);

    AD_CHECK(res != std::nullopt);
    return *(res.value());
  }

  // _________________________________________________________
  FullRelationMetaData& operator[](Key id) {
    auto res = binarySearch(id);

    AD_CHECK(res != std::nullopt);
    return *(res.value());
  }

  // ________________________________________________________
  size_t count(Key id) const {
    return binarySearch(id) != std::nullopt;
  }

  // ___________________________________________________________
  std::string getFilename() const { return _vec.getFilename(); }

 private:
  // the empty key, must be the first member to be initialized
  M _vec;
  IdWithDatatype _highestValue; // make sure that we insert in ascending order to make binary search work.

  std::optional<ConstIterator> binarySearch(Key v) const {
    auto it = std::lower_bound(_vec.begin(), _vec.end(), v, [](const Value& a, const Key& b){return a._relId < b; });
    if (it == _vec.end()) {
      return std::nullopt;
    }
    const auto& el = *it;
    if (el._relId == v) {
      return it;
    } else {
      return std::nullopt;
    }

  }
  // TODO<joka921> remove duplication via const-cast, figure out, how it is correct
  std::optional<Iterator> binarySearch(Key v) {
    auto it = std::lower_bound(_vec.begin(), _vec.end(), v, [](const Value& a, const Key& b){return a._relId < b; });
    if (it == _vec.end()) {
      return std::nullopt;
    }
    const auto& el = *it;
    if (el._relId == v) {
      return it;
    } else {
      return std::nullopt;
    }

  }
};

template<class M>
const typename MetaDataWrapperDense<M>::Value& rmd(const typename MetaDataWrapperDense<M>::Iterator it) {
  return (*it);
}

template<class M>
const typename MetaDataWrapperDense<M>::Value& id(const typename MetaDataWrapperDense<M>::Iterator it) {
  return it->_relId;
}



// _____________________________________________________________________
template <class hashMap>
class MetaDataWrapperHashMap {
 public:
  // using hashMap = ad_utility::HashMap<Id, FullRelationMetaData>;
  // using hashMap = ad_utility::HashMap<Id, FullRelationMetaData>;
  using Key = typename hashMap::key_type;
  using Value = typename hashMap::value_type;

  using Iterator = typename hashMap::iterator;

  using ConstIterator = typename hashMap::const_iterator;



  // nothing to do here, since the default constructor of the hashMap does
  // everything we want
  explicit MetaDataWrapperHashMap() = default;
  // nothing to setup, but has  to be defined to meet template requirements
  void setup(){};

  // _______________________________________________________________
  size_t size() const { return _map.size(); }

  // __________________________________________________________________
  ConstIterator cbegin() const { return _map.begin(); }

  // __________________________________________________________________
  Iterator begin() { return _map.begin(); }

  // ____________________________________________________________
  ConstIterator cend() const { return _map.end(); }

  // ____________________________________________________________
  Iterator end() { return _map.end(); }

  // ____________________________________________________________
  void add(Key id, const FullRelationMetaData& value) {
    AD_CHECK(id == value._relId);
    AD_CHECK(!count(id));
    _map[id] = value; }

  // __________________________________________________________
  const FullRelationMetaData& getAsserted(Key id) const {
    auto it = _map.find(id);
    AD_CHECK(it != _map.end());
    return std::cref(it->second);
  }

  // __________________________________________________________
  FullRelationMetaData& operator[](Key id) {
    auto it = _map.find(id);
    AD_CHECK(it != _map.end());
    return std::ref(it->second);
  }

  // ________________________________________________________
  size_t count(Key id) const {
    // can either be 1 or 0 for map-like types
    return _map.count(id);
  }

 private:
  hashMap _map;
};

template<class It>
const FullRelationMetaData& itToRmd(It&&  it) {
  if constexpr(std::is_pointer_v<std::decay_t<It>>) {
    return *it;
  } else {
    return it->second;
  }
}

template<class It>
const IdWithDatatype& itToId(It&&  it) {
  if constexpr(std::is_pointer_v<std::decay_t<It>>) {
    return it->_relId;
  } else {
    return it->first;
  }
}

