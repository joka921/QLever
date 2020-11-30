// Copyright 2019, University of Freiburg,
// Chair of Algorithms and Data Structures.
// Author: Johannes Kalmbach (johannes.kalmbach@gmail.com)

#ifndef QLEVER_IDTABLETYPEINFO_H
#define QLEVER_IDTABLETYPEINFO_H

#include <vector>
#include <variant>

#include "./IndexBuilderTypes.h"

/**
 * @brief For a set of result columns, store the corresponding Types of the entries
 *
 * If all entries have the same type, this information can be heavily compressed
 */
class IdTableTypeInfo {
  using TypeOrVec = std::variant<Datatype, std::vector<Datatype>>;
 public:
  explicit IdTableTypeInfo(size_t cols) : types_{cols} {}
  std::vector<TypeOrVec>& getData() {return types_;}
  const std::vector<TypeOrVec>& getData() const {return types_;}

 private:
  void check(size_t i) {
    if (i >= types_.size()) {
      throw std::runtime_error{"Trying to access type info for a non-existing column. Should never happen, please report this"};
    }
  }
  std::vector<TypeOrVec> types_;


};

#endif  // QLEVER_IDTABLETYPEINFO_H

