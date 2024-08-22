//  Copyright 2023, University of Freiburg,
//  Chair of Algorithms and Data Structures.
//  Author: Johannes Kalmbach <kalmbach@cs.uni-freiburg.de>

#ifndef QLEVER_SPECIALIDS_H
#define QLEVER_SPECIALIDS_H

#include <absl/strings/str_cat.h>

#include <exception>

#include "global/Constants.h"
#include "global/Id.h"
#include "util/HashMap.h"
#include "util/HashSet.h"
#include "util/SourceLocation.h"

namespace qlever {

// A mapping from special builtin IRIs that are not managed via the normal
// vocabulary to the IDs that are used to represent them. These IDs all have the
// `Undefined` datatype s.t. they do not accidentally interfere with other IDs.
// TODO<joka921> Use ad_utility::HashMap again once the debugging in MacOs is
// done.
struct M : private std::unordered_map<std::string, Id> {
  using Base = std::unordered_map<std::string, Id>;
  M(Base b) : Base{std::move(b)} {}
  using Base::contains;
  Id at(std::string_view key,
        ad_utility::source_location l =
            ad_utility::source_location::current()) const {
    auto it = Base::find(std::string{key});
    if (it != Base::end()) {
      return it->second;
    }
    throw std::runtime_error{
        absl::StrCat("Key \"", key, "\" was not found, requested at ",
                     l.file_name(), " in line ", l.line())};
  }
};
inline const M& specialIds() {
  static const auto ids = []() {
    std::unordered_map<std::string, Id> result{
        {HAS_PREDICATE_PREDICATE, Id::fromBits(1)},
        {std::string{HAS_PATTERN_PREDICATE}, Id::fromBits(2)},
        {std::string{DEFAULT_GRAPH_IRI}, Id::fromBits(3)},
        {INTERNAL_GRAPH_IRI, Id::fromBits(4)}};

    // Perform the following checks: All the special IDs are unique, all of them
    // have the `Undefined` datatype, but none of them is equal to the "actual"
    // UNDEF value.
    auto values = std::views::values(result);
    auto undefTypeButNotUndefValue = [](Id id) {
      return id != Id::makeUndefined() &&
             id.getDatatype() == Datatype::Undefined;
    };
    AD_CORRECTNESS_CHECK(
        std::ranges::all_of(values, undefTypeButNotUndefValue));
    ad_utility::HashSet<Id> uniqueIds(values.begin(), values.end());
    AD_CORRECTNESS_CHECK(uniqueIds.size() == result.size());
    return M{std::move(result)};
  }();
  return ids;
};

// Return the [lowerBound, upperBound) for the special Ids.
// This range can be used to filter them out in cases where we want to ignore
// triples that were added by QLever for internal reasons.
static constexpr std::pair<Id, Id> getBoundsForSpecialIds() {
  constexpr auto upperBound = Id::makeFromBool(false);
  static_assert(static_cast<int>(Datatype::Undefined) == 0);
  static_assert(upperBound.getBits() == 1UL << Id::numDataBits);
  return {Id::fromBits(1), upperBound};
}
}  // namespace qlever

#endif  // QLEVER_SPECIALIDS_H
