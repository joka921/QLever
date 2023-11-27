//  Copyright 2023, University of Freiburg,
//                  Chair of Algorithms and Data Structures.
//  Author: Johannes Kalmbach <kalmbach@cs.uni-freiburg.de>

#ifndef QLEVER_SHORTERNAMESPACES_H
#define QLEVER_SHORTERNAMESPACES_H

#include <ranges>
namespace ad_utility {}
// Define three shorter namespace aliases: `ad`, `stdr` and `stdv`.
// Can be used at file, namespace, or function scope.
// TODO<joka921> I would argue that `ad` instead of `ad_utiltiy` could also be
// leaked into header files etc. or maybe use something shorter like `ad_fr::`
// to reduce the probability of collision.
#define AD_SHORTER_NAMESPACES   \
  namespace ad = ad_utility;    \
  namespace stdr = std::ranges; \
  namespace stdv = std::views;

#endif  // QLEVER_SHORTERNAMESPACES_H
