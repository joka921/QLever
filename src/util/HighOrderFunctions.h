//
// Created by johannes on 21.11.19.
//

#ifndef QLEVER_HIGHORDERFUNCTIONS_H

namespace highOrderFunctions {

auto ADD = [](const auto& a, const auto& b) {return a + b;};
auto ID = [](const auto& a) {return a;};

template<class V>
auto CONST(const V& v) {
  return [v = v]([[maybe_unused]]const auto... args) {return v;};
}


}

#define QLEVER_HIGHORDERFUNCTIONS_H

#endif //QLEVER_HIGHORDERFUNCTIONS_H
