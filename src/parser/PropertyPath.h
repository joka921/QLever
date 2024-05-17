// Copyright 2022, University of Freiburg,
// Chair of Algorithms and Data Structures.
// Author: Florian Kramer (florian.kramer@mail.uni-freiburg.de)
#pragma once

#include <initializer_list>
#include <string>
#include <vector>

#include "parser/TripleComponent.h"
#include "parser/data/Variable.h"

using std::string;
using std::vector;

class PropertyPath {
 public:
  enum class Operation {
    SEQUENCE,
    ALTERNATIVE,
    INVERSE,
    IRI_OR_VAR,
    ZERO_OR_MORE,
    ONE_OR_MORE,
    ZERO_OR_ONE
  };

  PropertyPath() : _operation(Operation::IRI_OR_VAR) {}
  explicit PropertyPath(Operation op) : _operation(op) {
    if (op == Operation::ZERO_OR_MORE || op == Operation::ZERO_OR_ONE) {
      can_be_null_ = true;
    }
  }
  PropertyPath(Operation op, TripleComponent iri,
               std::initializer_list<PropertyPath> children);

  static PropertyPath fromVariable(Variable var) {
    return fromTripleComponent(std::move(var));
  }

  static PropertyPath fromIri(TripleComponent::Iri iri) {
    return fromTripleComponent(std::move(iri));
  }

  static PropertyPath fromTripleComponent(TripleComponent tc) {
    PropertyPath p(PropertyPath::Operation::IRI_OR_VAR);
    AD_CONTRACT_CHECK(tc.isIri() || tc.isVariable());
    p._iriOrVar = std::move(tc);
    return p;
  }

  static PropertyPath makeWithChildren(std::vector<PropertyPath> children,
                                       PropertyPath::Operation op) {
    PropertyPath p(std::move(op));
    p._children = std::move(children);
    return p;
  }

  static PropertyPath makeAlternative(std::vector<PropertyPath> children) {
    if (children.size() == 1) {
      return std::move(children.front());
    } else {
      return makeWithChildren(std::move(children), Operation::ALTERNATIVE);
    }
  }

  static PropertyPath makeSequence(std::vector<PropertyPath> children) {
    if (children.size() == 1) {
      return std::move(children.front());
    } else {
      return makeWithChildren(std::move(children), Operation::SEQUENCE);
    }
  }

  static PropertyPath makeInverse(PropertyPath child) {
    return makeWithChildren({std::move(child)}, Operation::INVERSE);
  }

  /**
   * @brief Make a PropertyPath based on the given child and apply the path
   * modifier. The path modifier may be one of: ? + *
   *
   * @param child The PropertyPath child
   * @param modifier A PropertyPath modifier (? + *)
   * @return PropertyPath With given modifier as Operation and given child
   */
  static PropertyPath makeModified(PropertyPath child,
                                   std::string_view modifier);

  static PropertyPath makeZeroOrMore(PropertyPath child) {
    return makeWithChildren({std::move(child)}, Operation::ZERO_OR_MORE);
  }

  static PropertyPath makeOneOrMore(PropertyPath child) {
    return makeWithChildren({std::move(child)}, Operation::ONE_OR_MORE);
  }

  static PropertyPath makeZeroOrOne(PropertyPath child) {
    return makeWithChildren({std::move(child)}, Operation::ZERO_OR_ONE);
  }

  bool operator==(const PropertyPath& other) const = default;

  void writeToStream(std::ostream& out) const;
  [[nodiscard]] std::string asString() const;

  void computeCanBeNull();

  // ASSERT that this property path consists of a single IRI and return that
  // IRI.
  [[nodiscard]] const TripleComponent::Iri& getIri() const;
  bool isIri() const;

  [[nodiscard]] const Variable& getVariable() const;
  bool isVariable() const;

  Operation _operation;

  // In case of an iri
  TripleComponent _iriOrVar;

  std::vector<PropertyPath> _children;

  /**
   * True iff this property path is either a transitive path with minimum length
   * of 0, or if all of this transitive path's children can be null.
   */
  bool can_be_null_ = false;
};
