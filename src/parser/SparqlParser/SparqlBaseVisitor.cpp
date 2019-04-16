
// Generated from Sparql.g4 by ANTLR 4.7.1

#include "SparqlBaseVisitor.h"
#include "../../util/Exception.h"

antlrcpp::Any SparqlBaseVisitor::visitNumericLiteralUnsigned(
    SparqlParser::NumericLiteralUnsignedContext* ctx) {
  Number res;
  if (ctx->DECIMAL()) {
    res._type = Number::type::DECIMAL;
    auto d = ctx->DECIMAL();
    res._value = std::stod(d->getText());
    LOG(TRACE) << "Parsed a decimal with value " << res._value << std::endl;
  } else if (ctx->INTEGER()) {
    res._type = Number::type::INTEGER;
    auto d = ctx->INTEGER();
    res._value = std::stod(d->getText());
    LOG(TRACE) << "Parsed an integer with value " << res._value << std::endl;
  } else if (ctx->DOUBLE()) {
    res._type = Number::type::DECIMAL;
    auto d = ctx->DOUBLE();
    res._value = std::stod(d->getText());
    LOG(TRACE) << "Parsed a double with value " << res._value << std::endl;
  }
  return res;
}

// ____________________________________________________________________________________________________________________
antlrcpp::Any SparqlBaseVisitor::visitNumericLiteralPositive(
    SparqlParser::NumericLiteralPositiveContext* ctx) {
  Number res;
  if (ctx->DECIMAL_POSITIVE()) {
    res._type = Number::type::DECIMAL;
    auto d = ctx->DECIMAL_POSITIVE();
    res._value = std::stod(d->getText());
    LOG(TRACE) << "Parsed a positive decimal with value " << res._value
               << std::endl;
  } else if (ctx->INTEGER_POSITIVE()) {
    res._type = Number::type::INTEGER;
    auto d = ctx->INTEGER_POSITIVE();
    res._value = std::stod(d->getText());
    LOG(TRACE) << "Parsed a positive integer with value " << res._value
               << std::endl;
  } else if (ctx->DOUBLE_POSITIVE()) {
    res._type = Number::type::DECIMAL;
    auto d = ctx->DOUBLE_POSITIVE();
    res._value = std::stod(d->getText());
    LOG(TRACE) << "Parsed a positive double with value " << res._value
               << std::endl;
  }
  return res;
}

// ____________________________________________________________________________________________________________________
antlrcpp::Any SparqlBaseVisitor::visitNumericLiteralNegative(
    SparqlParser::NumericLiteralNegativeContext* ctx) {
  Number res;
  if (ctx->DECIMAL_NEGATIVE()) {
    res._type = Number::type::DECIMAL;
    auto d = ctx->DECIMAL_NEGATIVE();
    res._value = std::stod(d->getText());
    LOG(TRACE) << "Parsed a negative decimal with value " << res._value
               << std::endl;
  } else if (ctx->INTEGER_NEGATIVE()) {
    res._type = Number::type::INTEGER;
    auto d = ctx->INTEGER_NEGATIVE();
    res._value = std::stod(d->getText());
    LOG(TRACE) << "Parsed a negative integer with value " << res._value
               << std::endl;
  } else if (ctx->DOUBLE_NEGATIVE()) {
    res._type = Number::type::DECIMAL;
    auto d = ctx->DOUBLE_NEGATIVE();
    res._value = std::stod(d->getText());
    LOG(TRACE) << "Parsed a negative double with value " << res._value
               << std::endl;
  }
  return res;
}

// ____________________________________________________________________________________________________________________
antlrcpp::Any SparqlBaseVisitor::visitString(SparqlParser::StringContext* ctx) {
  string res;
  if (auto x = ctx->STRING_LITERAL1(); x) {
    // this is the one with the single quotes
    // TODO<joka921> Convert inner quotes from single to double and the outer
    // ones vice versa
    res = x->toString();
  } else if (auto y = ctx->STRING_LITERAL2(); y) {
    // TODO<joka921> convert escaped chars to UTF8
    res = y->toString();
  }
  return res;
}

// ____________________________________________________________________________________________________________________
antlrcpp::Any SparqlBaseVisitor::visitRdfLiteral(
    SparqlParser::RdfLiteralContext* ctx) {
  // TODO<joka921> if we have support for more ^^types, we can dispatch them
  // here
  LOG(TRACE) << "Encountered an RDF LIteral\n";
  std::string baseValue = visitString(ctx->string());
  if (ctx->LANGTAG()) {
    baseValue += ctx->LANGTAG()->toString();
  } else if (ctx->iriRef()) {
    baseValue += "^^" + visitIriRef(ctx->iriRef());
  }
  LOG(TRACE) << " Final value of RDF Literal is " << baseValue << '\n';
  return baseValue;
}

// ____________________________________________________________________________________________________________________
antlrcpp::Any SparqlBaseVisitor::visitPrefixedName(
    SparqlParser::PrefixedNameContext* ctx) {
  if (ctx->PNAME_LN()) {
    auto val = ctx->PNAME_LN()->toString();
    auto pos = val.find(':');
    auto prefix = val.substr(0, pos);
    auto rem = val.substr(pos + 1);
    if (!_prefixes.count(prefix)) {
      throw ParseException("prefix " + prefix +
                           " was used in query but not declared in prologue");
    }
    std::string res = '<' + _prefixes[prefix] + rem + '>';
    LOG(TRACE) << "Expanded prefix to " << res << "\n";
    return res;
  } else {
    throw NotImplementedException("PNAME_NS case for prefixedName rule");
  }
}

// ____________________________________________________________________________________________________________________
antlrcpp::Any SparqlBaseVisitor::visitIriRef(SparqlParser::IriRefContext* ctx) {
  std::string res;
  if (auto x = ctx->IRI_REF(); x) {
    res = x->toString();
  } else {
    res = std::string(visitPrefixedName(ctx->prefixedName()));
  }
  LOG(TRACE) << "Parsed an iriRef as " << res << '\n';
  return res;
}

// ______________________________________________________________________________
antlrcpp::Any SparqlBaseVisitor::visitBlankNode(
    SparqlParser::BlankNodeContext* ctx) {
  (void)ctx;
  throw NotImplementedException("Blank Nodes");
}

// _______________________________________________________________________________
antlrcpp::Any SparqlBaseVisitor::visitVar(SparqlParser::VarContext* ctx) {
  if (auto t = ctx->VAR1(); t) {
    return t->toString();
  } else {
    t = ctx->VAR2();
    // $var, refactor to ?var
    return "?" + t->toString().substr(1);
  }
}

// ______________________________________________________________________________
string SparqlBaseVisitor::visitVerb(SparqlParser::VerbContext* ctx) {
  if (auto c = ctx->varOrIRIref(); c) {
    return visitVarOrIRIref(c);
  } else {
    // special predicate "a"
    return string(u8"<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>");
  }
  return visitChildren(ctx);
}

// ______________________________________________________________________________________________________________
vector<string> SparqlBaseVisitor::visitObjectList(
    SparqlParser::ObjectListContext* ctx) {
  std::vector<std::string> objects;
  auto objectContexts = ctx->object();
  for (const auto o : objectContexts) {
    objects.push_back(visitObject(o).as<std::string>());
    LOG(TRACE) << "Object " << objects.back() << "parsed in objectList\n";
  }
  return objects;
}

// ________________________________________________________________________________________________________________
antlrcpp::Any SparqlBaseVisitor::visitTriplesNode(
    SparqlParser::TriplesNodeContext* ctx) {
  (void)ctx;
  throw NotImplementedException(
      "triplesNode (collection or blankNodePropertyList");
}

// ______________________________________________________________________________________
antlrcpp::Any SparqlBaseVisitor::visitGraphTerm(
    SparqlParser::GraphTermContext* ctx) {
  if (ctx->NIL()) {
    return std::string("http://www.w3.org/1999/02/22-rdf-syntax-ns#nil");
  } else if (auto l = ctx->booleanLiteral(); l) {
    throw NotImplementedException("Boolean Literals");
  } else if (auto n = ctx->numericLiteral(); n) {
    // TODO<joka921> Convert to internal representation
    throw NotImplementedException("Numeric Literals");
  } else {
    // blankNodes, iriRefs and literals are already parsed as strings
    // at this point
    return visitChildren(ctx);
  }
}

// _____________________________________________________________________________________
antlrcpp::Any SparqlBaseVisitor::visitVarOrTerm(
    SparqlParser::VarOrTermContext* ctx) {
  // variables or terms all are strings for our purposes,
  // so this should be fine.
  return visitChildren(ctx);
}

antlrcpp::Any SparqlBaseVisitor::visitVarOrIRIref(
    SparqlParser::VarOrIRIrefContext* ctx) {
  // vars and Irirefs are already string values
  return visitChildren(ctx);
}

// _____________________________________________________________________________
antlrcpp::Any SparqlBaseVisitor::visitGraphNode(
    SparqlParser::GraphNodeContext* ctx) {
  // graphNode : varOrTerm | triplesNode
  // varOrTerm is a string, triplesNode is not yet supported
  return visitChildren(ctx);
}

// _________________________________________________________________________________________________
vector<std::pair<string, string>> SparqlBaseVisitor::visitPropertyListNotEmpty(
    SparqlParser::PropertyListNotEmptyContext* ctx) {
  // propertyListNotEmpty : verb objectList ( ';' ( verb objectList )? )*
  std::vector<std::pair<std::string, std::string>> predObs;
  auto obs = ctx->objectList();
  auto preds = ctx->verb();
  AD_CHECK(obs.size() == preds.size());
  for (size_t i = 0; i <= obs.size(); ++i) {
    std::string pred = visitVerb(preds[i]);
    std::vector<std::string> objList = visitObjectList(obs[i]);
    for (auto& o : objList) {
      predObs.emplace_back(pred, std::move(o));
    }
  }
  return predObs;
}

vector<array<string, 3>> SparqlBaseVisitor::visitTriplesSameSubject(
    SparqlParser::TriplesSameSubjectContext* ctx) {
  // triplesSameSubject : varOrTerm propertyListNotEmpty | triplesNode
  // propertyList
  if (auto v = ctx->varOrTerm(); v) {
    std::string subject = visitVarOrTerm(v);
    auto p = ctx->propertyListNotEmpty();
    auto predObs = visitPropertyListNotEmpty(p);

    std::vector<std::array<std::string, 3>> triples;
    triples.reserve(predObs.size());
    for (auto& po : predObs) {
      triples.push_back({subject, std::move(po.first), std::move(po.second)});
    }

    return triples;
  } else {
    throw NotImplementedException("triples with anonymous or blank nodes");
  }
}

// _______________________________________________________________________
vector<array<string, 3>> SparqlBaseVisitor::visitConstructTriples(
    SparqlParser::ConstructTriplesContext* ctx) {
  // constructTriples : triplesSameSubject ( '.' constructTriples? )?
  std::vector<std::array<std::string, 3>> triples =
      visitTriplesSameSubject(ctx->triplesSameSubject());
  if (auto t = ctx->constructTriples(); t) {
    auto otherTriples = visitConstructTriples(t);
    triples.insert(triples.end(), std::make_move_iterator(otherTriples.begin()),
                   std::make_move_iterator(otherTriples.end()));
  }
  return triples;
}

// __________________________________________________________________
vector<array<string, 3>> SparqlBaseVisitor::visitTriplesBlock(
    SparqlParser::TriplesBlockContext* ctx) {
  // triplesBlock: triplesSameSubject ( '.' triplesBlock? )?
  std::vector<std::array<std::string, 3>> triples =
      visitTriplesSameSubject(ctx->triplesSameSubject());
  if (auto t = ctx->triplesBlock(); t) {
    auto otherTriples = visitTriplesBlock(t);
    triples.insert(triples.end(), std::make_move_iterator(otherTriples.begin()),
                   std::make_move_iterator(otherTriples.end()));
  }
  return triples;
}

// ___________________________________________________________________________
antlrcpp::Any SparqlBaseVisitor::visitConstructQuery(
    SparqlParser::ConstructQueryContext* ctx) {
  (void)ctx;
  throw NotImplementedException("Construct Queries");
}

// __________________________________________________________________________
antlrcpp::Any SparqlBaseVisitor::visitDescribeQuery(
    SparqlParser::DescribeQueryContext* ctx) {
  (void)ctx;
  throw NotImplementedException("DescribeQuery");
}

// __________________________________________________________________________
antlrcpp::Any SparqlBaseVisitor::visitAskQuery(
    SparqlParser::AskQueryContext* ctx) {
  (void)ctx;
  throw NotImplementedException("AskQuery");
}

// ___________________________________________________________________________
antlrcpp::Any SparqlBaseVisitor::visitDatasetClause(
    SparqlParser::DatasetClauseContext* ctx) {
  // used only by not implemented query type
  return visitChildren(ctx);
}

// ______________________________________________________________________________
antlrcpp::Any SparqlBaseVisitor::visitDefaultGraphClause(
    SparqlParser::DefaultGraphClauseContext* ctx) {
  return visitChildren(ctx);
}

// ______________________________________________________________________
antlrcpp::Any SparqlBaseVisitor::visitNamedGraphClause(
    SparqlParser::NamedGraphClauseContext* ctx) {
  // used only by not implemented query type
  return visitChildren(ctx);
}

// _________________________________________________________________________
antlrcpp::Any SparqlBaseVisitor::visitSourceSelector(
    SparqlParser::SourceSelectorContext* ctx) {
  // used only by not implemented query type

  return visitChildren(ctx);
}

string SparqlBaseVisitor::visitIriRefOrFunction(
        SparqlParser::IriRefOrFunctionContext *ctx) {
// currently only able to return an iriRef, thus it is a std::string

//iriRefOrFunction: iriRef argList? | VARNAME argList?
if (ctx->argList()) {
throw NotImplementedException("nested function call within a filter expression");
}
if (ctx->VARNAME()) {
throw NotImplementedException("VARNAME within a filter Expression");
}
return visitIriRef(ctx->iriRef());
}
