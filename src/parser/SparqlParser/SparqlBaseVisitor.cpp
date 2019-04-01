
// Generated from Sparql.g4 by ANTLR 4.7.1

#include "SparqlBaseVisitor.h"

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
