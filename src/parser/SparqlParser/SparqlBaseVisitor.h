
// Generated from Sparql.g4 by ANTLR 4.7.1

#pragma once

#include "antlr4-runtime.h"
#include "antlr4-runtime/SparqlVisitor.h"

#include <iostream>
#include "../../util/Log.h"

/**
 * This class provides an empty implementation of SparqlVisitor, which can be
 * extended to create a visitor which only needs to handle a subset of the
 * available methods.
 */
class SparqlBaseVisitor : public SparqlVisitor {
 public:
  class NotImplementedException : public std::exception {
   public:
    NotImplementedException(const std::string& feature) {
      _message = "The feature " + feature +
                 " is not yet implemented in this SparqlParser";
    }
    virtual const char* what() const noexcept override {
      return _message.c_str();
    };

   private:
    std::string _message;
  };
  class ParseException : public std::exception {
   public:
    ParseException(std::string_view message) : _message(message) {}
    virtual const char* what() const noexcept override {
      return _message.c_str();
    };

   private:
    std::string _message;
  };

  virtual antlrcpp::Any visitQuery(SparqlParser::QueryContext* ctx) override {
    return visitChildren(ctx);
  }

  virtual antlrcpp::Any visitQueryBody(
      SparqlParser::QueryBodyContext* ctx) override {
    return visitChildren(ctx);
  }

  virtual antlrcpp::Any visitPrologue(
      SparqlParser::PrologueContext* ctx) override {
    return visitChildren(ctx);
  }

  virtual antlrcpp::Any visitBaseDecl(
      SparqlParser::BaseDeclContext* ctx) override {
    return visitChildren(ctx);
  }

  virtual antlrcpp::Any visitPrefixDecl(
      SparqlParser::PrefixDeclContext* ctx) override {
    auto prefix = ctx->PNAME_NS()->toString();
    // strip the colon at end of short name
    prefix = prefix.substr(0, prefix.size() - 1);
    // strip the angles from the remainder
    auto rem = ctx->IRI_REF()->toString();
    rem = rem.substr(1, rem.size() - 2);
    _prefixes[prefix] = rem;
    LOG(TRACE) << "Found a new Prefix: " << prefix << " " << rem << std::endl;
    return 0;
  }

  virtual antlrcpp::Any visitSelectQuery(
      SparqlParser::SelectQueryContext* ctx) override {
    return visitChildren(ctx);
  }

  virtual antlrcpp::Any visitSelectors(
      SparqlParser::SelectorsContext* ctx) override {
    return visitChildren(ctx);
  }

  virtual antlrcpp::Any visitAlias(SparqlParser::AliasContext* ctx) override {
    return visitChildren(ctx);
  }

  virtual antlrcpp::Any visitConstructQuery(
      SparqlParser::ConstructQueryContext* ctx) override;

  virtual antlrcpp::Any visitDescribeQuery(
      SparqlParser::DescribeQueryContext* ctx) override;

  virtual antlrcpp::Any visitAskQuery(
      SparqlParser::AskQueryContext* ctx) override;

  virtual antlrcpp::Any visitDatasetClause(
      SparqlParser::DatasetClauseContext* ctx) override;

  virtual antlrcpp::Any visitDefaultGraphClause(
      SparqlParser::DefaultGraphClauseContext* ctx) override;

  virtual antlrcpp::Any visitNamedGraphClause(
      SparqlParser::NamedGraphClauseContext* ctx) override;

  virtual antlrcpp::Any visitSourceSelector(
      SparqlParser::SourceSelectorContext* ctx) override;

  virtual antlrcpp::Any visitWhereClause(
      SparqlParser::WhereClauseContext* ctx) override {
    return visitChildren(ctx);
  }

  virtual antlrcpp::Any visitSolutionModifier(
      SparqlParser::SolutionModifierContext* ctx) override {
    return visitChildren(ctx);
  }

  virtual antlrcpp::Any visitLimitOffsetClauses(
      SparqlParser::LimitOffsetClausesContext* ctx) override {
    return visitChildren(ctx);
  }

  virtual antlrcpp::Any visitOrderClause(
      SparqlParser::OrderClauseContext* ctx) override {
    return visitChildren(ctx);
  }

  virtual antlrcpp::Any visitGroupClause(
      SparqlParser::GroupClauseContext* ctx) override {
    return visitChildren(ctx);
  }

  virtual antlrcpp::Any visitHavingClause(
      SparqlParser::HavingClauseContext* ctx) override {
    return visitChildren(ctx);
  }

  virtual antlrcpp::Any visitOrderCondition(
      SparqlParser::OrderConditionContext* ctx) override {
    return visitChildren(ctx);
  }

  virtual antlrcpp::Any visitLimitClause(
      SparqlParser::LimitClauseContext* ctx) override {
    return visitChildren(ctx);
  }

  virtual antlrcpp::Any visitOffsetClause(
      SparqlParser::OffsetClauseContext* ctx) override {
    return visitChildren(ctx);
  }

  virtual antlrcpp::Any visitGroupGraphPattern(
      SparqlParser::GroupGraphPatternContext* ctx) override {
    return visitChildren(ctx);
  }

  virtual vector<array<string, 3>> visitTriplesBlock(
      SparqlParser::TriplesBlockContext* ctx) override;

  virtual antlrcpp::Any visitGraphPatternNotTriples(
      SparqlParser::GraphPatternNotTriplesContext* ctx) override {
    return visitChildren(ctx);
  }

  virtual antlrcpp::Any visitOptionalGraphPattern(
      SparqlParser::OptionalGraphPatternContext* ctx) override {
    return visitChildren(ctx);
  }

  virtual antlrcpp::Any visitGraphGraphPattern(
      SparqlParser::GraphGraphPatternContext* ctx) override {
    return visitChildren(ctx);
  }

  virtual antlrcpp::Any visitGroupOrUnionGraphPattern(
      SparqlParser::GroupOrUnionGraphPatternContext* ctx) override {
    return visitChildren(ctx);
  }

  virtual antlrcpp::Any visitFilter(SparqlParser::FilterContext* ctx) override {
    return visitChildren(ctx);
  }

  virtual antlrcpp::Any visitConstraint(
      SparqlParser::ConstraintContext* ctx) override {
    return visitChildren(ctx);
  }

  virtual antlrcpp::Any visitFunctionCall(
      SparqlParser::FunctionCallContext* ctx) override {
    return visitChildren(ctx);
  }

  virtual antlrcpp::Any visitArgList(
      SparqlParser::ArgListContext* ctx) override {
    return visitChildren(ctx);
  }

  virtual antlrcpp::Any visitConstructTemplate(
      SparqlParser::ConstructTemplateContext* ctx) override {
    return visitChildren(ctx);
  }

  virtual vector<array<string, 3>> visitConstructTriples(
      SparqlParser::ConstructTriplesContext* ctx) override;

  virtual vector<array<string, 3>> visitTriplesSameSubject(
      SparqlParser::TriplesSameSubjectContext* ctx) override;

  virtual vector<std::pair<string, string>> visitPropertyListNotEmpty(
      SparqlParser::PropertyListNotEmptyContext* ctx) override;

  virtual antlrcpp::Any visitPropertyList(
      SparqlParser::PropertyListContext* ctx) override {
    return visitChildren(ctx);
  }

  virtual vector<string> visitObjectList(
      SparqlParser::ObjectListContext* ctx) override;

  virtual antlrcpp::Any visitObject(SparqlParser::ObjectContext* ctx) override {
    return visitChildren(ctx);
  }

  virtual string visitVerb(SparqlParser::VerbContext* ctx) override;

  virtual antlrcpp::Any visitTriplesNode(
      SparqlParser::TriplesNodeContext* ctx) override;

  virtual antlrcpp::Any visitBlankNodePropertyList(
      SparqlParser::BlankNodePropertyListContext* ctx) override {
    return visitChildren(ctx);
  }

  virtual antlrcpp::Any visitCollection(
      SparqlParser::CollectionContext* ctx) override {
    return visitChildren(ctx);
  }

  virtual antlrcpp::Any visitGraphNode(
      SparqlParser::GraphNodeContext* ctx) override;

  virtual antlrcpp::Any visitVarOrTerm(
      SparqlParser::VarOrTermContext* ctx) override;

  virtual antlrcpp::Any visitVarOrIRIref(
      SparqlParser::VarOrIRIrefContext* ctx) override;

  virtual antlrcpp::Any visitVar(SparqlParser::VarContext* ctx) override;

  virtual antlrcpp::Any visitGraphTerm(
      SparqlParser::GraphTermContext* ctx) override;

  virtual antlrcpp::Any visitExpression(
      SparqlParser::ExpressionContext* ctx) override {
    return visitChildren(ctx);
  }

  virtual antlrcpp::Any visitConditionalOrExpression(
      SparqlParser::ConditionalOrExpressionContext* ctx) override {
    return visitChildren(ctx);
  }

  virtual antlrcpp::Any visitConditionalAndExpression(
      SparqlParser::ConditionalAndExpressionContext* ctx) override {
    return visitChildren(ctx);
  }

  virtual antlrcpp::Any visitValueLogical(
      SparqlParser::ValueLogicalContext* ctx) override {
    return visitChildren(ctx);
  }

  virtual antlrcpp::Any visitRelationalExpression(
      SparqlParser::RelationalExpressionContext* ctx) override {
    return visitChildren(ctx);
  }

  virtual antlrcpp::Any visitNumericExpression(
      SparqlParser::NumericExpressionContext* ctx) override {
    return visitChildren(ctx);
  }

  virtual antlrcpp::Any visitAdditiveExpression(
      SparqlParser::AdditiveExpressionContext* ctx) override {
    return visitChildren(ctx);
  }

  virtual antlrcpp::Any visitMultiplicativeExpression(
      SparqlParser::MultiplicativeExpressionContext* ctx) override {
    return visitChildren(ctx);
  }

  virtual antlrcpp::Any visitUnaryExpression(
      SparqlParser::UnaryExpressionContext* ctx) override {
    return visitChildren(ctx);
  }

  virtual antlrcpp::Any visitPrimaryExpression(
      SparqlParser::PrimaryExpressionContext* ctx) override {
    return visitChildren(ctx);
  }

  virtual antlrcpp::Any visitBrackettedExpression(
      SparqlParser::BrackettedExpressionContext* ctx) override {
    return visitChildren(ctx);
  }

  virtual antlrcpp::Any visitRegexExpression(
      SparqlParser::RegexExpressionContext* ctx) override {
    return visitChildren(ctx);
  }

  virtual antlrcpp::Any visitIriRefOrFunction(
      SparqlParser::IriRefOrFunctionContext* ctx) override {
    return visitChildren(ctx);
  }

  virtual antlrcpp::Any visitRdfLiteral(
      SparqlParser::RdfLiteralContext* ctx) override;

  virtual antlrcpp::Any visitNumericLiteral(
      SparqlParser::NumericLiteralContext* ctx) override {
    return visitChildren(ctx);
  }

  virtual antlrcpp::Any visitNumericLiteralUnsigned(
      SparqlParser::NumericLiteralUnsignedContext* ctx) override;

  virtual antlrcpp::Any visitNumericLiteralPositive(
      SparqlParser::NumericLiteralPositiveContext* ctx) override;

  virtual antlrcpp::Any visitNumericLiteralNegative(
      SparqlParser::NumericLiteralNegativeContext* ctx) override;

  virtual antlrcpp::Any visitBooleanLiteral(
      SparqlParser::BooleanLiteralContext* ctx) override {
    return visitChildren(ctx);
  }

  virtual antlrcpp::Any visitString(SparqlParser::StringContext* ctx) override;

  virtual antlrcpp::Any visitIriRef(SparqlParser::IriRefContext* ctx) override;

  virtual antlrcpp::Any visitPrefixedName(
      SparqlParser::PrefixedNameContext* ctx) override;

  virtual antlrcpp::Any visitBlankNode(
      SparqlParser::BlankNodeContext* ctx) override;

 private:
  struct Number {
    enum class type { INTEGER, DECIMAL };

    Number() = default;
    Number(type t, double v) : _value(v), _type(t) {}
    double _value;
    type _type;
  };

  std::unordered_map<std::string, std::string> _prefixes;
};
