
// Generated from Sparql.g4 by ANTLR 4.7.1

#pragma once


#include "antlr4-runtime.h"
#include "SparqlVisitor.h"
#include <iostream>



/**
 * This class provides an empty implementation of SparqlVisitor, which can be
 * extended to create a visitor which only needs to handle a subset of the available methods.
 */
class  SparqlBaseVisitor : public SparqlVisitor {
public:

  virtual antlrcpp::Any visitQuery(SparqlParser::QueryContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual antlrcpp::Any visitQueryBody(SparqlParser::QueryBodyContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual antlrcpp::Any visitPrologue(SparqlParser::PrologueContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual antlrcpp::Any visitBaseDecl(SparqlParser::BaseDeclContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual antlrcpp::Any visitPrefixDecl(SparqlParser::PrefixDeclContext *ctx) override {
    auto prefix = ctx->PNAME_NS()->toString();
    // strip the colon at end of short name
    prefix = prefix.substr(0, prefix.size() - 1);
    // strip the angles from the remainder
    auto rem = ctx->IRI_REF()->toString();
    rem = rem.substr(1, rem.size() - 2);
    _prefixes[prefix] = rem;
    std::cerr << "Found a new Prefix: " << prefix << " " << rem << std::endl;
    return 0;
  }

  virtual antlrcpp::Any visitSelectQuery(SparqlParser::SelectQueryContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual antlrcpp::Any visitSelectors(SparqlParser::SelectorsContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual antlrcpp::Any visitAlias(SparqlParser::AliasContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual antlrcpp::Any visitConstructQuery(SparqlParser::ConstructQueryContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual antlrcpp::Any visitDescribeQuery(SparqlParser::DescribeQueryContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual antlrcpp::Any visitAskQuery(SparqlParser::AskQueryContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual antlrcpp::Any visitDatasetClause(SparqlParser::DatasetClauseContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual antlrcpp::Any visitDefaultGraphClause(SparqlParser::DefaultGraphClauseContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual antlrcpp::Any visitNamedGraphClause(SparqlParser::NamedGraphClauseContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual antlrcpp::Any visitSourceSelector(SparqlParser::SourceSelectorContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual antlrcpp::Any visitWhereClause(SparqlParser::WhereClauseContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual antlrcpp::Any visitSolutionModifier(SparqlParser::SolutionModifierContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual antlrcpp::Any visitLimitOffsetClauses(SparqlParser::LimitOffsetClausesContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual antlrcpp::Any visitOrderClause(SparqlParser::OrderClauseContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual antlrcpp::Any visitGroupClause(SparqlParser::GroupClauseContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual antlrcpp::Any visitHavingClause(SparqlParser::HavingClauseContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual antlrcpp::Any visitOrderCondition(SparqlParser::OrderConditionContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual antlrcpp::Any visitLimitClause(SparqlParser::LimitClauseContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual antlrcpp::Any visitOffsetClause(SparqlParser::OffsetClauseContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual antlrcpp::Any visitGroupGraphPattern(SparqlParser::GroupGraphPatternContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual antlrcpp::Any visitTriplesBlock(SparqlParser::TriplesBlockContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual antlrcpp::Any visitGraphPatternNotTriples(SparqlParser::GraphPatternNotTriplesContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual antlrcpp::Any visitOptionalGraphPattern(SparqlParser::OptionalGraphPatternContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual antlrcpp::Any visitGraphGraphPattern(SparqlParser::GraphGraphPatternContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual antlrcpp::Any visitGroupOrUnionGraphPattern(SparqlParser::GroupOrUnionGraphPatternContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual antlrcpp::Any visitFilter(SparqlParser::FilterContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual antlrcpp::Any visitConstraint(SparqlParser::ConstraintContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual antlrcpp::Any visitFunctionCall(SparqlParser::FunctionCallContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual antlrcpp::Any visitArgList(SparqlParser::ArgListContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual antlrcpp::Any visitConstructTemplate(SparqlParser::ConstructTemplateContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual antlrcpp::Any visitConstructTriples(SparqlParser::ConstructTriplesContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual antlrcpp::Any visitTriplesSameSubject(SparqlParser::TriplesSameSubjectContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual antlrcpp::Any visitPropertyListNotEmpty(SparqlParser::PropertyListNotEmptyContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual antlrcpp::Any visitPropertyList(SparqlParser::PropertyListContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual antlrcpp::Any visitObjectList(SparqlParser::ObjectListContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual antlrcpp::Any visitObject(SparqlParser::ObjectContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual antlrcpp::Any visitVerb(SparqlParser::VerbContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual antlrcpp::Any visitTriplesNode(SparqlParser::TriplesNodeContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual antlrcpp::Any visitBlankNodePropertyList(SparqlParser::BlankNodePropertyListContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual antlrcpp::Any visitCollection(SparqlParser::CollectionContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual antlrcpp::Any visitGraphNode(SparqlParser::GraphNodeContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual antlrcpp::Any visitVarOrTerm(SparqlParser::VarOrTermContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual antlrcpp::Any visitVarOrIRIref(SparqlParser::VarOrIRIrefContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual antlrcpp::Any visitVar(SparqlParser::VarContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual antlrcpp::Any visitGraphTerm(SparqlParser::GraphTermContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual antlrcpp::Any visitExpression(SparqlParser::ExpressionContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual antlrcpp::Any visitConditionalOrExpression(SparqlParser::ConditionalOrExpressionContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual antlrcpp::Any visitConditionalAndExpression(SparqlParser::ConditionalAndExpressionContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual antlrcpp::Any visitValueLogical(SparqlParser::ValueLogicalContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual antlrcpp::Any visitRelationalExpression(SparqlParser::RelationalExpressionContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual antlrcpp::Any visitNumericExpression(SparqlParser::NumericExpressionContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual antlrcpp::Any visitAdditiveExpression(SparqlParser::AdditiveExpressionContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual antlrcpp::Any visitMultiplicativeExpression(SparqlParser::MultiplicativeExpressionContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual antlrcpp::Any visitUnaryExpression(SparqlParser::UnaryExpressionContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual antlrcpp::Any visitPrimaryExpression(SparqlParser::PrimaryExpressionContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual antlrcpp::Any visitBrackettedExpression(SparqlParser::BrackettedExpressionContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual antlrcpp::Any visitRegexExpression(SparqlParser::RegexExpressionContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual antlrcpp::Any visitIriRefOrFunction(SparqlParser::IriRefOrFunctionContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual antlrcpp::Any visitRdfLiteral(SparqlParser::RdfLiteralContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual antlrcpp::Any visitNumericLiteral(SparqlParser::NumericLiteralContext *ctx) override {
    return visitChildren(ctx);
  }


  virtual antlrcpp::Any visitNumericLiteralUnsigned(SparqlParser::NumericLiteralUnsignedContext *ctx) override;

  virtual antlrcpp::Any visitNumericLiteralPositive(SparqlParser::NumericLiteralPositiveContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual antlrcpp::Any visitNumericLiteralNegative(SparqlParser::NumericLiteralNegativeContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual antlrcpp::Any visitBooleanLiteral(SparqlParser::BooleanLiteralContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual antlrcpp::Any visitString(SparqlParser::StringContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual antlrcpp::Any visitIriRef(SparqlParser::IriRefContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual antlrcpp::Any visitPrefixedName(SparqlParser::PrefixedNameContext *ctx) override {
    if (ctx->PNAME_LN()) {
      auto val = ctx->PNAME_LN()->toString();
      auto pos = val.find(':');
      auto prefix = val.substr(0, pos);
      auto rem = val.substr(pos + 1);
      if (!_prefixes.count(prefix)) {
        throw std::runtime_error("prefix " + prefix + " was used in query but not declared in prologue");
      }
      auto res = '<' + _prefixes[prefix] + rem + '>';
      std::cerr << "Expanded prefix to " << res << "\n";
      return res;
    } {
      throw std::runtime_error("The PNAME_NS case for prefixedName rule is not implemented in the sparql parser");
    }
  }

  virtual antlrcpp::Any visitBlankNode(SparqlParser::BlankNodeContext *ctx) override {
    return visitChildren(ctx);
  }

private:

  std::unordered_map<std::string, std::string> _prefixes;

};

