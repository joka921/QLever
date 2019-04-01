//
// Created by johannes on 01.04.19.
//

#include <iostream>

#include "antlr4-runtime.h"
#include "parser/SparqlParser/SparqlBaseVisitor.h"
#include "parser/SparqlParser/antlr4-runtime/SparqlLexer.h"
#include "parser/SparqlParser/antlr4-runtime/SparqlParser.h"

using namespace std;
using namespace antlr4;

int main(int argc, const char* argv[]) {
  (void)argc;
  (void)argv;
  std::ifstream stream;
  stream.open("input.scene");

  ANTLRInputStream input(
      "PREFIX wd:<http://wikidata.org/> SELECT ?x WHERE { ?x wd:blubb -4389343 "
      ". ?x <alpha> \"something\"@en }");
  SparqlLexer lexer(&input);
  CommonTokenStream tokens(&lexer);
  SparqlParser parser(&tokens);

  auto queryTree = parser.query();
  SparqlBaseVisitor visitor;
  auto res = visitor.visitQuery(queryTree);

  return 0;
}