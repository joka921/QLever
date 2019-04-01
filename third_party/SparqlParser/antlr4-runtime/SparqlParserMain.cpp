//
// Created by johannes on 01.04.19.
//


#include <iostream>

#include "SparqlLexer.h"
#include "SparqlParser.h"
#include "SparqlBaseVisitor.h"
#include "antlr4-runtime.h"

using namespace std;
using namespace antlr4;

int main(int argc, const char* argv[]) {
  std::ifstream stream;
  stream.open("input.scene");

  ANTLRInputStream input("PREFIX wd:<http://wikidata.org/> SELECT ?x WHERE { ?x wd:blubb 4.389  }");
  SparqlLexer lexer(&input);
  CommonTokenStream tokens(&lexer);
  SparqlParser parser(&tokens);

  auto queryTree = parser.query();
  SparqlBaseVisitor visitor;
  auto res = visitor.visitQuery(queryTree);

  return 0;
}