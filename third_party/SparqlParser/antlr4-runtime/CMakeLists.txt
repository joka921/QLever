set (SparqlParserSrc SparqlBaseVisitor.cpp SparqlLexer.cpp
                     SparqlParser.cpp SparqlVisitor.cpp)

add_library(sparqlParser ${SparqlParserSrc})
target_link_libraries(sparqlParser antlr4_static)

add_executable(SparqlParserMain SparqlParserMain.cpp)
target_link_libraries(SparqlParserMain sparqlParser antlr4_static)