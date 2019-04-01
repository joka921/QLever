set (SparqlParserSrc ../SparqlBaseVisitor.cpp SparqlLexer.cpp
        SparqlParser.cpp SparqlVisitor.cpp)

add_library(sparqlParser ${SparqlParserSrc})
target_compile_options(sparqlParser PRIVATE "-Wno-attributes")
target_link_libraries(sparqlParser antlr4_static)

