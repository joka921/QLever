add_library(rdfEscaping RdfEscaping.h RdfEscaping.cpp)
target_link_libraries(rdfEscaping ${ICU_LIBRARIES})
add_library(parser
        SparqlParser.h SparqlParser.cpp
        ParsedQuery.h ParsedQuery.cpp
        ParseException.h
        TsvParser.h TsvParser.cpp
        NTriplesParser.h NTriplesParser.cpp
        TurtleParser.h TurtleParser.cpp
        Tokenizer.h Tokenizer.cpp
        ContextFileParser.cpp ContextFileParser.h
        ParallelParseBuffer.h
        PropertyPathParser.h PropertyPathParser.cpp
        SparqlLexer.h SparqlLexer.cpp)
target_link_libraries(parser rdfEscaping re2 absl::flat_hash_map)
