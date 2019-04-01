
// Generated from Sparql.g4 by ANTLR 4.7.1


#include "SparqlBaseVisitor.h"


antlrcpp::Any SparqlBaseVisitor::visitNumericLiteralUnsigned(SparqlParser::NumericLiteralUnsignedContext *ctx) {

if (ctx->DECIMAL()) {
auto d = ctx->DECIMAL();
double res = std::stod(d->getText());
std::cerr << "Parsed a decimal with value " << res << std::endl;
return res;

}
return visitChildren(ctx);
}
