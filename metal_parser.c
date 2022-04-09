#include "metal_parser.h"

void MetalParserInitFromLexer(metal_parser_t* self, metal_lexer_t* lexer)
{
    self->lexer = lexer;
    self->currentTokenIndex = 0;
}

metal_expression_t* parseExpression(metal_parser_t* self, metal_expression_t* prev);
metal_expression_t* parseExpressionFromString(const char* exp)
{
    metal_lexer_t lexer;
    metal_parser_t parser;
    metal_lexer_state_t lexer_state =
        MetalLexerStateFromString(0, exp);

    InitMetalLexer(&lexer);

    MetalParserInitFromLexer(&parser, &lexer);

}
