#include "metal_parser.h"
#include <string.h>
#include <assert.h>

void MetalParserInitFromLexer(metal_parser_t* self, metal_lexer_t* lexer)
{
    self->lexer = lexer;
    self->currentTokenIndex = 0;
}

metal_token_t* MetalParserNextToken(metal_parser_t* self)
{
    metal_token_t* result = 0;

    if (self->currentTokenIndex < self->lexer->tokens_size)
    {
        result = self->lexer->tokens + self->currentTokenIndex++;
    }
    else
    {
        // TODO Error
    }

    return result;
}
uint32_t _newExp_idx = 0;
uint32_t _newExp_size = 0;
metal_expression_t* _newExp_mem = 0;

metal_expression_t*  allocNewExpression(metal_token_enum_t t)
{
    if(!_newExp_mem)
    {
    }
}

const char* MetalTokenEnum_toChars(metal_token_enum_t t);

metal_expression_t* parseExpression(metal_parser_t* self, metal_expression_t* prev)
{
    metal_expression_t* result = 0;

    metal_token_t* currentToken = MetalParserNextToken(self);
    metal_token_enum_t tokenType =
        (currentToken ? currentToken->TokenType : tok_invalid);

    if (tokenType == tok_unsignedNumber)
    {
        result = allocNewExpression(exp_signed_integer);
        result->ValueI64 = currentToken->ValueU64;
    }
    else
    {
        printf("TokenType: %s\n", MetalTokenEnum_toChars(tokenType));
        assert(0);
    }
}

static inline void LexString(metal_lexer_t* lexer, const char* line)
{
    uint32_t line_length = strlen(line);
    metal_lexer_state_t lexer_state =
        MetalLexerStateFromString(0, line);
    
    

    while(line_length > 0)
    {
        uint32_t initialPosition = lexer_state.Position;
        
        metal_token_t token =
            *MetalLexerLexNextToken(lexer, &lexer_state, line, line_length);
        
        uint32_t eaten_chars = lexer_state.Position - initialPosition;
        lexer += eaten_chars;
        line_length -= eaten_chars;
    }
}

metal_expression_t* parseExpressionFromString(const char* exp)
{
    metal_lexer_t lexer;
    metal_parser_t parser;
    metal_lexer_state_t lexer_state =
        MetalLexerStateFromString(0, exp);

    InitMetalLexer(&lexer);

    MetalParserInitFromLexer(&parser, &lexer);

    metal_expression_t* result = parseExpression(&parser, 0);
    return result;
}
