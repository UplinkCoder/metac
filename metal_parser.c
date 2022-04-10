#include "metal_parser.h"
#include <string.h>
#include <stdlib.h>
#include <assert.h>
#include "cache/crc32.c"

void MetalParserInitFromLexer(metal_parser_t* self, metal_lexer_t* lexer)
{
    self->Lexer = lexer;
    self->CurrentTokenIndex = 0;
}

metal_token_t* MetalParserNextToken(metal_parser_t* self)
{
    metal_token_t* result = 0;
    assert(self->Lexer->tokens_size);

    if (self->CurrentTokenIndex < self->Lexer->tokens_size)
    {
        result = self->Lexer->tokens + self->CurrentTokenIndex++;
    }
    else
    {
        // TODO Error
    }

    return result;
}
uint32_t _newExp_size = 0;
uint32_t _newExp_capacity = 0;
metal_expression_t* _newExp_mem = 0;

#ifndef ALIGN4
#  define ALIGN4(N) \
      (((N) + 3) & ~3)
#endif

metal_binary_expression_type_t BinExpTypeFromTokenType(metal_token_enum_t tokenType)
{
    metal_binary_expression_type_t result = bin_invalid;

    switch(tokenType)
    {
        case tok_plus :
            return bin_add;
        case tok_minus :
            return bin_sub;
        case tok_div :
            return bin_div;
        case tok_cat :
            return bin_cat;
        case tok_cat_ass :
            return bin_catass;
        case tok_assign :
            return bin_assign;
        case tok_equalsEquals :
            return bin_eq;
        case tok_notEqual :
            return bin_neq;

        case tok_star :
            return bin_mul;
    }
}

metal_expression_type_t ExpTypeFromTokenType(metal_token_enum_t tokenType)
{
    if (tokenType == tok_unsignedNumber)
    {
        return exp_signed_integer;
    }
    else if (tokenType == tok_stringLiteral)
    {
        return exp_string;
    }

    else if (tokenType == tok_kw_inject)
    {
        return exp_inject;
    }
    else if (tokenType == tok_kw_eject)
    {
        return exp_eject;
    }
    else if (tokenType == tok_kw_assert)
    {
        return exp_assert;
    }
    else if (tokenType == tok_dollar)
    {
        return exp_outer;
    }
    else if (tokenType == tok_addr)
    {
        return exp_addr;
    }
    else if (tokenType == tok_star)
    {
        return exp_ptr_or_mul;
    }
    else
    {
        assert(0);
        return exp_invalid;
    }

}

metal_expression_t* AllocNewExpression(metal_expression_type_t t)
{
    metal_expression_t* result = 0;

    if(!_newExp_mem)
    {
        _newExp_mem = cast(metal_expression_t*) malloc(sizeof(metal_expression_t) * 4096);
        _newExp_capacity = 4096;
    }

    if (_newExp_capacity < _newExp_size)
    {
        _newExp_capacity = ALIGN4(cast(uint32_t) (_newExp_capacity * 1.6f));
        _newExp_mem = cast(metal_expression_t*) realloc(_newExp_mem, _newExp_capacity);
    }
    else
    {
        result = _newExp_mem + _newExp_size++;
        result->Type = t;
    }

    return result;
}

const char* MetalTokenEnum_toChars(metal_token_enum_t t);

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
        line += eaten_chars;
        line_length -= eaten_chars;
    }
}

bool isOperator(metal_token_enum_t t)
{
    return (t >= tok_comma && t < tok_kw_struct);
}

uint32_t mix(uint32_t a, uint32_t b)
{
    return a ^ b;
}

metal_expression_t* parseExpression(metal_parser_t* self, metal_expression_t* prev)
{
    metal_expression_t* result = 0;

    metal_token_t* currentToken = MetalParserNextToken(self);
    metal_token_enum_t tokenType =
        (currentToken ? currentToken->TokenType : tok_invalid);

    if (tokenType == tok_unsignedNumber)
    {
        result = AllocNewExpression(exp_signed_integer);
        result->ValueI64 = currentToken->ValueU64;
        result->Hash = crc32c(~0, &result->ValueU64, sizeof(result->ValueI64));
    }
    else if (tokenType == tok_stringLiteral)
    {
        // result = GetOrAddStringLiteral(_string_table, currentToken);

        result = AllocNewExpression(exp_string);
        result->String = currentToken->String;
        result->Length = currentToken->Length;
        result->Hash = currentToken->StringKey;
    }
    else if (tokenType == tok_kw_inject)
    {
        result = AllocNewExpression(exp_eject);
        result->E1 = parseExpression(self, result);
        result->Hash = mix(
            crc32c(~0, "inject", sizeof("inject") - 1),
            result->E1->Hash
        );
    }
    else
    {
        assert(0);
    }

    printf("TokenType: %s\n", MetalTokenEnum_toChars(tokenType));
    currentToken = MetalParserNextToken(self);
    tokenType =
        (currentToken ? currentToken->TokenType : tok_invalid);
    printf("Next TokenType: %s\n", MetalTokenEnum_toChars(tokenType));

    if (isOperator(tokenType))
    {
        printf("It's an operator\n");
        tokenType = currentToken->TokenType;
        metal_expression_t* E1 = result;
        metal_expression_t* E2 = parseExpression(self, 0);

        result = AllocNewExpression(exp_binary + (tokenType - tok_comma));
    }
}

metal_expression_t* parseExpressionFromString(const char* exp)
{
    metal_lexer_t lexer;
    metal_parser_t parser;
    metal_lexer_state_t lexer_state =
        MetalLexerStateFromString(0, exp);

    InitMetalLexer(&lexer);
    LexString(&lexer, exp);

    MetalParserInitFromLexer(&parser, &lexer);

    metal_expression_t* result = parseExpression(&parser, 0);
    return result;
}
