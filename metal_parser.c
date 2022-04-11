#include "metal_parser.h"
#include <string.h>
#include <stdlib.h>
#include <assert.h>
#include "cache/crc32.c"
const char* MetalTokenEnum_toChars(metal_token_enum_t t);


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

metal_token_t* MetalParserPeekToken(metal_parser_t* self, int32_t p)
{
    metal_token_t* result = 0;
    assert(self->Lexer->tokens_size);

    if (cast(uint32_t)(self->CurrentTokenIndex + (p - 1)) < self->Lexer->tokens_size)
    {
        result = self->Lexer->tokens + self->CurrentTokenIndex + (p - 1);
    }
    else
    {
        // TODO Error
    }

    return result;
}


void MetalParserMatch(metal_parser_t* self, metal_token_enum_t type)
{
    metal_token_t* token = MetalParserNextToken(self);
    metal_token_enum_t got = token->TokenType;
    if (got != type)
    {
        printf("Expected: %s -- Got: %s\n", 
            MetalTokenEnum_toChars(type), MetalTokenEnum_toChars(got));
    }
}

uint32_t _newExp_size = 0;
uint32_t _newExp_capacity = 0;
metal_expression_t* _newExp_mem = 0;

#ifndef ALIGN4
#  define ALIGN4(N) \
      (((N) + 3) & ~3)
#endif

metal_expression_type_t BinExpTypeFromTokenType(metal_token_enum_t tokenType)
{
    metal_expression_type_t result = exp_invalid;

    switch(tokenType)
    {
        case tok_plus :
            return exp_add;
        case tok_minus :
            return exp_sub;
        case tok_div :
            return exp_div;
        case tok_cat :
            return exp_cat;
        case tok_cat_ass :
            return exp_catass;
        case tok_assign :
            return exp_assign;
        case tok_equalsEquals :
            return exp_eq;
        case tok_notEqual :
            return exp_neq;
        case tok_dot :
            return exp_dot;
        case tok_arrow :
            return exp_arrow;
        case tok_spaceship :
            return exp_spaceship;

        case tok_star :
            return exp_mul;
    }
}
const char* BinExpTypeToChars(metal_expression_type_t t)
{
    switch(t)
    {
        case exp_dot : return ".";
        case exp_dotdot : return "..";
        case exp_arrow : return "->";

        case exp_add : return "+";
        case exp_sub : return "-";
        case exp_mul : return "*";
        case exp_div : return "/";

        case exp_cat : return "~";
        case exp_assign : return "=";
        case exp_eq : return "==";
        case exp_neq : return "!=";
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

bool IsBinaryOperator(metal_token_enum_t t)
{
    return (t >= tok_comma && t < tok_kw_struct);
}

uint32_t mix(uint32_t a, uint32_t b)
{
    return a ^ b;
}

metal_expression_t* ParseExpression(metal_parser_t* self, metal_expression_t* prev)
{
    metal_expression_t* result = 0;

    metal_token_t* currentToken = MetalParserNextToken(self);
    metal_token_enum_t tokenType =
        (currentToken ? currentToken->TokenType : tok_invalid);

    if (tokenType == tok_lParen)
    {
        result = AllocNewExpression(exp_paren);
        result->E1 = ParseExpression(self, 0);
        MetalParserMatch(self, tok_rParen);
    }
    else if (tokenType == tok_unsignedNumber)
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
    else if (tokenType == tok_identifier)
    {
        result = AllocNewExpression(exp_identifier);
        result->Identifier = currentToken->Identifier;
        result->Length = currentToken->Length;
        result->Hash = currentToken->IdentifierKey;
    }
    else if (tokenType == tok_kw_inject)
    {
        result = AllocNewExpression(exp_eject);
        result->E1 = ParseExpression(self, result);
        result->Hash = mix(
            crc32c(~0, "inject", sizeof("inject") - 1),
            result->E1->Hash
        );
    }
    else
    {
        printf("Unexpected Token: %s\n", MetalTokenEnum_toChars(tokenType));
        assert(0);
    }

//    printf("TokenType: %s\n", MetalTokenEnum_toChars(tokenType));
//  printf("Next TokenType: %s\n", MetalTokenEnum_toChars(tokenType));

    metal_token_t* peekNext = MetalParserPeekToken(self, 1);
    if (peekNext && IsBinaryOperator(peekNext->TokenType))
    {
        MetalParserMatch(self, peekNext->TokenType);
//        printf("It's an operator\n");
        
        metal_expression_type_t exp_type = BinExpTypeFromTokenType(tokenType);

        metal_expression_t* E1 = result;
        metal_expression_t* E2 = ParseExpression(self, 0);

        result = AllocNewExpression(exp_type);
        result->E1 = E1;
        result->E2 = E2;
    }

    return result;
}

metal_expression_t* ParseExpressionFromString(const char* exp)
{
    metal_lexer_t lexer;
    metal_parser_t parser;
    metal_lexer_state_t lexer_state =
        MetalLexerStateFromString(0, exp);

    InitMetalLexer(&lexer);
    LexString(&lexer, exp);

    MetalParserInitFromLexer(&parser, &lexer);

    metal_expression_t* result = ParseExpression(&parser, 0);
    return result;
}

#include <stdio.h>

#define CASE_MACRO(EXP_TYPE, ...) \
    case EXP_TYPE : return #EXP_TYPE;

const char* MetalExpType_toChars(metal_expression_type_t type)
{
    switch(type)
    {
        FOREACH_EXP(CASE_MACRO)
    }
}

const char* PrintExpression(metal_expression_t* exp)
{
    char scratchpad[512];
    uint32_t expStringLength = 0;

    if (exp->Type == exp_paren)
    {
        scratchpad[expStringLength++] = '(';
        const char* e1  = PrintExpression(exp->E1);
        uint32_t e1_length = strlen(e1);
        memcpy(scratchpad + expStringLength, e1, e1_length);
        expStringLength += e1_length;
        free(e1);
        scratchpad[expStringLength++] = ')';
    }
    else if (exp->Type == exp_identifier)
    {
        expStringLength += sprintf(scratchpad + expStringLength, "%.*s ",
            exp->Length, exp->Identifier);
    }
    else if (exp->Type == exp_string)
    {
        expStringLength += sprintf(scratchpad + expStringLength, "\"%.*s\" ",
            exp->Length, exp->String);
    }
    else if (exp->Type == exp_signed_integer)
    {
        expStringLength += sprintf(scratchpad + expStringLength, "%d ",
            (int)exp->ValueI64);
    }
    else if (exp->Type > exp_bin_invalid && exp->Type < exp_bin_max)
    {
        scratchpad[expStringLength++] = '(';
        const char* e1  = PrintExpression(exp->E1);
        uint32_t e1_length = strlen(e1);
        memcpy(scratchpad + expStringLength, e1, e1_length);
        expStringLength += e1_length;
        free(e1);

        const char* op = BinExpTypeToChars(exp->Type);
        uint32_t op_length = strlen(op);
        memcpy(scratchpad + expStringLength, op, op_length);
        expStringLength += op_length;
        *(scratchpad + expStringLength++) = ' ';

        const char* e2  = PrintExpression(exp->E2);
        uint32_t e2_length = strlen(e2);
        memcpy(scratchpad + expStringLength, e2, e2_length);
        expStringLength += e2_length;
        scratchpad[expStringLength++] = ')';
        free(e2);
    }
    else
    {
        printf("don't know how to print %s\n", (MetalExpType_toChars(exp->Type)));
    }

    char* result = (const char*) malloc(expStringLength + 1);
    memcpy(result, scratchpad, expStringLength);
    result[expStringLength] = '\0';

    return result;
}

uint32_t OpToPrecedence(metal_expression_type_t exp)
{
    if (exp == exp_paren)
    {
        return 1;
    }
    else if (exp == exp_identifier || exp == exp_signed_integer)
    {
        return 2;
    }
    else
    {
        
    }
    return 0;
}

void ReorderExpression(metal_expression_t* exp)
{
    uint32_t prec = OpToPrecedence(exp->Type);
}

#ifdef TEST_PARSER
void TestParseExprssion(void)
{
    metal_expression_t* expr = ParseExpression("12 - 16 - 99");
    assert(!strcmp(PrintExpression("(12 - (16 - 99 ))")));
    ReorderExpression(exp);
    assert(!strcmp(PrintExpression("((12 - 16 ) - 99 )")));

}
#endif
