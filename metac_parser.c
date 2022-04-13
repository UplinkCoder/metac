#include "metac_parser.h"
#include <string.h>
#include <stdlib.h>
#include <assert.h>
#include "cache/crc32.c"
const char* MetaCExpressionType_toChars(metac_expression_type_t type);


void MetaCParserInitFromLexer(metac_parser_t* self, metac_lexer_t* lexer)
{
    self->Lexer = lexer;
    self->CurrentTokenIndex = 0;
}

metac_token_t* MetaCParserNextToken(metac_parser_t* self)
{
    metac_token_t* result = 0;
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

metac_token_t* MetaCParserPeekToken(metac_parser_t* self, int32_t p)
{
    metac_token_t* result = 0;
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


void MetaCParserMatch(metac_parser_t* self, metac_token_enum_t type)
{
    metac_token_t* token = MetaCParserNextToken(self);
    metac_token_enum_t got = token->TokenType;
    if (got != type)
    {
        printf("Expected: %s -- Got: %s\n",
            MetaCTokenEnum_toChars(type), MetaCTokenEnum_toChars(got));
    }
}

uint32_t _newExp_size = 0;
uint32_t _newExp_capacity = 0;
metac_expression_t* _newExp_mem = 0;

#ifndef ALIGN4
#  define ALIGN4(N) \
      (((N) + 3) & ~3)
#endif

metac_expression_type_t BinExpTypeFromTokenType(metac_token_enum_t tokenType)
{
    metac_expression_type_t result = exp_invalid;
    if (tokenType >= FIRST_BINARY_TOKEN(TOK_SELF) | tokenType <= LAST_BINARY_TOKEN(TOK_SELF))
    {
        return cast(metac_expression_type_t)(cast(int)tokenType -
                (cast(int)FIRST_BINARY_TOKEN(TOK_SELF) -
                 cast(int)FIRST_BINARY_EXP(TOK_SELF)));
    }
}

const char* BinExpTypeToChars(metac_expression_type_t t)
{
    switch(t)
    {
        case exp_comma     : return ",";
        case exp_dot       : return ".";
        case exp_dotdot    : return "..";
        case exp_arrow     : return "->";

        case exp_add       : return "+";
        case exp_sub       : return "-";
        case exp_mul       : return "*";
        case exp_div       : return "/";
        case exp_xor       : return "^";
        case exp_or        : return "|";
        case exp_and       : return "&";
        case exp_cat       : return "~";
        case exp_lsh       : return "<<";
        case exp_rsh       : return ">>";

        case exp_oror      : return "||";
        case exp_andand    : return "&&";

        case exp_assign    : return "=";

        case exp_add_ass   : return "+=";
        case exp_sub_ass   : return "-=";
        case exp_mul_ass   : return "*=";
        case exp_div_ass   : return "/=";
        case exp_xor_ass   : return "^=";
        case exp_or_ass    : return "|=";
        case exp_and_ass   : return "&=";
        case exp_cat_ass   : return "~=";
        case exp_lsh_ass   : return "<<=";
        case exp_rsh_ass   : return ">>=";

        case exp_eq        : return "==";
        case exp_neq       : return "!=";
        case exp_lt        : return "<";
        case exp_le        : return "<=";
        case exp_gt        : return ">";
        case exp_ge        : return ">=";
        case exp_spaceship : return "<=>";
    }
}

metac_expression_type_t ExpTypeFromTokenType(metac_token_enum_t tokenType)
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
    else if (tokenType == tok_and)
    {
        return exp_addr_or_and;
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

metac_expression_t* AllocNewExpression(metac_expression_type_t t)
{
    metac_expression_t* result = 0;

    if(!_newExp_mem)
    {
        _newExp_mem = cast(metac_expression_t*) malloc(sizeof(metac_expression_t) * 4096);
        _newExp_capacity = 4096;
    }

    if (_newExp_capacity < _newExp_size)
    {
        _newExp_capacity = ALIGN4(cast(uint32_t) (_newExp_capacity * 1.6f));
        _newExp_mem = cast(metac_expression_t*) realloc(_newExp_mem, _newExp_capacity);
    }
    else
    {
        result = _newExp_mem + _newExp_size++;
        result->Type = t;
    }

    return result;
}

static inline void LexString(metac_lexer_t* lexer, const char* line)
{
    uint32_t line_length = strlen(line);
    metac_lexer_state_t lexer_state =
        MetaCLexerStateFromString(0, line);

    while(line_length > 0)
    {
        uint32_t initialPosition = lexer_state.Position;

        metac_token_t token =
            *MetaCLexerLexNextToken(lexer, &lexer_state, line, line_length);

        uint32_t eaten_chars = lexer_state.Position - initialPosition;
        line += eaten_chars;
        line_length -= eaten_chars;
    }
}

bool IsBinaryOperator(metac_token_enum_t t)
{
    return (t >= FIRST_BINARY_TOKEN(TOK_SELF) && t <= LAST_BINARY_TOKEN(TOK_SELF));
}

uint32_t Mix(uint32_t a, uint32_t b)
{
    return a ^ b;
}
bool g_reorder_expression = true;

metac_expression_t* ParseExpression(metac_parser_t* self, metac_expression_t* prev)
{
    metac_expression_t* result = 0;
    if (g_reorder_expression)
    {
        if (self->ExpressionReorderState == 0)
        {
            self->ExpressionReorderState =
                cast(metac_parser_reorder_state_t*) malloc(sizeof(metac_parser_reorder_state_t));
            self->ExpressionReorderState->nOperands = 0;
            self->ExpressionReorderState->nOperators = 0;
            self->ExpressionReorderState->Depth = 0;
        }
    }
    metac_token_t* currentToken = MetaCParserNextToken(self);
    metac_token_enum_t tokenType =
        (currentToken ? currentToken->TokenType : tok_invalid);

#define PushOperator(OP) \
    if (g_reorder_expression) \
    self->ExpressionReorderState->operatorStack[ \
        self->ExpressionReorderState->nOperators++ \
    ] = OP;

#define PushOperand(OP) \
    if (g_reorder_expression) \
    self->ExpressionReorderState->operandStack[ \
        self->ExpressionReorderState->nOperands++ \
    ] = OP;

#define PopOperator(CHECK) \
    { \
    metac_expression_type_t check = \
        self->ExpressionReorderState->operatorStack[ \
            --self->ExpressionReorderState->nOperators \
        ]; \
        assert(check == CHECK); \
    }

    if (tokenType == tok_lParen)
    {
        result = AllocNewExpression(exp_paren);
        PushOperator(exp_paren);
        result->E1 = ParseExpression(self, 0);
        result->Hash = Mix(crc32c(~0, "()", 2), result->E1->Hash);
        PushOperand(result);
        MetaCParserMatch(self, tok_rParen);
        PopOperator(exp_paren);
    }
    else if (tokenType == tok_unsignedNumber)
    {
        result = AllocNewExpression(exp_signed_integer);
        result->ValueI64 = currentToken->ValueU64;
        result->Hash = crc32c(~0, &result->ValueU64, sizeof(result->ValueU64));
        PushOperand(result);
    }
    else if (tokenType == tok_stringLiteral)
    {
        // result = GetOrAddStringLiteral(_string_table, currentToken);

        result = AllocNewExpression(exp_string);
        result->String = currentToken->String;
        result->Length = currentToken->Length;
        result->Hash = currentToken->StringKey;
        PushOperand(result);
    }
    else if (tokenType == tok_identifier)
    {
        result = AllocNewExpression(exp_identifier);
        result->Identifier = currentToken->Identifier;
        result->Length = currentToken->Length;
        result->Hash = currentToken->IdentifierKey;
        PushOperand(result);
    }
    else if (tokenType == tok_kw_eject)
    {
        result = AllocNewExpression(exp_eject);
        PushOperator(exp_eject);
        result->E1 = ParseExpression(self, 0);
        result->Hash = Mix(
            crc32c(~0, "eject", sizeof("eject") - 1),
            result->E1->Hash
        );
        PushOperand(result);
        PopOperator(exp_eject);
    }
    else if (tokenType == tok_kw_inject)
    {
        result = AllocNewExpression(exp_inject);
        PushOperator(exp_inject);
        result->E1 = ParseExpression(self, 0);
        result->Hash = Mix(
            crc32c(~0, "inject", sizeof("inject") - 1),
            result->E1->Hash
        );
        PushOperand(result);
        PopOperator(exp_inject);
    }
    else if (tokenType == tok_kw_typeof)
    {
        result = AllocNewExpression(exp_typeof);
        PushOperator(exp_typeof);
        metac_token_t* nextToken = MetaCParserPeekToken(self, 1);
        if (!nextToken || nextToken->TokenType != tok_lParen)
        {
            ParseError(self->lexer_state, "Expected typeof to be followed by '('");
        }

        metac_expression_t* parenExp = ParseExpression(self, 0);
        PopOperator(exp_typeof);
        assert(parenExp->Type == exp_paren);
        result->E1 = parenExp->E1;
    }
    else if (tokenType == tok_and)
    {
        result = AllocNewExpression(exp_addr);
        PushOperator(exp_addr);
        result->E1 = ParseExpression(self, 0);
        result->Hash = Mix(
            crc32c(~0, "&", sizeof("&") - 1),
            result->E1->Hash
        );
        PushOperand(result);
        PopOperator(exp_addr);
    }
    else if (tokenType == tok_star)
    {
        result = AllocNewExpression(exp_ptr);
        PushOperator(exp_ptr);
        result->E1 = ParseExpression(self, 0);
        result->Hash = Mix(
            crc32c(~0, "*", sizeof("*") - 1),
            result->E1->Hash
        );
        PushOperand(result);
    }
    else
    {
        printf("Unexpected Token: %s\n", MetaCTokenEnum_toChars(tokenType));
        assert(0);
    }

//    printf("TokenType: %s\n", MetaCTokenEnum_toChars(tokenType));

    metac_token_t* peekNext = MetaCParserPeekToken(self, 1);
    if (peekNext)
    {
        if (IsBinaryOperator(peekNext->TokenType))
        {
            metac_token_enum_t op = peekNext->TokenType;
            MetaCParserMatch(self, op);
            printf("Next BinaryOp: %s\n", MetaCTokenEnum_toChars(op));

    //        printf("It's an operator\n");

            metac_expression_type_t exp_type = BinExpTypeFromTokenType(op);
            PushOperator(exp_type);

            metac_expression_t* E1 = result;
            metac_expression_t* E2 = ParseExpression(self, 0);

            result = AllocNewExpression(exp_type);
            result->E1 = E1;
            result->E2 = E2;

            PopOperator(exp_type);
        }
        else if (peekNext->TokenType == tok_lParen)
        {
            PushOperator(exp_call);

            metac_expression_t* E1 = result;
            metac_expression_t* E2 = ParseExpression(self, 0);
            result = AllocNewExpression(exp_call);
            result->E1 = E1;
            result->E2 = E2;

            PopOperator(exp_call);
        }
    }
    return result;
}
#undef PushOperand
#undef PushOperator
#undef PopOperator
#undef PopOperand

metac_expression_t* ParseExpressionFromString(const char* exp)
{
    metac_lexer_t lexer;
    metac_parser_t parser;
    metac_lexer_state_t lexer_state =
        MetaCLexerStateFromString(0, exp);

    InitMetaCLexer(&lexer);
    LexString(&lexer, exp);

    MetaCParserInitFromLexer(&parser, &lexer);

    metac_expression_t* result = ParseExpression(&parser, 0);
    return result;
}

#include <stdio.h>


const char* MetaCExpressionType_toChars(metac_expression_type_t type)
{
    const char* result = 0;

#define CASE_MACRO(EXP_TYPE) \
    case EXP_TYPE : {result = #EXP_TYPE;} break;

    switch(type)
    {
        FOREACH_EXP(CASE_MACRO)
    }

    return result;

#undef CASE_MACRO
}

bool IsBinaryExp(metac_expression_type_t type)
{
    return (type >= FIRST_BINARY_EXP(TOK_SELF) && type <= LAST_BINARY_EXP(TOK_SELF));
}

const char* PrintExpression(metac_expression_t* exp)
{
    char scratchpad[512];
    uint32_t expStringLength = 0;

    if (exp->Type == exp_paren)
    {
        if (!IsBinaryExp(exp->E1->Type))
            scratchpad[expStringLength++] = '(';
        const char* e1  = PrintExpression(exp->E1);
        uint32_t e1_length = strlen(e1);
        memcpy(scratchpad + expStringLength, e1, e1_length);
        expStringLength += e1_length;
        free(e1);
        if (!IsBinaryExp(exp->E1->Type))
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
    else if (IsBinaryExp(exp->Type))
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
    else if (exp->Type == exp_call)
    {
        const char* e1  = PrintExpression(exp->E1);
        uint32_t e1_length = strlen(e1);
        memcpy(scratchpad + expStringLength, e1, e1_length);
        expStringLength += e1_length;
        free(e1);
        *(scratchpad + expStringLength++) = ' ';
        *(scratchpad + expStringLength++) = '(';

        const char* e2  = PrintExpression(exp->E2);
        uint32_t e2_length = strlen(e2);
        memcpy(scratchpad + expStringLength, e2, e2_length);
        expStringLength += e2_length;

        scratchpad[expStringLength++] = ')';
    }
    else if (exp->Type == exp_addr || exp->Type == exp_ptr)
    {
        {
            const char* op = 0;
            if (exp->Type == exp_addr)
                op = "&";
            else if (exp->Type == exp_ptr)
                op = "*";

            assert(op);

            expStringLength += sprintf(scratchpad + expStringLength, "%s ", op);
        }

        if (!IsBinaryExp(exp->E1->Type))
            scratchpad[expStringLength++] = '(';

        const char* e1  = PrintExpression(exp->E1);
        uint32_t e1_length = strlen(e1);
        memcpy(scratchpad + expStringLength, e1, e1_length);
        expStringLength += e1_length;
        free(e1);
        if (!IsBinaryExp(exp->E1->Type))
            scratchpad[expStringLength++] = ')';
    }
    else if (exp->Type == exp_inject || exp->Type == exp_eject || exp->Type == exp_typeof)
    {
        {
            const char* op = 0;
            if (exp->Type == exp_inject)
                op = "inject";
            else if (exp->Type == exp_eject)
                op = "eject";
            else if (exp->Type == exp_typeof)
                op = "typeof";

            assert(op);

            expStringLength += sprintf(scratchpad + expStringLength, "%s ", op);
        }

        if (!IsBinaryExp(exp->E1->Type))
            scratchpad[expStringLength++] = '(';

       const char* e1  = PrintExpression(exp->E1);
        uint32_t e1_length = strlen(e1);
        memcpy(scratchpad + expStringLength, e1, e1_length);
        expStringLength += e1_length;
        free(e1);

        if (!IsBinaryExp(exp->E1->Type))
            scratchpad[expStringLength++] = ')';
    }
    else
    {
        printf("don't know how to print %s\n", (MetaCExpressionType_toChars(exp->Type)));
    }

    char* result = (const char*) malloc(expStringLength + 1);
    memcpy(result, scratchpad, expStringLength);
    result[expStringLength] = '\0';

    return result;
}

uint32_t OpToPrecedence(metac_expression_type_t exp)
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

void ReorderExpression(metac_expression_t* exp)
{
    uint32_t prec = OpToPrecedence(exp->Type);
}

#ifdef TEST_PARSER
void TestParseExprssion(void)
{
    metac_expression_t* expr = ParseExpression("12 - 16 - 99");
    assert(!strcmp(PrintExpression("(12 - (16 - 99 ))")));
    ReorderExpression(exp);
    assert(!strcmp(PrintExpression("((12 - 16 ) - 99 )")));
}
#endif
