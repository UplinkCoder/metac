#ifndef IDENTIFIER_TABLE
#  error "You must compile the parser with IDENTIFIER_TABLE set"
#endif

#include "metac_identifier_table.c"
#include "metac_lexer.c"
#include "metac_parser.h"

#include <string.h>
#include <stdlib.h>
#include <assert.h>

const char* MetaCExpressionKind_toChars(metac_expression_kind_t type);

void MetaCParserInitFromLexer(metac_parser_t* self, metac_lexer_t* lexer)
{
    self->Lexer = lexer;
    self->CurrentTokenIndex = 0;
    InitIdentifierTable(&self->IdentifierTable);
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

uint32_t _newStmt_size = 0;
uint32_t _newStmt_capacity = 0;
metac_statement_t* _newStmt_mem = 0;

#ifndef ALIGN4
#  define ALIGN4(N) \
      (((N) + 3) & ~3)
#endif

metac_expression_kind_t BinExpTypeFromTokenType(metac_token_enum_t tokenType)
{
    metac_expression_kind_t result = exp_invalid;
    if ((tokenType >= FIRST_BINARY_TOKEN(TOK_SELF) | tokenType <= LAST_BINARY_TOKEN(TOK_SELF)))
    {
        return cast(metac_expression_kind_t)(cast(int)tokenType -
                (cast(int)FIRST_BINARY_TOKEN(TOK_SELF) -
                 cast(int)FIRST_BINARY_EXP(TOK_SELF)));
    }
}

const char* BinExpTypeToChars(metac_binary_expression_kind_t t)
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

        case exp_assign    : return "==";

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

    assert(0);
    return 0;
}

metac_expression_kind_t ExpTypeFromTokenType(metac_token_enum_t tokenType)
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

void _newMemRealloc(void** memP, uint32_t* capacity, const uint32_t elementSize)
{
    if (!*memP)
    {
        (*capacity) = 4095 / 1.6f;
    }

    {
        *capacity = ALIGN4(cast(uint32_t) ((*capacity) * 1.6f));
        *memP = realloc(*memP, ((*capacity) * elementSize));
    }
}

metac_expression_t* AllocNewExpression(metac_expression_kind_t kind)
{
    metac_expression_t* result = 0;

    if (_newExp_capacity <= _newExp_size)
    {
        _newMemRealloc(
            (void**)&_newExp_mem,
            &_newExp_capacity,
            sizeof(metac_expression_t)
        );
    }

    {
        result = _newExp_mem + _newExp_size++;
        result->Kind = kind;
    }

    return result;
}

metac_statement_t* AllocNewStatement(metac_statement_kind_t kind)
{
    metac_statement_t* result = 0;

    if (_newStmt_capacity <= _newStmt_size)
    {
        _newMemRealloc(
            (void**)&_newStmt_mem,
            &_newStmt_capacity,
            sizeof(metac_statement_t)
        );
    }

    {
        result = _newStmt_mem + _newStmt_size++;
        result->Kind = kind;
        result->Next = 0;
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

bool MetaCParserPeekMatch(metac_parser_t* self, metac_token_enum_t expectedType, bool optional)
{
    metac_token_t* peekToken =
        MetaCParserPeekToken(self, 1);
    bool result = true;

    if (!peekToken || peekToken->TokenType != expectedType)
    {
        result = false;
        if (!optional)
        {
            ParseErrorF(self->lexer_state, "expected %s but got %s",
                MetaCTokenEnum_toChars(expectedType),
                MetaCTokenEnum_toChars(peekToken ? peekToken->TokenType : tok_eof)
            );
        }
    }
    return result;
}

bool IsBinaryOperator(metac_token_enum_t t)
{
    return (t >= FIRST_BINARY_TOKEN(TOK_SELF) && t <= LAST_BINARY_TOKEN(TOK_SELF));
}

uint32_t Mix(uint32_t a, uint32_t b)
{
  return a ^ b + 0x9e3779b9 + (a << 6) + (a >> 2);

}
bool g_reorder_expression = true;

metac_expression_t* MetaCParserParseExpression(metac_parser_t* self, metac_expression_t* prev)
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
    metac_expression_kind_t check = \
        self->ExpressionReorderState->operatorStack[ \
            --self->ExpressionReorderState->nOperators \
        ]; \
        assert(check == CHECK); \
    }

    if (tokenType == tok_lParen)
    {
        result = AllocNewExpression(exp_paren);
        PushOperator(exp_paren);
        result->E1 = MetaCParserParseExpression(self, 0);
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
        result->Length = LENGTH_FROM_STRING_KEY(currentToken->StringKey);
        result->Hash = currentToken->StringKey;
        PushOperand(result);
    }
    else if (tokenType == tok_identifier)
    {
        result = AllocNewExpression(exp_identifier);
#ifndef IDENTIFIER_TABLE
        result->Identifier = currentToken->Identifier;
#else
        result->IdentifierPtr = currentToken->IdentifierPtr;
#endif
        result->Length = LENGTH_FROM_IDENTIFIER_KEY(currentToken->IdentifierKey);
        result->Hash = currentToken->IdentifierKey;
        PushOperand(result);
    }
    else if (tokenType == tok_kw_eject)
    {
        result = AllocNewExpression(exp_eject);
        PushOperator(exp_eject);
        result->E1 = MetaCParserParseExpression(self, 0);
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
        result->E1 = MetaCParserParseExpression(self, 0);
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

        metac_expression_t* parenExp = MetaCParserParseExpression(self, 0);
        PopOperator(exp_typeof);
        assert(parenExp->Kind == exp_paren);
        result->E1 = parenExp->E1;
    }
    else if (tokenType == tok_and)
    {
        result = AllocNewExpression(exp_addr);
        PushOperator(exp_addr);
        result->E1 = MetaCParserParseExpression(self, 0);
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
        result->E1 = MetaCParserParseExpression(self, 0);
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

            metac_expression_kind_t exp_type = BinExpTypeFromTokenType(op);
            PushOperator(exp_type);

            metac_expression_t* E1 = result;
            metac_expression_t* E2 = MetaCParserParseExpression(self, 0);

            result = AllocNewExpression(exp_type);
            result->E1 = E1;
            result->E2 = E2;

            PopOperator(exp_type);
        }
        else if (peekNext->TokenType == tok_lParen)
        {
            PushOperator(exp_call);
            metac_token_t* peek2 = MetaCParserPeekToken(self, 2);
            metac_expression_t* E1 = result;
            assert(peek2);
            metac_expression_t* E2 = 0;
            if (peek2->TokenType != tok_rParen)
                E2 = MetaCParserParseExpression(self, 0);
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
static metac_statement_t* MetaCParserParseBlockStatement(metac_parser_t* self, metac_statement_t* parent);

static metac_statement_t* MetaCParserParseStatement(metac_parser_t* self, metac_statement_t* parent)
{
    metac_statement_t* result = 0;

    metac_token_t* currentToken = MetaCParserPeekToken(self, 1);
    metac_token_enum_t tokenType =
        (currentToken ? currentToken->TokenType : tok_invalid);

    if (tokenType == tok_invalid)
    {

    }
    else if (tokenType == tok_kw_if)
    {
        result = AllocNewStatement(stmt_if);
        MetaCParserMatch(self, tok_kw_if);
        MetaCParserPeekMatch(self, tok_lParen, 0);
        metac_expression_t* condExpP =
            MetaCParserParseExpression(self, 0);
        assert(condExpP->Kind == exp_paren);
        result->IfCond = condExpP;
        result->IfBody = MetaCParserParseBlockStatement(self, result);
        if (MetaCParserPeekMatch(self, tok_kw_else, 1))
        {
            result->ElseBody = MetaCParserParseBlockStatement(self, result);
        }
    }
    else if (tokenType == tok_kw_switch)
    {
        uint32_t switchHash =
            crc32c(~0, "switch", sizeof("switch") - 1);
        result = AllocNewStatement(stmt_switch);

        MetaCParserMatch(self, tok_kw_switch);
        MetaCParserMatch(self, tok_lParen);
        metac_expression_t* cond =
            MetaCParserParseExpression(self, 0);
        switchHash = Mix(switchHash, cond->Hash);
        MetaCParserMatch(self, tok_rParen);
        metac_statement_t* caseBlock =
            MetaCParserParseBlockStatement(self, result);
        switchHash = Mix(switchHash, caseBlock->Hash);
    }
    else if (tokenType == tok_identifier)
    {
        metac_token_t* peek2 = MetaCParserPeekToken(self, 2);
        if (peek2 && peek2->TokenType == tok_colon)
        {
            MetaCParserMatch(self, tok_identifier);
            MetaCParserMatch(self, tok_colon);

            result = AllocNewStatement(stmt_label);
            result->Label = GetOrAddIdentifier(&self->IdentifierTable,
                                               IDENTIFIER_PTR(&self->Lexer->IdentifierTable, *currentToken),
                                               currentToken->IdentifierKey);
        }
    }
    else if (tokenType == tok_kw_case)
    {
        uint32_t caseHash =
            crc32c(~0, "case", sizeof("case") - 1);
        result = AllocNewStatement(stmt_case);

        MetaCParserMatch(self, tok_kw_case);
        metac_expression_t* caseExp =
            MetaCParserParseExpression(self, 0);
        MetaCParserMatch(self, tok_colon);

        caseHash = Mix(caseHash, caseExp->Hash);
        MetaCParserMatch(self, tok_rParen);
        metac_statement_t* caseBlock =
            MetaCParserParseBlockStatement(self, result);
        caseHash = Mix(caseHash, caseBlock->Hash);
        result->Hash = caseHash;

    }
    else if (tokenType == tok_lBrace)
    {
        result = MetaCParserParseBlockStatement(self, parent);
    }

    return result;
}


static metac_statement_t* MetaCParserParseBlockStatement(metac_parser_t* self, metac_statement_t* parent)
{
    metac_statement_t* result = 0;

    MetaCParserMatch(self, tok_lBrace);

    metac_statement_t* firstStatement = 0;
    metac_statement_t* nextStatement = 0;
    result = AllocNewStatement(stmt_block);

    for (;;)
    {
        metac_token_t* peekToken = MetaCParserPeekToken(self, 1);

        if (peekToken && peekToken->TokenType == tok_rBrace)
        {
            if (!firstStatement)
            {
                // maybe put an empty statement?
            }
            break;
        }

        if (!firstStatement)
        {
            firstStatement = MetaCParserParseStatement(self, result);
            nextStatement = firstStatement;
            result->Hash = Mix(result->Hash, nextStatement->Hash);
        }
        else
        {
            nextStatement->Next = MetaCParserParseStatement(self, result);
            result->Hash = Mix(result->Hash, nextStatement->Hash);
            if (nextStatement->Next)
            {
                nextStatement = nextStatement->Next;
            }
        }
    }

    result->Next = firstStatement;

    MetaCParserMatch(self, tok_rBrace);

    return result;
}


/// static lexer for using in the g_lineParser
static metac_lexer_t g_lineLexer = {
    &g_lineLexer.inlineTokens,
    0,
    (sizeof(g_lineLexer.inlineTokens) / sizeof(g_lineLexer.inlineTokens[0]))
};
/// There can only be one LineParser as it uses static storage
static metac_parser_t g_lineParser = { &g_lineLexer };

#define ARRAY_SIZE(A) \
    ((unsigned int)(sizeof((A)) / sizeof((A)[0])))

metac_expression_t* MetaCParserParseExpressionFromString(const char* exp)
{
    assert(g_lineLexer.tokens_capacity == ARRAY_SIZE(g_lineLexer.inlineTokens));
    g_lineParser.CurrentTokenIndex = 0;
    g_lineLexer.tokens_size = 0;
    InitIdentifierTable(&g_lineLexer.IdentifierTable);
    if (!g_lineParser.IdentifierTable.Slots)
    {
        InitIdentifierTable(&g_lineParser.IdentifierTable);
    }

    LexString(&g_lineLexer, exp);

    metac_expression_t* result = MetaCParserParseExpression(&g_lineParser, 0);
    return result;
}

metac_statement_t* MetaCParserParseStatementFromString(const char* exp)
{
    assert(g_lineLexer.tokens_capacity == ARRAY_SIZE(g_lineLexer.inlineTokens));
    g_lineParser.CurrentTokenIndex = 0;
    g_lineLexer.tokens_size = 0;
    InitIdentifierTable(&g_lineLexer.IdentifierTable);
    if (!g_lineParser.IdentifierTable.Slots)
    {
        InitIdentifierTable(&g_lineParser.IdentifierTable);
    }

    LexString(&g_lineLexer, exp);

    metac_statement_t* result = MetaCParserParseStatement(&g_lineParser, 0);

    return result;
}


#include <stdio.h>


const char* MetaCExpressionKind_toChars(metac_expression_kind_t type)
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

bool IsBinaryExp(metac_expression_kind_t type)
{
    return (type >= FIRST_BINARY_EXP(TOK_SELF) && type <= LAST_BINARY_EXP(TOK_SELF));
}

const char* PrintExpression(metac_parser_t* self, metac_expression_t* exp)
{
    char scratchpad[512];
    uint32_t expStringLength = 0;

    if (exp->Kind == exp_paren)
    {
        if (!IsBinaryExp(exp->E1->Kind))
            scratchpad[expStringLength++] = '(';
        const char* e1  = PrintExpression(self, exp->E1);
        uint32_t e1_length = strlen(e1);
        memcpy(scratchpad + expStringLength, e1, e1_length);
        free((void*)e1);
        expStringLength += e1_length;
        if (!IsBinaryExp(exp->E1->Kind))
            scratchpad[expStringLength++] = ')';
    }
    else if (exp->Kind == exp_identifier)
    {
        expStringLength += sprintf(scratchpad + expStringLength, "%.*s ",
            LENGTH_FROM_IDENTIFIER_KEY(exp->IdentifierKey),
            IdentifierPtrToCharPtr(
                &self->Lexer->IdentifierTable,
                exp->IdentifierPtr
            )
        );
    }
    else if (exp->Kind == exp_string)
    {
        expStringLength += sprintf(scratchpad + expStringLength, "\"%.*s\" ",
            LENGTH_FROM_STRING_KEY(exp->StringKey), exp->String);
    }
    else if (exp->Kind == exp_signed_integer)
    {
        expStringLength += sprintf(scratchpad + expStringLength, "%d ",
            (int)exp->ValueI64);
    }
    else if (IsBinaryExp(exp->Kind))
    {
        scratchpad[expStringLength++] = '(';
        const char* e1  = PrintExpression(self, exp->E1);
        uint32_t e1_length = strlen(e1);
        memcpy(scratchpad + expStringLength, e1, e1_length);
        free((void*)e1);
        expStringLength += e1_length;

        const char* op = BinExpTypeToChars(exp->Kind);
        uint32_t op_length = strlen(op);
        memcpy(scratchpad + expStringLength, op, op_length);
        expStringLength += op_length;
        *(scratchpad + expStringLength++) = ' ';

        const char* e2  = PrintExpression(self, exp->E2);
        uint32_t e2_length = strlen(e2);
        memcpy(scratchpad + expStringLength, e2, e2_length);
        free((void*)e2);
        expStringLength += e2_length;
        scratchpad[expStringLength++] = ')';
    }
    else if (exp->Kind == exp_call)
    {
        const char* e1  = PrintExpression(self, exp->E1);
        uint32_t e1_length = strlen(e1);
        memcpy(scratchpad + expStringLength, e1, e1_length);
        free((void*)e1);
        expStringLength += e1_length;
        *(scratchpad + expStringLength++) = ' ';
        *(scratchpad + expStringLength++) = '(';

        if (exp->E2)
        {
            const char* e2  = PrintExpression(self, exp->E2);
            uint32_t e2_length = strlen(e2);
            memcpy(scratchpad + expStringLength, e2, e2_length);
            free((void*)e2);
            expStringLength += e2_length;
        }
        scratchpad[expStringLength++] = ')';
    }
    else if (exp->Kind == exp_addr || exp->Kind == exp_ptr)
    {
        {
            const char* op = 0;
            if (exp->Kind == exp_addr)
                op = "&";
            else if (exp->Kind == exp_ptr)
                op = "*";

            assert(op);

            expStringLength += sprintf(scratchpad + expStringLength, "%s ", op);
        }

        if (!IsBinaryExp(exp->E1->Kind))
            scratchpad[expStringLength++] = '(';

        const char* e1  = PrintExpression(self, exp->E1);
        uint32_t e1_length = strlen(e1);
        memcpy(scratchpad + expStringLength, e1, e1_length);
        free((void*)e1);
        expStringLength += e1_length;
        if (!IsBinaryExp(exp->E1->Kind))
            scratchpad[expStringLength++] = ')';
    }
    else if (exp->Kind == exp_inject || exp->Kind == exp_eject || exp->Kind == exp_typeof)
    {
        {
            const char* op = 0;
            if (exp->Kind == exp_inject)
                op = "inject";
            else if (exp->Kind == exp_eject)
                op = "eject";
            else if (exp->Kind == exp_typeof)
                op = "typeof";

            assert(op);

            expStringLength += sprintf(scratchpad + expStringLength, "%s ", op);
        }

        if (!IsBinaryExp(exp->E1->Kind))
            scratchpad[expStringLength++] = '(';

        const char* e1  = PrintExpression(self, exp->E1);
        uint32_t e1_length = strlen(e1);
        memcpy(scratchpad + expStringLength, e1, e1_length);
        free((void*)e1);
        expStringLength += e1_length;

        if (!IsBinaryExp(exp->E1->Kind))
            scratchpad[expStringLength++] = ')';
    }
    else
    {
        printf("don't know how to print %s\n", (MetaCExpressionKind_toChars(exp->Kind)));
    }

    char* result = (char*) malloc(expStringLength + 1);
    memcpy(result, scratchpad, expStringLength);
    result[expStringLength] = '\0';

    return result;
}

uint32_t OpToPrecedence(metac_expression_kind_t exp)
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
    uint32_t prec = OpToPrecedence(exp->Kind);
}

#ifdef TEST_PARSER
void TestParseExprssion(void)
{
    metac_expression_t* expr = MetaCParserParseExpression("12 - 16 - 99");
    assert(!strcmp(PrintExpression(self, "(12 - (16 - 99 ))")));
    ReorderExpression(exp);
    assert(!strcmp(PrintExpression(self, "((12 - 16 ) - 99 )")));
}
#endif
