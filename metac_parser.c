#ifndef _METAC_PARSER_C_
#define _METAC_PARSER_C_

#define TYPE_EXP
#include "metac_identifier_table.c"

#include "metac_lexer.c"
#include "metac_parser.h"
#include "metac_lpp.h"
#include "metac_alloc_node.h"

#include <string.h>
#include <stdlib.h>
#include <assert.h>

#include "3rd_party/tracy/TracyC.h"

struct metac_declaration_t;
const void* _emptyPointer = (const void*)0x1;

#ifndef emptyPointer
#  define emptyPointer ((void*)_emptyPointer)
#endif

// void _newMemRealloc(void** memP, uint32_t* capacity, const uint32_t elementSize);
const char* MetaCExpressionKind_toChars(metac_expression_kind_t type);

bool IsExpressionNode(metac_node_kind_t Kind)
{
    return ((Kind > node_exp_invalid) & (Kind < node_exp_max));
}

#define compiler_key 0x8481e0
#define context_key 0x7a2a7f
#define target_key 0x63a0c4
#define type_key 0x40869f
#define defined_key 0x7d9260

static inline void InitSpecialIdentifier(metac_parser_t* self)
{
    self->SpecialNamePtr_Compiler =
        GetOrAddIdentifier(&self->IdentifierTable, compiler_key, "compiler");

    self->SpecialNamePtr_Context =
        GetOrAddIdentifier(&self->IdentifierTable, context_key, "context");

    self->SpecialNamePtr_Target =
        GetOrAddIdentifier(&self->IdentifierTable, target_key, "target");

    self->SpecialNamePtr_Type =
        GetOrAddIdentifier(&self->IdentifierTable, type_key, "type");

    self->SpecialNamePtr_Defined =
        GetOrAddIdentifier(&self->IdentifierTable, defined_key, "defined");
}


void MetaCParser_Init(metac_parser_t* self)
{
    self->CurrentTokenIndex = 0;
    IdentifierTable_Init(&self->IdentifierTable, IDENTIFIER_LENGTH_SHIFT, 13);
    IdentifierTable_Init(&self->StringTable, STRING_LENGTH_SHIFT, 13);

    self->PackStackCapacity = 8;
    self->PackStack = (uint16_t*)
        calloc(sizeof(*self->PackStack), self->PackStackCapacity);
    self->PackStackTop = -1;
/*
    self->Defines = self->inlineDefines;
    self->DefineCount = 0;
    self->DefineCapacity = ARRAY_SIZE(self->inlineDefines);
*/
    self->LexerState = 0;
#ifndef NO_PREPROCESSOR
    self->Preprocessor = 0;
#endif
    self->BlockStatementStackCapacity = 16;
    self->BlockStatementStackCount = 0;
    self->BlockStatementStack = (stmt_block_t**)
        malloc(sizeof(stmt_block_t*) * self->BlockStatementStackCapacity);

    self->OpenParens = 0;

    MetaCLocationStorage_Init(&self->LocationStorage);

    InitSpecialIdentifier(self);
    MetaCPrinter_Init(&self->DebugPrinter,
                      &self->IdentifierTable, &self->StringTable);
}

void MetaCParser_Free(metac_parser_t* self)
{
    IdentifierTable_Free(&self->IdentifierTable);
    IdentifierTable_Free(&self->StringTable);
    free(self->BlockStatementStack);
    static const metac_parser_t zeroParser = {0};
    *self = zeroParser;
    self = 0;
}

void MetaCParser_InitFromLexer(metac_parser_t* self, metac_lexer_t* lexer)
{
    self->Lexer = lexer;
    MetaCParser_Init(self);
}
//TODO Implement IsMacro
//    and Handle Macro
#define HandleMacro(...)
#define HandlePreprocessor(...)
#define IsMacro(...) false

static inline metac_location_t LocationFromToken(metac_parser_t* self,
                                                 metac_token_t* tok)
{
    return self->Lexer->LocationStorage.Locations[tok->LocationId - 4];
}

metac_identifier_ptr_t RegisterIdentifier(metac_parser_t* self,
                                          metac_token_t* token)
{
    const char* identifierString =
        IdentifierPtrToCharPtr(
            MEMBER_SUFFIX(&self->Lexer->Identifier),
            token->IdentifierPtr
        );

    uint32_t identifierKey = token->IdentifierKey;
    return GetOrAddIdentifier(MEMBER_SUFFIX(&self->Identifier),
                              identifierKey, identifierString);
}

metac_identifier_ptr_t RegisterString(metac_parser_t* self,
                                      metac_token_t* token)
{
    const char* string =
        IdentifierPtrToCharPtr(
            MEMBER_SUFFIX(&self->Lexer->String),
            token->StringPtr
        );
        uint32_t stringKey = token->StringKey;
        return GetOrAddIdentifier(MEMBER_SUFFIX(&self->String),
                                  stringKey, string);
}
/*
void AddDefine(metac_parser_t* self, metac_token_t* token, uint32_t nParameters)
{
    metac_define_t define;

    assert(token->TokenType == tok_identifier);

    define.NumberOfParameters = nParameters;
    define.IdentifierPtr = RegisterIdentifier(self, token);
    define.TokenPosition = token->Position;
    define.SourceId = self->LexerState->SourceId;
    define.IdentifierKey = token->IdentifierKey;

    assert(self->DefineCount < self->DefineCapacity);
    self->Defines[self->DefineCount++] = define;

    if (self->DefineCapacity >= self->DefineCount)
    {
        bool wasInline = (self->Defines == self->inlineDefines);
        if (wasInline)
        {
            self->Defines = (metac_define_t*)malloc(32 * sizeof(metac_define_t));
            self->DefineCapacity = 32;
            memcpy(self->Defines, self->inlineDefines,
                sizeof(metac_define_t) * ARRAY_SIZE(self->inlineDefines));
            return ;
        }
        _newMemRealloc((void**)&self->Defines, &self->DefineCapacity, sizeof(metac_define_t));

    }
}
*/
static inline bool MetaCParser_PeekMatch(metac_parser_t* self, metac_token_enum_t expectedType, bool optional)
{
    metac_token_t* peekToken =
        MetaCParser_PeekToken(self, 1);
    bool result = true;

    metac_location_t loc = {0};
    if (self->LexerState)
    {
        loc.StartLine = self->LexerState->Line;
        loc.StartColumn = self->LexerState->Column;
    }

    if (!peekToken || peekToken->TokenType != expectedType)
    {
        result = false;
        if (!optional)
        {
            ParseErrorF(loc, "expected %s but got %s",
                MetaCTokenEnum_toChars(expectedType),
                MetaCTokenEnum_toChars(peekToken ? peekToken->TokenType : tok_eof)
            );
        }
    }

    return result;
}

void MetaCParser_Advance(metac_parser_t* self)
{
#if !defined(NO_PREPROCESSOR)
    metac_preprocessor_t* preProc = self->Preprocessor;
    if (preProc && preProc->DefineTokenStackCount)
    {
        metac_token_t_array* tokens = &preProc->DefineTokenStack[preProc->DefineTokenStackCount - 1];
        uint32_t* idx = &preProc->DefineTokenIndexStack[preProc->DefineTokenStackCount - 1];
        (*idx)++;

        if (tokens->Count <= (*idx))
        {
            preProc->DefineTokenStackCount--;
        }
    }
    else
#endif
    self->CurrentTokenIndex++;
}

static inline void LoadTokens(metac_parser_t* self,
                              uint32_t* tokenCount,
                              metac_token_t** tokens,
                              uint32_t** tokenOffset)
{
#if defined(NO_PREPROCESSOR)
    (*tokens) = self->Lexer->Tokens;
    (*tokenCount) = self->Lexer->TokenCount;
    (*tokenOffset) = &self->CurrentTokenIndex;
#else
    metac_preprocessor_t* preProc = self->Preprocessor;
    (*tokenCount) = ((preProc && preProc->DefineTokenStackCount > 0) ?
        preProc->DefineTokenStack[preProc->DefineTokenStackCount - 1].Count :
        self->Lexer->TokenCount);

    (*tokens) = ((preProc ? preProc->DefineTokenStackCount > 0 : 0) ?
        preProc->DefineTokenStack[preProc->DefineTokenStackCount - 1].Ptr :
        self->Lexer->Tokens);

    (*tokenOffset) = ((preProc ? preProc->DefineTokenStackCount > 0 : 0) ?
        &preProc->DefineTokenIndexStack[preProc->DefineTokenStackCount - 1] :
        &self->CurrentTokenIndex);
#endif // !NO_PREPROCESSOR
}


#define NextToken() \
    (((*tokenOffset) < tokenCount) ? \
    (tokens + ((*tokenOffset)++)) : 0)

#define PeekMatch(RESULT, TOKEN_TYPE) \
    ( ((RESULT = NextToken()), (RESULT && RESULT->TokenType == TOKEN_TYPE)) ? \
    ( RESULT = NextToken() ) : ( (RESULT = tokens + (--(*tokenOffset)) - 1), (metac_token_t*)0) )


#if !defined(NO_PREPROCESSOR)
metac_token_t* MetaCPreProcessor_NextDefineToken(metac_preprocessor_t* self)
{
    printf("F: %s\n", __FUNCTION__);
    assert(self->DefineTokenStackCount);

    metac_token_t* result = 0;
    metac_token_t_array tokens;
Lbegin:
     tokens =
        self->DefineTokenStack[self->DefineTokenStackCount - 1];

    uint32_t defineTokenIndex =
        self->DefineTokenIndexStack[self->DefineTokenStackCount - 1]++;
    printf("defineTokenIndex: %u\n", defineTokenIndex);
    if (defineTokenIndex < tokens.Count)
    {
        result = &tokens.Ptr[defineTokenIndex];
    }
    else
    {
        if (--self->DefineTokenStackCount != 0)
        {
            self->DefineTokenIndexStack[self->DefineTokenStackCount] = 0;
            goto Lbegin;
        }
    }

    return result;
}

metac_token_t* MetaCPreProcessor_PeekDefineToken(metac_preprocessor_t* self,
                                                 uint32_t offset)
{
    assert(self->DefineTokenStackCount);

    metac_token_t* result = 0;
    metac_token_t_array tokens;
Lbegin:
    tokens =
        self->DefineTokenStack[self->DefineTokenStackCount - 1];

    uint32_t defineTokenIndex =
        self->DefineTokenIndexStack[self->DefineTokenStackCount - 1];
/*
    printf("defineTokenIndex + offset < tokens.Count -- %u < %u\n",
            defineTokenIndex + offset, tokens.Count);
*/
    if (defineTokenIndex + offset < tokens.Count)
    {
        result = &tokens.Ptr[defineTokenIndex + offset];
        //printf("Got %s from define\n", MetaCTokenEnum_toChars(result->TokenType);
    }
    else
    {
        if (--self->DefineTokenStackCount != 0)
        {
            self->DefineTokenIndexStack[self->DefineTokenStackCount] = 0;
            goto Lbegin;
        }
    }

    return result;
}

metac_token_t* MetaCParser_HandleIdentifier(metac_parser_t* self,
                                            metac_token_t* tok,
                                            metac_preprocessor_t* preProc)
{
    uint32_t tokenCount;
    metac_token_t* tokens;
    uint32_t* tokenOffset;

    LoadTokens(self, &tokenCount, &tokens, &tokenOffset);

    metac_token_t* result = tok;

    const uint32_t idKey = result->IdentifierKey;

    int32_t defineSlotIdx;

    LcontinuePreprocSearch:
    do {
        defineSlotIdx = (
            preProc ?
            MetaCIdentifierTable_HasKey(&preProc->DefineIdentifierTable, idKey) :
            -1
        );
        if (defineSlotIdx != -1)
            break;
        preProc = preProc ? preProc->Parent : 0;
    } while(preProc);

    if (defineSlotIdx != -1)
    {
        const char* idChars = IdentifierPtrToCharPtr(&self->Lexer->IdentifierTable,
                                                     result->IdentifierPtr);
        metac_preprocessor_define_ptr_t matchingDefine = {0};

        matchingDefine = MetaCPreProcessor_GetDefine(preProc, idKey, idChars);

        if (!matchingDefine.v)
        {
            preProc = preProc->Parent;
            goto LcontinuePreprocSearch;
        }

        metac_preprocessor_define_t define =
            self->Preprocessor->DefineTable.DefineMemory[matchingDefine.v - 4];
        NextToken();

        if (define.ParameterCount > 0 || define.IsVariadic)
        {
            metac_token_t* tok = NextToken();
            result = NextToken();
            if (tok->TokenType != tok_lParen)
            {
                printf("Expected '(' but got %s\n", MetaCTokenEnum_toChars(tok->TokenType));
            }

            metac_token_t paramTokens[64];
            uint32_t paramTokenIndex = 0;
            uint32_t paramTokenCount = 0;
            uint32_t paramTokenCapacity = ARRAY_SIZE(paramTokens);

            DEF_STACK_ARRAY(metac_token_t_array, paramArrays, 32);

            uint32_t ParenDepth = 1;

            for(;;)
            {
                if (result->TokenType == tok_lParen)
                    ParenDepth += 1;
                if (result->TokenType == tok_rParen)
                    ParenDepth -= 1;

                if (ParenDepth == 0 ||
                    (ParenDepth == 1 && result->TokenType == tok_comma))
                {
                    metac_token_t_array param = {
                        paramTokens + paramTokenIndex,
                        paramTokenCount - paramTokenIndex,
                        paramTokenCount - paramTokenIndex
                    };

                    ADD_STACK_ARRAY(paramArrays, param);
                    printf("Added paramTuple of %u tokens\n", paramTokenCount - paramTokenIndex);
                    paramTokenIndex = paramTokenCount;
                    if (ParenDepth == 0)
                    {
                        break;
                    }
                    else
                    {
                        result = NextToken();
                        continue;
                    }
                }

                if (paramTokenCount < paramTokenCapacity)
                {
                    printf("[%u] Adding another token\n", paramTokenCount);
                    paramTokens[paramTokenCount++] = *result;
                    result = NextToken();
                }
                else
                {
                    // TODO Realloc
                    assert(0);
                }
            }

            assert(result->TokenType == tok_rParen);
            // the fllowing NextToken skips over the rParen
            NextToken();

            printf("define.TokenCount: %u\n", define.TokenCount);
            MetaCPreProcessor_PushDefine(preProc, &define, paramArrays);
            result =
                &preProc->DefineTokenStack[preProc->DefineTokenStackCount - 1].Ptr[
                    preProc->DefineTokenIndexStack[preProc->DefineTokenStackCount - 1]];
        }
        else
        {
			metac_token_t_array_array emptyParams = {0};
			MetaCPreProcessor_PushDefine(preProc, &define,  emptyParams);
            assert(preProc->DefineTokenStackCount);
            uint32_t stackIdx = preProc->DefineTokenStackCount - 1;
            result = &preProc->DefineTokenStack[stackIdx].Ptr[
                preProc->DefineTokenIndexStack[stackIdx - 1]];
            printf("result: %s\n", MetaCTokenEnum_toChars(result->TokenType));
        }
    }

    return result;

}
#endif //NO_PREPROCESSOR

metac_token_t* MetaCParser_NextToken(metac_parser_t* self)
{
    uint32_t tokenCount;
    metac_token_t* tokens;
    uint32_t* tokenOffset;

    metac_token_t* result;

    LoadTokens(self, &tokenCount, &tokens, &tokenOffset);
#if defined(NO_PREPROCESSOR)
    result = NextToken();
#else
    metac_preprocessor_t* preProc = self->Preprocessor;
    if (preProc && preProc->DefineTokenStackCount)
    {
    LnextDefineToken:
        result = MetaCPreProcessor_NextDefineToken(preProc);
        if (!result)
            goto LnextToken;
    }
    else
    {
        assert(self->Lexer->TokenCount);
    LnextToken:
        result = NextToken();
    }

    if(result)
    {
        if (result->TokenType == tok_identifier && preProc)
        {
            printf(IdentifierPtrToCharPtr(&self->Lexer->IdentifierTable, result->IdentifierPtr));
            uint32_t oldDefineStackTop = preProc->DefineTokenStackCount;
            result = MetaCParser_HandleIdentifier(self, result, preProc);
            bool wasDefine =
                (oldDefineStackTop < preProc->DefineTokenStackCount);
            if (wasDefine)
            {
                printf("Exchanged identifier for: %s\n", MetaCTokenEnum_toChars(result->TokenType));
                if (result->TokenType == tok_identifier)
                printf("    id: %s\n", IdentifierPtrToCharPtr(&self->Lexer->IdentifierTable, result->IdentifierPtr));
                MetaCParser_Advance(self);
            }
        }
    }
    else
    {
        // TODO Error
    }

#endif //NO_PREPROCESSOR
    if (result)
    {
        self->LastLocation = self->Lexer->LocationStorage.Locations[result->LocationId - 4];
    }
    return result;
}

#undef PeekMatch
#undef NextToken

uint32_t MetaCParser_HowMuchLookahead(metac_parser_t* self)
{
    return (self->Lexer->TokenCount - self->CurrentTokenIndex);
}

metac_token_t* MetaCParser_PeekToken_(metac_parser_t* self, int32_t p, uint32_t line)
{
    metac_token_t* result = 0;
    assert(self->Lexer->TokenCount);
#if !defined(NO_PREPROCESSOR)

    metac_preprocessor_t* preProc = self->Preprocessor;

    uint32_t CurrentTokenIndex =
        (preProc && preProc->DefineTokenStackCount)
            ? preProc->DefineTokenIndexStack[preProc->DefineTokenStackCount - 1]:
            self->CurrentTokenIndex;
/*
    printf("PeekToken(%u) from %u -- CurrentTokenIndex %u -- DefineStackCount %d\n",
                       p, line, CurrentTokenIndex, preProc ? preProc->DefineTokenStackCount : -1);
    if (line == 675 && self->CurrentTokenIndex == 2)
    {
        // asm ("int $3;");
    }
*/
LpeekDefine:
    if (preProc && preProc->DefineTokenStackCount)
    {
        result = MetaCPreProcessor_PeekDefineToken(preProc, p - 1);
        if (!result)
            goto Lpeek;
    } else
#endif
    if (cast(uint32_t)(self->CurrentTokenIndex + (p - 1)) < self->Lexer->TokenCount)
    {
Lpeek:
        result = self->Lexer->Tokens + self->CurrentTokenIndex + (p - 1);
        self->LastLocation =
            self->Lexer->LocationStorage.Locations[result->LocationId - 4];

#if !defined(NO_PREPROCESSOR)
        if(result)
        {
            if (result->TokenType == tok_identifier)
            {
//                uint32_t oldDefineStackTop = preProc->DefineTokenStackCount;
                result = MetaCParser_HandleIdentifier(self, result, preProc);
//                bool wasDefine =
//                    (oldDefineStackTop < preProc->DefineTokenStackCount);
            }
        }
#endif
    }

    return result;
}

metac_token_t* MetaCParser_Match_(metac_parser_t* self, metac_token_enum_t type,
                                 const char* filename, uint32_t lineNumber)
{
    metac_token_t* token = MetaCParser_PeekToken(self, 1);
    metac_token_enum_t got = (token ? token->TokenType : tok_eof);
    if (got != type)
    {
        if (got != tok_eof)
        {
            metac_location_t loc = self->Lexer->LocationStorage.Locations[token->LocationId - 4];

            printf("[%s:%u] Expected: %s -- Got: %s {line: %u: col: %u}\n",
                filename, lineNumber,
                MetaCTokenEnum_toChars(type), MetaCTokenEnum_toChars(got),
                loc.StartLine, loc.StartColumn);
        }
        else
        {
            printf("[%s:%u] Expected: %s -- Got: End of file\n",
                filename, lineNumber,
                MetaCTokenEnum_toChars(type));
        }
    }
    else
    {
        MetaCParser_Advance(self);

    }
    return token;
}

#ifndef ALIGN4
#  define ALIGN4(N) \
      (((N) + 3) & ~3)
#endif
const char* BinExpTypeToChars(metac_binary_expression_kind_t t)
{
    switch(t)
    {
        case bin_exp_invalid: assert(0);

        case exp_comma     : return ",";
        case exp_dot       : return ".";
        case exp_dotdot    : return "..";
        case exp_arrow     : return "->";

        case exp_add       : return "+";
        case exp_sub       : return "-";
        case exp_mul       : return "*";
        case exp_div       : return "/";
        case exp_rem       : return "%";
        case exp_xor       : return "^";
        case exp_or        : return "|";
        case exp_and       : return "&";
        case exp_lsh       : return "<<";
        case exp_rsh       : return ">>";

        case exp_oror      : return "||";
        case exp_andand    : return "&&";

        case exp_assign    : return "=";

        case exp_add_ass   : return "+=";
        case exp_sub_ass   : return "-=";
        case exp_mul_ass   : return "*=";
        case exp_div_ass   : return "/=";
        case exp_rem_ass   : return "%=";
        case exp_xor_ass   : return "^=";
        case exp_or_ass    : return "|=";
        case exp_and_ass   : return "&=";
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

metac_expression_kind_t BinExpTypeFromTokenType(metac_token_enum_t tokenType)
{
    metac_expression_kind_t result = exp_invalid;
    if (((tokenType >= FIRST_BINARY_TOKEN(TOK_SELF)) & (tokenType <= LAST_BINARY_TOKEN(TOK_SELF))))
    {
        result = cast(metac_expression_kind_t)(cast(int)tokenType -
                (cast(int)FIRST_BINARY_TOKEN(TOK_SELF) -
                 cast(int)FIRST_BINARY_EXP(TOK_SELF)));
    }

    if (tokenType == tok_lParen)
        result = exp_call;

    if (tokenType == tok_lBracket)
        result = exp_index;

    if (tokenType == tok_bang)
        result =  exp_template_instance;

    return result;
}

metac_expression_kind_t ExpTypeFromTokenType(metac_token_enum_t tokenType)
{
    if (tokenType == tok_uint)
    {
        return exp_signed_integer;
    }
    else if (tokenType == tok_string)
    {
        return exp_string;
    }
    else if (tokenType == tok_lParen)
    {
        return exp_paren;
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
    else if (tokenType == tok_identifier)
    {
        return exp_identifier;
    }
    else
    {
        assert(0);
        return exp_invalid;
    }

}


static inline bool IsPostfixOperator(metac_token_enum_t t)
{
    return (t == tok_plusplus || t == tok_minusminus);
}

static inline bool IsBinaryOperator(metac_token_enum_t t, parse_expression_flags_t flags)
{
    if ((flags & (expr_flags_call | expr_flags_enum)) != 0
     && (t == tok_comma))
        return false;

    return ((t >= FIRST_BINARY_TOKEN(TOK_SELF) && t <= LAST_BINARY_TOKEN(TOK_SELF))
            || t == tok_lParen || t == tok_lBracket || t == tok_bang);
}

typedef enum precedence_level_t
{
    prec_none,
    prec_comma,
    prec_op_assign,
    prec_oror,
    prec_andand,
    prec_or,
    prec_xor,
    prec_and,
    prec_eq,
    prec_cmp,
    prec_shift,
    prec_add,
    prec_mul,
    prec_max
} precedence_level_t;

static inline uint32_t OpToPrecedence(metac_expression_kind_t exp)
{
    if (exp == exp_comma)
    {
        return 1;
    }
    else if (exp >= exp_assign && exp <= exp_rsh_ass)
    {
        return 2;
    }
    else if (exp == exp_ternary)
    {
        return 3;
    }
    else if (exp == exp_oror)
    {
        return 4;
    }
    else if (exp == exp_andand)
    {
        return 5;
    }
    else if (exp == exp_or)
    {
        return 7;
    }
    else if (exp == exp_xor)
    {
        return 8;
    }
    else if (exp == exp_and)
    {
        return 9;
    }
    else if (exp == exp_eq || exp == exp_neq)
    {
        return 10;
    }
    else if (exp >= exp_lt && exp <= exp_ge)
    {
        return 11;
    }
    else if (exp == exp_rsh || exp == exp_lsh)
    {
        return 12;
    }
    else if (exp == exp_add || exp == exp_sub)
    {
        return 13;
    }
    else if (exp == exp_div || exp == exp_mul || exp == exp_rem)
    {
        return 14;
    }
    else if (exp == exp_ptr || exp == exp_arrow || exp == exp_dot || exp == exp_addr
          || exp == exp_increment || exp == exp_decrement)
    {
        return 15;
    }
    else if (exp == exp_call || exp == exp_index
          || exp == exp_compl || exp == exp_post_increment
          || exp == exp_post_decrement || exp == exp_template_instance)
    {
        return 16;
    }
    else if (exp == exp_umin   || exp == exp_unary_dot
          || exp == exp_sizeof || exp == exp_not)
    {
        return 17;
    }
    else if (exp == exp_paren
          || exp == exp_signed_integer
          || exp == exp_string
          || exp == exp_identifier
          || exp == exp_char
          || exp == exp_tuple
          || exp == exp_type)
    {
        return 18;
    }
    assert(0);
    return 0;
}

static inline bool IsTypeToken(metac_token_enum_t tokenType)
{
    bool  result =
           (   tokenType == tok_kw_const
            || (tokenType >= tok_kw_auto && tokenType <= tok_kw_double)
            || tokenType == tok_kw_unsigned
            || tokenType == tok_kw_signed
            || tokenType == tok_star
            || tokenType == tok_kw_struct
            || tokenType == tok_kw_enum
            || tokenType == tok_kw_union
            || tokenType == tok_identifier );

    return result;
}

static inline bool IsDeclarationToken(metac_token_enum_t tokenType)
{

   bool result = (   tokenType == tok_kw_static
                  || tokenType == tok_kw_inline
                  || tokenType == tok_kw_extern
                  || tokenType == tok_comment_multi
                  || tokenType == tok_comment_single
                  || IsTypeToken(tokenType)  );

    return result;
}

static inline bool IsDeclarationFirstToken(metac_token_enum_t tokenType)
{
    if (tokenType == tok_star)
        return false;

    return IsDeclarationToken(tokenType);
}


static inline bool IsPrimaryExpressionToken(metac_token_enum_t tokenType)
{
#ifdef TYPE_EXP
    if (IsTypeToken(tokenType) && tokenType != tok_star)
        return true;
#endif
    switch(tokenType)
    {
    case tok_lParen:
    case tok_uint:
    case tok_string:
    case tok_char:
    case tok_identifier:
    case tok_lBrace:
        return true;
    default:
        return false;
    }
}

static inline bool IsPunctuationToken(metac_token_enum_t tok)
{
    if (tok == tok_star)
        return false;
    else
    return (
        (IsBinaryOperator(tok, expr_flags_none)
            && tok != tok_star && tok != tok_lParen
            && tok != tok_lBracket)
        ||  tok == tok_dotdot
        ||  tok == tok_comma
        ||  tok == tok_semicolon
        ||  tok == tok_cat
        ||  tok == tok_rParen);
}

static bool CouldBeCast(metac_parser_t* self, metac_token_enum_t tok)
{
    bool result = true;

    if (tok != tok_lParen)
        return false;

    // first we see if the next could be a type token
    // because if it isn't then we are certainly not as cast
    metac_token_t* peek;
    bool seenStar = false;
    int rParenPos = 0;
    for(int peekCount = 2;
        (peek = MetaCParser_PeekToken(self, peekCount)), peek;
        peekCount++)
    {
        if (peek->TokenType == tok_lParen)
            return false;

        if (peek->TokenType == tok_rParen)
        {
            rParenPos = peekCount;
            break;
        }
        if (peek->TokenType == tok_star)
        {
            seenStar = true;
            if (peekCount == 2)
                return false;
        }
        else if (IsBinaryOperator(peek->TokenType, expr_flags_none))
        {
            return false;
        }
        if (!IsTypeToken(peek->TokenType))
        {
            return false;
        }
        else
        {
            if (peek->TokenType == tok_identifier && seenStar)
                return false;
        }
    }

    if (rParenPos)
    {
        metac_token_t* afterParen =
            MetaCParser_PeekToken(self, rParenPos + 1);
        if (!afterParen || IsPunctuationToken(afterParen->TokenType))
            return false;
    }

    return true;
}

typedef enum typescan_flags_t
{
    TypeScan_None               = 0,
    TypeScan_SeenStar           = (1 << 1),
    TypeScan_FirstWasIdentifier = (1 << 2),
} typescan_flags_t;

#define U32(VAR) \
    (*(uint32_t*)(&VAR))

static inline bool CouldBeType(metac_parser_t* self, metac_token_enum_t tok, parse_expression_flags_t eflags)
{
    bool result = false;
    metac_token_t* peek = 0;
    uint32_t peekN = 2;
    typescan_flags_t flags = TypeScan_None;

    if (eflags & expr_flags_pp)
        return false;

    if (tok == tok_identifier)
    {
        U32(flags) |= TypeScan_FirstWasIdentifier;
    }

    if (tok == tok_star)
    {
        result = false;
    }
    else while (IsTypeToken(tok))
    {
        result |= true;
        // if we see a * set the seen_star flag
        // if we then see an identifier after the star
        // the expression cannot be a type literal
        if (tok == tok_star)
            U32(flags) |= TypeScan_SeenStar;
        else if (((flags & TypeScan_SeenStar) != 0)
              && tok == tok_identifier)
        {
            result = false;
            break;
        }
        peek = MetaCParser_PeekToken(self, peekN++);
        tok = (peek ? peek->TokenType : tok_eof);
    }
    // if we just had a single identifier don't
    // treat it as a type
    if ((flags & TypeScan_FirstWasIdentifier) != 0
      && peekN <= 3)
    {
        result = false;
    }

    return result;
}

static metac_type_modifiers ParseTypeModifiers(metac_parser_t* self);

decl_type_t* MetaCParser_ParseTypeDeclaration(metac_parser_t* self,
                                              metac_declaration_t* parent,
                                              metac_declaration_t* prev);

static const metac_location_t invalidLocation = {0,0,0,0,0};
#define cast_key 0x4520d2
#define CRC32C_QUESTION 0xc372d93b
#define CRC32C_PLUSPLUS 0x61d225eb
#define CRC32C_MINUSMINUS 0x2ebc9331

metac_expression_t* MetaCParser_ParsePrimaryExpression(metac_parser_t* self, parse_expression_flags_t flags)
{
    metac_expression_t* result = 0;

    metac_token_t* currentToken = MetaCParser_PeekToken(self, 1);
    if (currentToken)
    {
/*
        printf("ParsePrimaryExpression-currentToken: %s\n", MetaCTokenEnum_toChars(currentToken->TokenType));
        if (currentToken->TokenType == tok_identifier)
            printf("    id: %s\n", IdentifierPtrToCharPtr(&self->Lexer->IdentifierTable, currentToken->IdentifierPtr));
*/
    }
#if !defined(NO_PREPROCESSOR)
    if (self->Preprocessor)
    {
        printf("Preproc is there -- InDefine: %u\n",
               self->Preprocessor->DefineTokenStackCount > 0);
    }
#endif
    metac_token_enum_t tokenType =
        (currentToken ? currentToken->TokenType : tok_eof);

    metac_location_t loc = (currentToken ?
        LocationFromToken(self, currentToken) :
        invalidLocation);
    uint32_t hash = 0;

#ifdef TYPE_EXP
    if (CouldBeType(self, tokenType, flags))
    {
        decl_type_t* type =
            MetaCParser_ParseTypeDeclaration(self, 0, 0);
        result = AllocNewExpression(exp_type);
        result->TypeExp = type;
        MetaCLocation_Expand(&loc,
            self->LocationStorage.Locations[result->TypeExp->LocationIdx - 4]);
        result->Hash = CRC32C_VALUE(type_key, result->TypeExp->Hash);
    }
    else
#endif

    if (tokenType == tok_lParen && CouldBeCast(self, tokenType))
    {
        hash = cast_key;
        result = AllocNewExpression(exp_cast);
        //typedef unsigned int b;
        metac_token_t* lParen = MetaCParser_Match(self, tok_lParen);
        metac_type_modifiers typeModifiers = ParseTypeModifiers(self);

        if (IsTypeToken(MetaCParser_PeekToken(self, 1)->TokenType))
        {
            result->CastType = MetaCParser_ParseTypeDeclaration(self, 0, 0);
            result->CastType->TypeModifiers =  typeModifiers;
        }
        else
        {
            AllocNewDeclaration(decl_type, &result->CastType);
            result->CastType->TypeModifiers =  typeModifiers;
            result->CastType->TypeKind = type_modifiers;
            result->CastType->Hash = CRC32C_VALUE(~0, typeModifiers);
        }
        hash = CRC32C_VALUE(hash, result->CastType->Hash);
        // printf("Parsed CastType: '%s'\n", MetaCPrinter_PrintDeclaration(&self->DebugPrinter, result->CastType));
        MetaCParser_Match(self, tok_rParen);
        result->CastExp = MetaCParser_ParseExpression(self, flags, 0);
        hash = CRC32C_VALUE(hash, result->CastExp->Hash);
        MetaCLocation_Expand(&loc, self->LocationStorage.Locations[result->CastExp->LocationIdx - 4]);
        result->Hash = hash;
    }
    else if (tokenType == tok_uint)
    {
        MetaCParser_Match(self, tok_uint);
        result = AllocNewExpression(exp_signed_integer);
        result->ValueI64 = currentToken->ValueI64;
        int32_t val32 = (int32_t)currentToken->ValueI64;
        // only hash 32bit if the value could fit into an i32
        if (val32 == currentToken->ValueI64)
        {
            result->Hash = CRC32C_VALUE(~0, val32);
        }
        else
        {
            result->Hash =
                    CRC32C_VALUE(~0, currentToken->ValueI64);
        }
        //PushOperand(result);
    }
    else if (tokenType == tok_string)
    {
        // result = GetOrAddStringLiteral(_string_table, currentToken);
        MetaCParser_Match(self, tok_string);
        result = AllocNewExpression(exp_string);
        result->StringPtr = RegisterString(self, currentToken);
        result->StringKey = currentToken->StringKey;
        result->Hash = currentToken->StringKey;
        //PushOperand(result);
    }
    else if (tokenType == tok_char)
    {
        MetaCParser_Match(self, tok_char);
        result = AllocNewExpression(exp_char);
        const uint32_t length = currentToken->charLength;
        const char* chars = currentToken->chars;
        const uint32_t hash = crc32c_nozero(~0, chars, length);

        (*(uint64_t*)result->Chars) =
            (*(uint64_t*) chars);
        result->CharKey = CHAR_KEY(hash, length);
        result->Hash = result->CharKey;
    }
    else if (tokenType == tok_identifier)
    {
        result = AllocNewExpression(exp_identifier);
        MetaCParser_Match(self, tok_identifier);
        result->IdentifierPtr = RegisterIdentifier(self, currentToken);
        result->IdentifierKey = currentToken->IdentifierKey;
        result->Hash = currentToken->IdentifierKey;
        //PushOperand(result);
    }
    else if (tokenType == tok_lParen)
    {
        self->OpenParens++;
        MetaCParser_Match(self, tok_lParen);
        result = AllocNewExpression(exp_paren);
        {
            if (!MetaCParser_PeekMatch(self, tok_rParen, 1))
                result->E1 = MetaCParser_ParseExpression(self, expr_flags_none, 0);
        }
        //PushOperator(exp_paren);
        result->Hash = CRC32C_VALUE(crc32c_nozero(~0, "()", 2), result->E1->Hash);
        //PushOperand(result);
        metac_token_t* endParen =
            MetaCParser_Match(self, tok_rParen);
        MetaCLocation_Expand(&loc, LocationFromToken(self, endParen));
        self->OpenParens--;
        //PopOperator(exp_paren);
    }
    else if (tokenType == tok_lBrace)
    {
        // self->OpenBraces++;
        MetaCParser_Match(self, tok_lBrace);
        exp_tuple_t* tupleList = (exp_tuple_t*)emptyPointer;
        uint32_t hash = ~0;

        exp_tuple_t** nextElement = &tupleList;
        uint32_t nElements = 0;
        while (!MetaCParser_PeekMatch(self, tok_rBrace, 1))
        {
            nElements++;
            assert((*nextElement) == _emptyPointer);

            (*nextElement) = (exp_tuple_t*)AllocNewExpression(exp_tuple);
            metac_expression_t* exp = MetaCParser_ParseExpression(self, expr_flags_call, 0);
            hash = CRC32C_VALUE(hash, exp->Hash);
            ((*nextElement)->Expression) = exp;
            nextElement = &((*nextElement)->Next);
            (*nextElement) = (exp_tuple_t*) _emptyPointer;

            if(MetaCParser_PeekMatch(self, tok_comma, 1))
            {
                MetaCParser_Match(self, tok_comma);
            }
        }
        metac_token_t* endBrace =
            MetaCParser_Match(self, tok_rBrace);
        MetaCLocation_Expand(&loc, LocationFromToken(self, endBrace));

        result = AllocNewExpression(exp_tuple);
        result->Hash = hash;
        result->TupleExpressionList = tupleList;
        result->TupleExpressionCount = nElements;

        //PopOperator(exp_call);
    }
/*
    else if (tokenType == tok_pp___FUNCTION__)
    {
        MetaCParser_Match(self, tok_pp___FUNCTION__);
        result = AllocNewExpression(exp_string);
        const char* chars = "__FUNCTION__";
        const uint32_t length = strlen(chars);
        const uint32_t hash = crc32c_nozero(~0, chars, length);
        const uint32_t stringKey = STRING_KEY(hash, length);

        metac_identifier_ptr_t stringPtr =
            GetOrAddIdentifier(&self->StringTable, stringKey, chars);
        result->StringKey = stringKey;
        result->StringPtr = stringPtr;
        result->Hash = stringKey;
    }*/
    else
    {
        assert(0); // Not a primary Expression;
    }

    result->LocationIdx = MetaCLocationStorage_Store(&self->LocationStorage, loc);
    return result;
}

metac_expression_t* MetaCParser_ParsePostfixExpression(metac_parser_t* self,
                                                       metac_expression_t* left)
{
    metac_expression_t* result = 0;

    metac_token_t* peek = MetaCParser_PeekToken(self, 1);

    metac_token_enum_t peekTokenType = peek->TokenType;

    metac_location_t loc =
        self->LocationStorage.Locations[left->LocationIdx - 4];

    if (peekTokenType == tok_plusplus)
    {
        MetaCParser_Match(self, peekTokenType);
        metac_expression_t* E1 = left;
        result = AllocNewExpression(exp_post_increment);
        result->E1 = E1;
        uint32_t hash = CRC32C_PLUSPLUS;
        result->Hash = CRC32C_VALUE(result->E1->Hash, hash);
    }
    else if (peekTokenType == tok_minusminus)
    {
        MetaCParser_Match(self, peekTokenType);

        metac_expression_t* E1 = left;
        result = AllocNewExpression(exp_post_decrement);
        result->E1 = E1;
        uint32_t hash = CRC32C_MINUSMINUS;
        result->Hash = CRC32C_VALUE(result->E1->Hash, hash);
    }
    else
        assert(!"Unknown postfix expression, this function should never have been called");

    MetaCLocation_Expand(&loc, LocationFromToken(self, peek));
    result->LocationIdx =
        MetaCLocationStorage_Store(&self->LocationStorage, loc);

    return result;
}
decl_type_t* MetaCParser_ParseTypeDeclaration(metac_parser_t* self, metac_declaration_t* parent, metac_declaration_t* prev);

static inline metac_expression_t* ParseDotSpecialExpression(metac_parser_t* self,
                                                            metac_expression_kind_t k)
{
    metac_expression_t* result = 0;
    metac_token_t* startToken;

    if (MetaCParser_PeekMatch(self, tok_dot, 1))
    {
        startToken = MetaCParser_Match(self, tok_dot);
        metac_location_t loc = LocationFromToken(self, startToken);
        metac_token_t* peek;
        peek = MetaCParser_PeekToken(self, 1);
        if (!peek)
        {
            fprintf(stderr, "Expected expression after '.special.'\n");
        }
        else
        {
            result = AllocNewExpression(k);
            result->E1 = MetaCParser_ParseExpression(self, expr_flags_none, 0);
            metac_location_t endLoc = self->LocationStorage.Locations[result->E1->LocationIdx - 4];
            MetaCLocation_Expand(&loc, endLoc);
            result->LocationIdx = MetaCLocationStorage_Store(&self->LocationStorage, loc);
        }
    }

    return result;
}

static inline uint32_t PtrVOnMatch(metac_parser_t* self,
                                   metac_identifier_ptr_t ptr,
                                   const char* matchStr,
                                   uint32_t matchLen)
{
    const char* idChars = IdentifierPtrToCharPtr(&self->IdentifierTable,
                                                 ptr);
    return ((!memcmp(idChars, matchStr, matchLen)) ? ptr.v : 0);
}


static inline metac_expression_t* ParseUnaryDotExpression(metac_parser_t* self)
{
    metac_expression_t* result = 0;

    metac_token_t* peek;
    peek = MetaCParser_PeekToken(self, 1);
    if (peek && peek->TokenType == tok_identifier)
    {
        switch(peek->IdentifierKey)
        {
            case compiler_key:
            {
                metac_identifier_ptr_t identifierPtr =
                    RegisterIdentifier(self, peek);
                if (self->SpecialNamePtr_Compiler.v == identifierPtr.v)
                {
                    MetaCParser_Match(self, tok_identifier);
                    result = ParseDotSpecialExpression(self, exp_dot_compiler);
                }
            } break;
            case context_key:
            {
                metac_identifier_ptr_t identifierPtr =
                    RegisterIdentifier(self, peek);
                if (self->SpecialNamePtr_Context.v == identifierPtr.v)
                {
                    MetaCParser_Match(self, tok_identifier);
                    result = ParseDotSpecialExpression(self, exp_dot_context);
                }
            } break;
            case target_key:
            {
                metac_identifier_ptr_t identifierPtr =
                    RegisterIdentifier(self, peek);
                if (self->SpecialNamePtr_Target.v == identifierPtr.v)
                {
                    MetaCParser_Match(self, tok_identifier);
                    result = ParseDotSpecialExpression(self, exp_dot_target);
                }
            } break;
        }
    }

    if (!result)
    {
        result = AllocNewExpression(exp_unary_dot);
        result->E1 = MetaCParser_ParseExpression(self, expr_flags_unary, 0);
        result->Hash = CRC32C_VALUE(
            crc32c_nozero(~0, ".", sizeof(".") - 1),
            result->E1->Hash
        );
    }

    return result;
}
#define CRC32C_AND   0xab9ec598
#define CRC32C_MINUS 0x32176ea3
#define CRC32C_BANG  0x7f54a173
#define CRC32C_STAR  0xe6dd0a48

metac_expression_t* MetaCParser_ParseUnaryExpression(metac_parser_t* self, parse_expression_flags_t eflags)
{
    metac_expression_t* result = 0;

    metac_token_t* currentToken = MetaCParser_PeekToken(self, 1);
    metac_token_enum_t tokenType =
        (currentToken ? currentToken->TokenType : tok_eof);
    metac_location_t loc = LocationFromToken(self, currentToken);

    bool isPrimaryExp = false;

    if (tokenType == tok_dot)
    {
        MetaCParser_Match(self, tok_dot);
        result = ParseUnaryDotExpression(self);
    }
    else if (tokenType == tok_kw_eject)
    {
        MetaCParser_Match(self, tok_kw_eject);
        result = AllocNewExpression(exp_eject);
        //PushOperator(exp_eject);
        result->E1 = MetaCParser_ParseExpression(self, expr_flags_none, 0);
        result->Hash = CRC32C_VALUE(eject_key, result->E1->Hash);
        //PushOperand(result);
        //PopOperator(exp_eject);
    }
    else if (tokenType == tok_kw_inject)
    {
        MetaCParser_Match(self, tok_kw_inject);
        result = AllocNewExpression(exp_inject);
        //PushOperator(exp_inject);
        result->E1 = MetaCParser_ParseExpression(self, expr_flags_none, 0);
        result->Hash = CRC32C_VALUE(inject_key, result->E1->Hash);
        //PushOperand(result);
        //PopOperator(exp_inject);
    }
    else if (tokenType == tok_kw_typeof)
    {
        MetaCParser_Match(self, tok_kw_typeof);
        result = AllocNewExpression(exp_typeof);
        //PushOperator(exp_typeof);
        metac_token_t* nextToken = MetaCParser_PeekToken(self, 1);
        if (!nextToken || nextToken->TokenType != tok_lParen)
        {
            ParseError(loc, "Expected typeof to be followed by '('");
        }

        metac_expression_t* parenExp = MetaCParser_ParseExpression(self, expr_flags_none, 0);
        //PopOperator(exp_typeof);
        assert(parenExp->Kind == exp_paren);
        result->E1 = parenExp->E1;
    }
    else if (tokenType == tok_kw_sizeof)
    {
        MetaCParser_Match(self, tok_kw_sizeof);
        result = AllocNewExpression(exp_sizeof);
        metac_token_t* nextToken = MetaCParser_PeekToken(self, 1);
        bool wasParen = false;
        if (nextToken->TokenType == tok_lParen)
        {
            wasParen = true;
            MetaCParser_Match(self, tok_lParen);
        }
        result->E1 = MetaCParser_ParseExpression(self, cast(parse_expression_flags_t)(eflags & expr_flags_pp), 0);
        if (wasParen)
        {
            MetaCParser_Match(self, tok_rParen);
        }
        result->Hash = CRC32C_VALUE(sizeof_key, result->E1->Hash);
    }
    else if (tokenType == tok_kw_assert)
    {
        MetaCParser_Match(self, tok_kw_assert);
        result = AllocNewExpression(exp_assert);
        //PushOperator(exp_assert);
        metac_token_t* nextToken = MetaCParser_PeekToken(self, 1);
        if (!nextToken || nextToken->TokenType != tok_lParen)
        {
            ParseError(loc, "Expected assert to be followed by '('");
        }
        metac_expression_t* parenExp = MetaCParser_ParseExpression(self, expr_flags_none, 0);
        //PopOperator(exp_assert);
        assert(parenExp->Kind == exp_paren);
        result->E1 = parenExp->E1;
        result->Hash = CRC32C_VALUE(assert_key, result->E1->Hash);
    }
    else if (tokenType == tok_minus)
    {
        MetaCParser_Match(self, tok_minus);
        result = AllocNewExpression(exp_umin);
        //PushOperator(exp_addr);
        result->E1 = MetaCParser_ParseExpression(self,
			cast(parse_expression_flags_t)(expr_flags_unary | (eflags & expr_flags_pp)), 0);
        result->Hash = CRC32C_VALUE(CRC32C_MINUS, result->E1->Hash);
    }
    else if (tokenType == tok_minusminus)
    {
        MetaCParser_Match(self, tok_minusminus);
        result = AllocNewExpression(exp_decrement);
        //PushOperator(exp_addr);
        result->E1 = MetaCParser_ParseExpression(self,
			cast(parse_expression_flags_t)(expr_flags_unary | (eflags & expr_flags_pp)), 0);
        result->Hash = CRC32C_VALUE(CRC32C_MINUSMINUS, result->E1->Hash);
    }
    else if (tokenType == tok_plusplus)
    {
        MetaCParser_Match(self, tok_plusplus);
        result = AllocNewExpression(exp_increment);
        //PushOperator(exp_addr);
        result->E1 = MetaCParser_ParseExpression(self,
			cast(parse_expression_flags_t)(expr_flags_unary | (eflags & expr_flags_pp)), 0);
        result->Hash = CRC32C_VALUE(CRC32C_PLUSPLUS, result->E1->Hash);
    }
    else if (tokenType == tok_and)
    {
        MetaCParser_Match(self, tok_and);
        result = AllocNewExpression(exp_addr);
        //PushOperator(exp_addr);
        result->E1 = MetaCParser_ParseExpression(self, cast(parse_expression_flags_t)(expr_flags_addr | (eflags & expr_flags_pp)), 0);
        result->Hash = CRC32C_VALUE(CRC32C_AND, result->E1->Hash);
        //PushOperand(result);
        //PopOperator(exp_addr);
    }
    else if (tokenType == tok_star)
    {
        MetaCParser_Match(self, tok_star);
        result = AllocNewExpression(exp_ptr);
        //PushOperator(exp_ptr);
        result->E1 = MetaCParser_ParseExpression(self, cast(parse_expression_flags_t)(expr_flags_unary | (eflags & expr_flags_pp)), 0);
        result->Hash = CRC32C_VALUE(CRC32C_STAR, result->E1->Hash);
        //PushOperand(result);
    }
    else if (tokenType == tok_bang)
    {
        MetaCParser_Match(self, tok_bang);
        result = AllocNewExpression(exp_not);
        result->E1 = MetaCParser_ParseExpression(self,
			cast(parse_expression_flags_t)(expr_flags_unary | (eflags & expr_flags_pp)), 0);
        result->Hash = CRC32C_VALUE(CRC32C_BANG, result->E1->Hash);
    }
    else if (tokenType == tok_cat)
    {
        MetaCParser_Match(self, tok_cat);
        result = AllocNewExpression(exp_compl);
        result->E1 = MetaCParser_ParseExpression(self,
			cast(parse_expression_flags_t)(expr_flags_unary | (eflags & expr_flags_pp)), 0);
        result->Hash = CRC32C_VALUE(
            crc32c_nozero(~0, "~", 1),
            result->E1->Hash
        );
    }
    else if (IsPrimaryExpressionToken(tokenType))
    {
        isPrimaryExp = true;
        result = MetaCParser_ParsePrimaryExpression(self, eflags);
    }
    else
    {
        if (tokenType != tok_eof)
        {
            metac_location_t location =
                self->Lexer->LocationStorage.Locations[currentToken->LocationId - 4];
            fprintf(stderr, "line: %d col: %d\n", location.StartLine, location.StartColumn);
        }
        fprintf(stderr, "Unexpected Token: %s\n", MetaCTokenEnum_toChars(tokenType));
        assert(0);
    }

    if (!isPrimaryExp)
    {
        metac_location_t endLoc =
            self->LocationStorage.Locations[result->E1->LocationIdx - 4];
        MetaCLocation_Expand(&loc, endLoc);
    }

    result->LocationIdx =
        MetaCLocationStorage_Store(&self->LocationStorage, loc);

    metac_token_t* peek_post = MetaCParser_PeekToken(self, 1);
    metac_token_enum_t postTokenType =
        (peek_post ? peek_post->TokenType : tok_invalid);

    if (IsPostfixOperator(postTokenType))
    {
        result = MetaCParser_ParsePostfixExpression(self, result);
    }

    return result;
}

exp_argument_t* MetaCParser_ParseArgumentList(metac_parser_t* self)
{
    metac_location_t loc =
        LocationFromToken(self, MetaCParser_PeekToken(self, 0));

    metac_token_t* peekToken = MetaCParser_PeekToken(self, 1);
    exp_argument_t* arguments = (exp_argument_t*) _emptyPointer;
    exp_argument_t** nextArgument = &arguments;
    uint32_t nArguments = 0;
    uint32_t hash = ~0;

    while (!MetaCParser_PeekMatch(self, tok_rParen, 1))
    {
        nArguments++;
        assert((*nextArgument) == _emptyPointer);

        (*nextArgument) = (exp_argument_t*)AllocNewExpression(exp_argument);
        metac_expression_t* exp = MetaCParser_ParseExpression(self, expr_flags_call, 0);
        ((*nextArgument)->Expression) = exp;
        assert(exp->Hash);
        hash = CRC32C_VALUE(hash, exp->Hash);
        nextArgument = &((*nextArgument)->Next);
        (*nextArgument) = (exp_argument_t*) _emptyPointer;
        if(MetaCParser_PeekMatch(self, tok_comma, 1))
        {
            MetaCParser_Match(self, tok_comma);
        }
    }

    if (arguments != emptyPointer)
    {
        arguments->Hash = hash;
        metac_token_t* rParen = MetaCParser_PeekToken(self, 1);
        MetaCLocation_Expand(&loc, LocationFromToken(self, rParen));
        arguments->LocationIdx =
            MetaCLocationStorage_Store(&self->LocationStorage, loc);
    }

    return arguments;
}
metac_expression_t* MetaCParser_ParseBinaryExpression(metac_parser_t* self,
                                                      parse_expression_flags_t eflags,
                                                      metac_expression_t* left,
                                                      uint32_t min_prec)
{
    metac_expression_t* result = 0;

//    metac_location_t* startLocation =
//        self->Lexer->LocationStorage.Locations + (left->LocationIdx - 4);

    metac_token_t* peekToken;
    metac_token_enum_t peekTokenType;
    metac_location_t loc =
        self->LocationStorage.Locations[left->LocationIdx - 4];

    peekToken = MetaCParser_PeekToken(self, 1);
    peekTokenType = (peekToken ? peekToken->TokenType : tok_eof);
    // a * b + c * d
    // a * b | (a * b) + c |
    // here we pop the stack
/*
    if (peekTokenType == tok_lBracket)
    {
        MetaCParser_Match(self, tok_lBracket);

        metac_expression_t* E1 = left;
        result = AllocNewExpression(exp_index);
        result->E1 = E1;
        result->E2 = MetaCParser_ParseExpression(self, eflags, 0);

        MetaCParser_Match(self, tok_rBracket);
    }
    else
*/
/*
    if (peekTokenType == tok_lParen)
    {
        self->OpenParens++;
        MetaCParser_Match(self, tok_lParen);
        metac_expression_t* E1 = left;
        assert(peekToken);

        metac_token_t* peekToken = MetaCParser_PeekToken(self, 1);
        exp_argument_t* arguments = (exp_argument_t*) _emptyPointer;
        exp_argument_t** nextArgument = &arguments;
        uint32_t nArguments = 0;

        while (!MetaCParser_PeekMatch(self, tok_rParen, 1))
        {
            nArguments++;
            assert((*nextArgument) == _emptyPointer);

            (*nextArgument) = (exp_argument_t*)AllocNewExpression(exp_argument);
            ((*nextArgument)->Expression) = MetaCParser_ParseExpression(self, expr_flags_call, 0);
            nextArgument = &((*nextArgument)->Next);
            (*nextArgument) = (exp_argument_t*) _emptyPointer;
            if(MetaCParser_PeekMatch(self, tok_comma, 1))
            {
                MetaCParser_Match(self, tok_comma);
            }
        }
        metac_token_t* rParen = MetaCParser_Match(self, tok_rParen);
        self->OpenParens--;

        result = AllocNewExpression(exp_call);
        result->E1 = E1;
        result->E2 = (metac_expression_t*)arguments;
        MetaCLocation_Expand(&loc, LocationFromToken(self, rParen));
        //PopOperator(exp_call);
    }
    else*/ if (IsBinaryOperator(peekTokenType, eflags))
    {
        bool rhsIsArgs = false;
        metac_expression_kind_t exp_right;

        while(IsBinaryOperator(peekTokenType, eflags)
           && OpToPrecedence(BinExpTypeFromTokenType(peekTokenType)) >= min_prec)
        {
            exp_right = BinExpTypeFromTokenType(peekTokenType);
            uint32_t opPrecedence = OpToPrecedence(exp_right);
            metac_token_t*  startTok = MetaCParser_Match(self, peekTokenType);
            metac_location_t rhsLoc = LocationFromToken(self, startTok);
            metac_expression_t* rhs;

            if (exp_right == exp_index)
            {
                rhs = MetaCParser_ParseExpression(self, eflags, 0);
                metac_token_t* rBracket =
                    MetaCParser_Match(self, tok_rBracket);
                MetaCLocation_Expand(&rhsLoc,
                    LocationFromToken(self, rBracket));
            }
            else if (exp_right == exp_template_instance
                && MetaCParser_PeekMatch(self, tok_lParen, 1))
            {
                MetaCParser_Match(self, tok_lParen);
                goto LparseArgumentList;
            }
            else if (exp_right == exp_call)
            {
            LparseArgumentList:
                rhs = (metac_expression_t*)MetaCParser_ParseArgumentList(self);
                if ((metac_node_t)rhs != emptyPointer)
                    rhsIsArgs = true;

                metac_token_t* rParen =
                    MetaCParser_Match(self, tok_rParen);
                MetaCLocation_Expand(&rhsLoc,
                    LocationFromToken(self, rParen));
            }
            else
            {
                rhs = MetaCParser_ParseUnaryExpression(self, eflags);
            }
            peekToken = MetaCParser_PeekToken(self, 1);
            peekTokenType = (peekToken ? peekToken->TokenType : tok_eof);

            while(IsBinaryOperator(peekTokenType, eflags)
               && opPrecedence <
                  OpToPrecedence(BinExpTypeFromTokenType(peekTokenType)))
            {
                rhs = MetaCParser_ParseBinaryExpression(self, eflags, rhs, opPrecedence + 0);
                peekToken = MetaCParser_PeekToken(self, 1);
                peekTokenType = (peekToken ? peekToken->TokenType : tok_eof);
            }

            result = AllocNewExpression(exp_right);
            result->E1 = left;
            result->E2 = rhs;
            if (rhs != emptyPointer)
            {
                result->Hash = CRC32C_VALUE(left->Hash, rhs->Hash);
                MetaCLocation_Expand(&rhsLoc,
                    self->LocationStorage.Locations[rhs->LocationIdx - 4]);
            }
            else
            {
                uint32_t emptyHash = ~0;
                result->Hash = CRC32C_VALUE(left->Hash, emptyHash);
            }

            MetaCLocation_Expand(&loc, rhsLoc);
            result->LocationIdx =
                MetaCLocationStorage_Store(&self->LocationStorage, loc);
            left = result;
        }
    }
    else
    {
        assert(!"Unexpected Token");
    }
    if (!result->Hash)
    {
        if (result->E2 == emptyPointer)
        {
            result->Hash = result->E1->Hash;
        }
        else
        {
            result->Hash = ~0;
            result->Hash = CRC32C_VALUE(result->E1->Hash, result->E2->Hash);
        }

        result->LocationIdx = MetaCLocationStorage_Store(&self->LocationStorage, loc);
    }
    return result;
}

bool IsBinaryExp(metac_expression_kind_t kind)
{
    return ((kind >= FIRST_BINARY_EXP(TOK_SELF)) && (kind <= LAST_BINARY_EXP(TOK_SELF))
            || kind == exp_index);
}

bool IsBinaryAssignExp(metac_expression_kind_t kind)
{
   return (kind >= exp_add_ass && kind <= exp_rsh_ass);
}
#ifndef NO_PREPROCESSOR
/// returns the directive and places the tokens which follow it into
/// the token buffer,
metac_preprocessor_directive_t MetaCParser_ParsePreproc(metac_parser_t* self,
                                                        metac_preprocessor_t* preproc,
                                                        metac_token_buffer_t* buffer)
{
    metac_token_t* result= buffer->Ptr;

    MetaCParser_Match(self, tok_hash);
    metac_token_t* peek = MetaCParser_PeekToken(self, 1);
    metac_token_enum_t tokenType = (peek ? peek->TokenType : tok_eof);
    metac_preprocessor_directive_t directive = pp_invalid;

    switch (tokenType)
    {
        case tok_kw_if:
        {
            directive = pp_if;
        } break;
        case tok_kw_else:
        {
            directive = pp_else;
        } break;
        case tok_identifier: {
            switch(peek->IdentifierKey)
            {
                case eval_key:
                {
                    directive = pp_eval;
                } goto Lmatch;

                case ifdef_key:
                {
                    directive = pp_ifdef;
                } goto Lmatch;

                case elif_key:
                {
                    directive = pp_elif;
                } goto Lmatch;

                case include_key:
                {
                    directive = pp_include;
                } goto Lmatch;

                case define_key:
                {
                    directive = pp_define;
                } goto Lmatch;

                case endif_key:
                {
                    directive = pp_endif;
                } goto Lmatch;

                default:
                    printf("couldn't match directive\n");
                break;

                Lmatch:
                {
                    printf("CurrentTokenIndex before match: %u\n", self->CurrentTokenIndex);
                    MetaCParser_Match(self, tok_identifier);
                    //if (directive == pp_eval)
                    //    asm ("int $3;");
                    printf("CurrentTokenIndex after match: %u\n", self->CurrentTokenIndex);
                }
            }
        }
    }

    return directive;
}
#endif

metac_expression_t* MetaCParser_ParseExpression(metac_parser_t* self,
                                                parse_expression_flags_t eflags,
                                                metac_expression_t* prev)
{
    metac_expression_t* result = 0;
    metac_token_t* currentToken = MetaCParser_PeekToken(self, 1);
    metac_token_enum_t tokenType =
        (currentToken ? currentToken->TokenType : tok_invalid);
    metac_location_t loc =  {0};
    if (currentToken)
        loc = LocationFromToken(self, currentToken);

    // printf("ParseExpression -- currentToken: %s\n", MetaCTokenEnum_toChars(currentToken->TokenType));
    bool isDefined = false;
#if !defined(NO_PREPROCESSOR)
    if (eflags & expr_flags_pp)
    {
        if (tokenType == tok_identifier &&
            currentToken->IdentifierKey == defined_key)
        {
            isDefined = true;
            MetaCParser_Match(self, tok_identifier);
            currentToken = MetaCParser_PeekToken(self, 1);
            tokenType =
                (currentToken ? currentToken->TokenType : tok_invalid);
        }
    }
#endif

    if (IsPrimaryExpressionToken(tokenType))
    {
        result = MetaCParser_ParsePrimaryExpression(self, eflags);
    }
    else  if (!prev /*|| prec_left > prec_right*/)
    {
        result = MetaCParser_ParseUnaryExpression(self, eflags);
    }
    else
    {
        result = MetaCParser_ParseBinaryExpression(self, eflags, prev, 0);
    }

#if !defined(NO_PREPROCESSOR)
    if (isDefined)
    {
        if (result->Kind == exp_paren)
            result = result->E1;

        assert(result->Kind == exp_identifier);

        metac_expression_t* call = AllocNewExpression(exp_call);
        exp_argument_t* args = cast(exp_argument_t*) AllocNewExpression(exp_argument);
        metac_expression_t* definedIdExp = AllocNewExpression(exp_identifier);
        metac_location_t endLoc = self->LocationStorage.Locations[result->LocationIdx - 4];
        MetaCLocation_Expand(&loc, endLoc);

        definedIdExp->IdentifierKey = defined_key;
        definedIdExp->IdentifierPtr = self->SpecialNamePtr_Defined;

        uint32_t hash = defined_key;
        call->LocationIdx = MetaCLocationStorage_Store(&self->LocationStorage, loc);
        call->Hash = CRC32C_VALUE(hash, result->Hash);
        args->Expression = result;
        args->Next = cast(exp_argument_t*) emptyPointer;

        call->E2 = cast(metac_expression_t*)args;
        call->E1 = definedIdExp;

        result = call;
    }
#endif
//    printf("TokenType: %s\n", MetaCTokenEnum_toChars(tokenType));

    metac_token_t* peekNext = MetaCParser_PeekToken(self, 1);
    if (peekNext)
    {
        uint32_t min_prec = 0;

        tokenType = peekNext->TokenType;
        //within a call we must not treat a comma as a binary expression
        if (((eflags & (expr_flags_call | expr_flags_enum)) != 0) && tokenType == tok_comma)
            goto LreturnExp;

        if ((eflags & expr_flags_unary))
            goto LreturnExp;

        if (IsBinaryOperator(tokenType, eflags))
        {
            //uint32_t prec = OpToPrecedence(BinExpTypeFromTokenType(tokenType));
            uint32_t prec = OpToPrecedence(result->Kind);
            if (prec < min_prec)
                return result;
            result = MetaCParser_ParseBinaryExpression(self, eflags, result, min_prec);
            peekNext = MetaCParser_PeekToken(self, 1);
            tokenType = (peekNext ? peekNext->TokenType : tok_eof);
        }
        else if (IsPostfixOperator(tokenType))
        {
            result = MetaCParser_ParsePostfixExpression(self, result);
        }
        else if (peekNext->TokenType == tok_lParen)
        {
            result = MetaCParser_ParseBinaryExpression(self, eflags, result, OpToPrecedence(exp_call));
        }
        else if (peekNext->TokenType == tok_bang)
        {
            result = MetaCParser_ParseBinaryExpression(self, eflags, result, OpToPrecedence(exp_template_instance));
        }
        else if (tokenType == tok_lBracket)
        {
            result = MetaCParser_ParseBinaryExpression(self, eflags, result, OpToPrecedence(exp_index));
        }
        else if (tokenType == tok_rBracket || tokenType == tok_rParen)
        {
            // there's nothing to see here crray on
        }

        if (tokenType == tok_question)
        {
            uint32_t hash = CRC32C_QUESTION;
            // inline parse Ternary Expression
            MetaCParser_Match(self, tok_question);
            metac_expression_t* Econd = result;
            hash = CRC32C_VALUE(hash, Econd->Hash);
            metac_expression_t* E1 =
                MetaCParser_ParseExpression(self, expr_flags_none, 0);
            hash = CRC32C_VALUE(hash, E1->Hash);
            MetaCParser_Match(self, tok_colon);
            metac_expression_t* E2 =
                MetaCParser_ParseExpression(self, expr_flags_none, 0);
            hash = CRC32C_VALUE(hash, E2->Hash);
            result = AllocNewExpression(exp_ternary);
            result->E1 = E1;
            result->E2 = E2;
            result->Econd = Econd;
            result->Hash = hash;
            result->LocationIdx = MetaCLocationStorage_Store(&self->LocationStorage,
                MetaCLocationStorage_FromPair(&self->LocationStorage,
                                              result->Econd->LocationIdx, result->E2->LocationIdx));
        }

        //else assert(!"Stray Input");
    }

    assert(result->Hash != 0);
LreturnExp:
    return result;
}

static inline bool IsDeclType(metac_declaration_t* decl)
{
    metac_declaration_kind_t kind = decl->DeclKind;
    return (kind == decl_type
         || kind == decl_type_struct
         || kind == decl_type_enum
         || kind == decl_type_union);
}


#define ErrorDeclaration() \
    (metac_declaration_t*)0

#define ErrorTypeDeclaration() \
    (decl_type_t*)0


static decl_type_array_t* ParseArraySuffix(metac_parser_t* self, decl_type_t* type);

static metac_type_modifiers ParseTypeModifiers(metac_parser_t* self)
{
    metac_type_modifiers typeModifiers = typemod_none;
    metac_token_t* currentToken;
LnextToken:
    currentToken = MetaCParser_PeekToken(self, 1);
    metac_token_enum_t tokenType =
        (currentToken ? currentToken->TokenType : tok_invalid);

    if (tokenType == tok_kw_const)
    {
        MetaCParser_Match(self, tok_kw_const);
        U32(typeModifiers) |= typemod_const;
        goto LnextToken;
    }
    else if (tokenType == tok_kw_unsigned)
    {
        MetaCParser_Match(self, tok_kw_unsigned);
        U32(typeModifiers) |= typemod_unsigned;
        goto LnextToken;
    }
    else if (tokenType == tok_kw_signed)
    {
        MetaCParser_Match(self, tok_kw_signed);
        U32(typeModifiers) |= typemod_signed;
        goto LnextToken;
    }

    return typeModifiers;
}

decl_type_t* MetaCParser_ParseTypeDeclaration(metac_parser_t* self, metac_declaration_t* parent, metac_declaration_t* prev)
{
    decl_type_t* result = 0;

    metac_type_modifiers typeModifiers = ParseTypeModifiers(self);

    decl_type_t* type = AllocNewDeclaration(decl_type, &result);
    metac_token_t* currentToken = 0;
    metac_location_t loc =
        LocationFromToken(self, MetaCParser_PeekToken(self, 1));
    uint32_t hash = type_key;

    currentToken = MetaCParser_NextToken(self);
    metac_token_enum_t tokenType =
        (currentToken ? currentToken->TokenType : tok_invalid);


    while(IsTypeToken(tokenType))
    {
        if (tokenType == tok_identifier)
        {
            type->TypeKind = type_identifier;
            type->TypeIdentifier = RegisterIdentifier(self, currentToken);
            U32(type->TypeModifiers) |= typeModifiers;
            if (type->TypeIdentifier.v == self->SpecialNamePtr_Type.v)
            {
                type->TypeKind = type_type;
            }
            hash = CRC32C_VALUE(hash, type->TypeIdentifier);
            assert((typeModifiers & typemod_unsigned) == 0);
            break;
        }

        if (tokenType >= tok_kw_auto && tokenType <= tok_kw_double)
        {
            type->TypeKind = (metac_type_kind_t)(type_auto + (tokenType - tok_kw_auto));
            U32(type->TypeModifiers) |= typeModifiers;
            if (tokenType == tok_kw_long)
            {
                if (MetaCParser_PeekMatch(self, tok_kw_long, 1))
                {
                    MetaCParser_Match(self, tok_kw_long);
                    type->TypeKind = type_long_long;
                }
                else if (MetaCParser_PeekMatch(self, tok_kw_double, 1))
                {
                    MetaCParser_Match(self, tok_kw_double);
                    type->TypeKind = type_long_double;
                }
            }
            // eat the optional int behind short,long and long long int
            if (tokenType == tok_kw_short
             || tokenType == tok_kw_long)
            {
                if (MetaCParser_PeekMatch(self, tok_kw_int, 1))
                {
                    MetaCParser_Match(self, tok_kw_int);
                }
            }
            assert(hash == type_key);
            hash ^= (type->TypeModifiers & typemod_unsigned);
            hash = CRC32C_VALUE(hash, type->TypeKind);
            break;
        }
        else if (tokenType == tok_star)
        {
            ParseError(loc, "* is unexpected to start a type declaration");
            assert(0); // this is not supposed to happen
        }
        else if (tokenType == tok_kw_struct || tokenType == tok_kw_union)
        {
            bool isStruct = tokenType == tok_kw_struct;

            bool isPredeclated = true;

            decl_type_struct_t* struct_ = AllocNewDeclaration(decl_type_struct, &result);
            type = (decl_type_t*)struct_;

            if (isStruct)
            {
                struct_->TypeKind = type_struct;
                struct_->DeclKind = decl_type_struct;
            }
            else
            {
                struct_->TypeKind = type_union;
                struct_->DeclKind = decl_type_union;
            }

            if (MetaCParser_PeekMatch(self, tok_identifier, 1))
            {
                metac_token_t* structName = MetaCParser_NextToken(self);
                struct_->Identifier = RegisterIdentifier(self, structName);
            }
            else
            {
                struct_->Identifier = empty_identifier;
            }

            switch(struct_->TypeKind)
            {
                case type_struct:
                    hash = struct_key;
                break;
                case type_union:
                    hash = union_key;
                break;
            }
            hash = CRC32C_VALUE(hash, struct_->Identifier);
            if (tokenType == tok_kw_struct)
            {
                if (MetaCParser_PeekMatch(self, tok_colon, 1))
                {
                    MetaCParser_Match(self, tok_colon);
                    metac_token_t* baseName = MetaCParser_NextToken(self);
                    struct_->BaseIdentifier = RegisterIdentifier(self, baseName);
                }
                goto LSetEmptyBase;
            }
            else
            {
        LSetEmptyBase:
                struct_->BaseIdentifier = empty_identifier;
            }
            hash = CRC32C_VALUE(hash, struct_->BaseIdentifier);

            if (MetaCParser_PeekMatch(self, tok_lBrace, 1))
            {
                MetaCParser_Match(self, tok_lBrace);
                decl_field_t **nextMemberPtr = &struct_->Fields;

                isPredeclated = false;
                while(!MetaCParser_PeekMatch(self, tok_rBrace, 1))
                {
                    decl_field_t* field =
                        AllocNewDeclaration(decl_field, (metac_declaration_t**)
                                            nextMemberPtr);
                    field->Next = (decl_field_t*)_emptyPointer;
                    metac_declaration_t *decl =
                        (metac_declaration_t*)MetaCParser_ParseDeclaration(self, (metac_declaration_t*)struct_);
                    assert(decl->Hash != 0);
                    hash = CRC32C_VALUE(hash, decl->Hash);

                    if (decl->DeclKind == decl_comment)
                        continue;

                    if (decl->DeclKind == decl_variable)
                    {
                        field->Field = (decl_variable_t*)decl;
                    }
                    else
                    {
                        // make sure only struct or unions are anonymous
                        assert(decl->DeclKind == decl_type_struct
                            || decl->DeclKind == decl_type_union);
                        // he have to synthezise the variable
                        AllocNewDeclaration(decl_variable, &field->Field);
                        field->Field->VarType = cast(decl_type_t*)decl;
                        field->Field->VarIdentifier = empty_identifier;
                    }

                    // MetaCParser_Match(self, tok_semicolon);
                    nextMemberPtr = &field->Next;
                    struct_->FieldCount++;
                }
                MetaCParser_Match(self, tok_rBrace);
            }
            else
            {
                struct_->Fields = cast(decl_field_t*)emptyPointer;
                printf("We just have a decl\n");
            }
            break;
        }
        else if (tokenType == tok_kw_enum)
        {
            decl_type_enum_t* enum_ = AllocNewDeclaration(decl_type_enum, &result);

            if (MetaCParser_PeekMatch(self, tok_identifier, 1))
            {
                metac_token_t* structName = MetaCParser_NextToken(self);
                enum_->Identifier = RegisterIdentifier(self, structName);
            }
            else
            {
                enum_->Identifier = empty_identifier;
            }
            hash = CRC32C_VALUE(enum_key, enum_->Identifier);

            if (MetaCParser_PeekMatch(self, tok_colon, 1))
            {
                MetaCParser_Match(self, tok_colon);
                enum_->BaseType =
                    MetaCParser_ParseTypeDeclaration(self, 0, 0);
                hash = CRC32C_VALUE(hash, enum_->BaseType->Hash);
            }

            if (MetaCParser_PeekMatch(self, tok_lBrace, 1))
            {
                MetaCParser_Match(self, tok_lBrace);
                decl_enum_member_t **nextMemberPtr = &enum_->Members;
                uint32_t memberCount = 0;

                while(!MetaCParser_PeekMatch(self, tok_rBrace, 1))
                {
                    memberCount++;
                    decl_enum_member_t* member =
                        AllocNewDeclaration(decl_enum_member, (metac_declaration_t**)
                                            nextMemberPtr);
                    member->Next = (decl_enum_member_t*) _emptyPointer;
                    metac_token_t* idToken = MetaCParser_Match(self, tok_identifier);
                    member->Name = RegisterIdentifier(self, idToken);
                    hash = CRC32C_VALUE(hash, member->Name);
                    metac_token_t* afterName = MetaCParser_PeekToken(self, 1);
                    if (afterName
                         && ((afterName->TokenType == tok_comma)
                          | (afterName->TokenType == tok_rBrace)))
                    {
                        member->Value = (metac_expression_t*)emptyPointer;
                        if (afterName->TokenType == tok_rBrace)
                            break;
                    }
                    else
                    {
                        MetaCParser_Match(self, tok_assign);
                        member->Value = MetaCParser_ParseExpression(self, expr_flags_enum, 0);
                        assert(member->Value->Hash != 0);
                        hash = CRC32C_VALUE(hash, member->Value->Hash);
                    }
                    metac_token_t* afterMember = MetaCParser_PeekToken(self, 1);
                    if (afterMember->TokenType != tok_rBrace)
                        MetaCParser_Match(self, tok_comma);
                    nextMemberPtr = &member->Next;
                }
                MetaCParser_Match(self, tok_rBrace);
                enum_->MemberCount = memberCount;
            }
            break;
        }
    }

    assert(hash != 0);
    result->Hash = hash;

    currentToken = MetaCParser_PeekToken(self, 1);
    tokenType =
        (currentToken ? currentToken->TokenType : tok_invalid);

    bool nextIsConst = false;
    while(tokenType == tok_star
       || tokenType == tok_kw_const)
    {
        if (tokenType == tok_kw_const)
        {
            MetaCParser_Match(self, tok_kw_const);
            nextIsConst = true;
        }
        MetaCParser_Match(self, tok_star);
        decl_type_t* elementType = result;
        decl_type_ptr_t* ptr = AllocNewDeclaration(decl_type_ptr, &result);
        if (nextIsConst)
            U32(ptr->TypeModifiers) |= typemod_const;
        ptr->ElementType = elementType;
        ptr->Hash = hash = CRC32C_VALUE(CRC32C_S("*"), hash);
        currentToken = MetaCParser_PeekToken(self, 1);
        tokenType =
            (currentToken ? currentToken->TokenType : tok_invalid);
        nextIsConst = false;
    }

    metac_location_t endLoc =
        LocationFromToken(self, currentToken ? currentToken : MetaCParser_PeekToken(self, 0));
    MetaCLocation_Expand(&loc, endLoc);
    result->LocationIdx = MetaCLocationStorage_Store(&self->LocationStorage, loc);
    return result;
}

bool IsTypeDecl(metac_declaration_kind_t kind)
{
    return ((kind >= FIRST_DECL_TYPE(TOK_SELF))
          & (kind <= LAST_DECL_TYPE(TOK_SELF)));
}
uint32_t HashDecl(metac_declaration_t* decl);

decl_parameter_list_t ParseParameterList(metac_parser_t* self,
                                         decl_function_t* parent)
{
    decl_parameter_list_t result = {(decl_parameter_t*)emptyPointer};
    uint32_t parameterCount = 0;
    decl_parameter_t** nextParam = &result.List;

    while (!MetaCParser_PeekMatch(self, tok_rParen, 1))
    {
        assert((*nextParam) == emptyPointer);

        if (result.IsVariadic)
        {
            ParseError(self->LastLocation,
                        "you cannot have ... at any position other than the end of the parameter list\n");
        }

        if (MetaCParser_PeekMatch(self, tok_dotdotdot, 1))
        {
            MetaCParser_Match(self, tok_dotdotdot);
            result.IsVariadic = true;
            continue;
        }

        decl_parameter_t* param;
        AllocNewDeclaration(decl_parameter, &param);
        parameterCount++;
        (*nextParam) = param;

        metac_declaration_t* paramDecl =
			MetaCParser_ParseDeclaration(self, (metac_declaration_t*)parent);
        if (paramDecl->DeclKind == decl_variable)
        {
            param->Parameter = (decl_variable_t*)
                paramDecl;
        }
        else if (IsTypeDecl(paramDecl->DeclKind))
        {
            // now we synthezie a variable without name
            decl_variable_t* var;
            AllocNewDeclaration(decl_variable, &var);
            var->Hash = 0;
            var->VarType = (decl_type_t*)paramDecl;
            var->VarIdentifier = empty_identifier;
            var->VarInitExpression = (metac_expression_t*) emptyPointer;
            var->Hash = HashDecl(cast(metac_declaration_t*)var);
            param->Parameter = var;
        }
        else
        {
            metac_token_t* peek = MetaCParser_PeekToken(self , 1);
            metac_location_t loc = LocationFromToken(self, peek);
            ParseError(loc, "Invalid parameter");
        }
        nextParam = &param->Next;
        (*nextParam) = (decl_parameter_t*) _emptyPointer;

        if (MetaCParser_PeekMatch(self, tok_comma, 1))
        {
            MetaCParser_Match(self, tok_comma);
        }
        else
        {
            assert(MetaCParser_PeekMatch(self, tok_rParen, 1));
        }
    }
    MetaCParser_Match(self, tok_rParen);
    result.ParameterCount = parameterCount;

    return result;
}

static stmt_block_t* MetaCParser_ParseBlockStatement(metac_parser_t* self,
                                                     metac_statement_t* parent,
                                                     metac_statement_t* prev);
void EatAttributes(metac_parser_t* self)
{
    while(MetaCParser_PeekMatch(self, tok_kw___attribute__, 1))
    {
        MetaCParser_Match(self, tok_kw___attribute__);
        MetaCParser_Match(self, tok_lParen);
        MetaCParser_Match(self, tok_lParen);
        metac_token_t *currentToken =
            MetaCParser_PeekToken(self, 1);

        while(currentToken && currentToken->TokenType != tok_rParen)
        {
            MetaCParser_Match(self, currentToken->TokenType);
            currentToken = MetaCParser_PeekToken(self, 1);
        }

        if (currentToken)
        {
            MetaCParser_Match(self, tok_rParen);
            MetaCParser_Match(self, tok_rParen);
        }
    }
}
static metac_storageclasses_t ParseStorageClasses(metac_parser_t* self);

decl_function_t* ParseFunctionDeclaration(metac_parser_t* self, decl_type_t* type)
{
    decl_function_t result;

    metac_token_t* id = MetaCParser_Match(self, tok_identifier);
    metac_location_t loc = LocationFromToken(self, id);
    metac_identifier_ptr_t identifier = RegisterIdentifier(self, id);

    MetaCParser_Match(self, tok_lParen);
    decl_function_t* funcDecl = AllocNewDeclaration(decl_function, &result);
    funcDecl->ReturnType = type;
    funcDecl->Identifier = identifier;

    funcDecl->FunctionBody = (stmt_block_t*) _emptyPointer;
    decl_parameter_list_t parameterList = ParseParameterList(self, funcDecl);
    funcDecl->Parameters = parameterList.List;
    funcDecl->ParameterCount = parameterList.ParameterCount;

    // eat attributes here
    EatAttributes(self);
    // eat storage classes
    ParseStorageClasses(self);

    if (MetaCParser_PeekMatch(self, tok_lBrace, 1))
    {
        funcDecl->FunctionBody = MetaCParser_ParseBlockStatement(self, 0, 0);
    }

    return funcDecl;
}
#define CRC32C_COLON 0xf683cd27
#define CRC32C_SLASH_SLASH 0xe8c2d328
#define CRC32C_BRACKETS 0x89b24289
#define CRC32C_SEMICOLON 0x4e84e24
#define CRC32C_PARENSTARPAREN 0xdb5bdc12

uint32_t HashDecl(metac_declaration_t* decl)
{
    uint32_t result = 0;

    switch(decl->DeclKind)
    {
        case decl_label:
        {
            decl_label_t* label = cast(decl_label_t*) decl;
            result = CRC32C_VALUE(CRC32C_COLON, label->Identifier);
        } break;
        case decl_comment:
        {
            decl_comment_t* comment = cast(decl_comment_t*) decl;
            result = crc32c(CRC32C_SLASH_SLASH, comment->Text, comment->Length);
        } break;
        case decl_type_typedef:
        {
            decl_type_typedef_t* type_typedef = (decl_type_typedef_t*) decl;
            result = typedef_key;
            result = CRC32C_VALUE(result, type_typedef->Type->Hash);
            result = CRC32C_VALUE(result, type_typedef->Identifier);
        } break;

        case decl_type:
        {
            decl_type_t* type = (decl_type_t*) decl;
            if (type->TypeKind == type_identifier)
            {
                result = CRC32C_VALUE(type_key, type->TypeIdentifier);
            }
            else
            {
                result = CRC32C_VALUE(
                    (type_key ^ type->TypeModifiers & typemod_unsigned),
                    type->TypeKind
                );
            }
        } break;

        case decl_type_union:
            result = union_key;
            goto LhashAgg;
        case decl_type_struct:
            result = struct_key;
            goto LhashAgg;
        LhashAgg:
        {
            decl_type_struct_t* agg = cast(decl_type_struct_t*) decl;
            result = CRC32C_VALUE(result, agg->Identifier);
            result = CRC32C_VALUE(result, agg->BaseIdentifier);

            if (agg->Fields && agg->Fields != emptyPointer)
            {
                decl_field_t* field = agg->Fields;
                for(uint32_t i = 0; i < agg->FieldCount; i++)
                {
                    uint32_t fieldHash = HashDecl(cast(metac_declaration_t*)field->Field);
                    result = CRC32C_VALUE(result, fieldHash);
                    field = field->Next;
                }
                assert(field->Next = cast(decl_field_t*)emptyPointer);
            }
        } break;
        case decl_variable:
        {
            decl_variable_t* variable = cast(decl_variable_t*) decl;
            result = HashDecl(cast(metac_declaration_t*)variable->VarType);
            result = CRC32C_VALUE(result, variable->VarIdentifier);
        } break;
        case decl_type_ptr:
        {
            decl_type_ptr_t* type_ptr = cast(decl_type_ptr_t*) decl;
            uint32_t ElementTypeHash = HashDecl(cast(metac_declaration_t*)type_ptr->ElementType);
            result = CRC32C_VALUE(CRC32C_STAR, ElementTypeHash);
        } break;
        case decl_type_functiontype:
        {
            decl_type_functiontype_t* type_functiontype = cast(decl_type_functiontype_t*) decl;
            uint32_t hash = CRC32C_PARENSTARPAREN;
            const uint32_t nParams = type_functiontype->ParameterCount;
            hash = CRC32C_VALUE(hash, type_functiontype->ReturnType);
            hash = CRC32C_VALUE(hash, nParams);
            decl_parameter_t* Param = type_functiontype->Parameters;
            for(uint32_t i = 0; i < nParams; i++)
            {
                uint32_t paramHash = HashDecl(cast(metac_declaration_t*)Param->Parameter);
                hash = CRC32C_VALUE(hash, paramHash);
            }
            result = hash;
        } break;
        case decl_type_array:
        {
            decl_type_array_t* type_array = cast(decl_type_array_t*) decl;
            uint32_t ElementTypeHash = HashDecl(cast(metac_declaration_t*)type_array->ElementType);
            if (type_array->Dim->Kind == exp_signed_integer)
            {
                uint32_t dimToHash = (uint32_t)type_array->Dim->ValueU64;
                result = CRC32C_VALUE(CRC32C_BRACKETS, dimToHash);
            }
            else
            {
                result = CRC32C_BRACKETS;
            }

            result = CRC32C_VALUE(result, ElementTypeHash);
        }
        break;
        default: assert(0);
    }
    if (decl->Hash)
        assert(decl->Hash == result);
    assert(result != 0);

    return result;
}
#define __thread_key 0x8c8d19
static metac_storageclasses_t ParseStorageClasses(metac_parser_t* self)
{
    metac_token_t* currentToken = 0;
    metac_token_enum_t currentTokenType = tok_invalid;
    metac_storageclasses_t result = cast(metac_storageclasses_t)0;

    for(;;)
    {
        currentToken = MetaCParser_PeekToken(self, 1);
        if (!currentToken)
            break;
        currentTokenType = currentToken->TokenType;
        if (currentTokenType == tok_kw_static)
        {
            MetaCParser_Match(self, tok_kw_static);
            U32(result) |= storageclass_static;
        }
        else if (currentTokenType == tok_kw_inline)
        {
            MetaCParser_Match(self, tok_kw_inline);
            U32(result) |= storageclass_inline;
        }
        else if (currentTokenType == tok_kw_extern)
        {
            MetaCParser_Match(self, tok_kw_extern);
            U32(result) |= storageclass_extern;
        }
        else if (currentTokenType == tok_kw_volatile)
        {
            MetaCParser_Match(self, tok_kw_volatile);
            U32(result) |= storageclass_volatile;
        }
        else if (currentTokenType == tok_identifier)
        {
            if (currentToken->IdentifierKey == __thread_key)
            {
                //TODO register __thread as a known identifier
                // so we can compare the ptr
                // for now accept this as the __thread storage class
                MetaCParser_Match(self, tok_identifier);
                U32(result) |= storageclass_thread;
            }
            else
                break;
        }
        else
            break;
    }

    return result;
}

metac_declaration_t* MetaCParser_ParseDeclaration(metac_parser_t* self, metac_declaration_t* parent)
{
    // get rid of attribs before declarations
    //TODO acutally keep track of them
    EatAttributes(self);

    metac_storageclasses_t stc = ParseStorageClasses(self);

    metac_token_t* currentToken = MetaCParser_PeekToken(self, 1);
    metac_token_enum_t tokenType =
        (currentToken ? currentToken->TokenType : tok_invalid);
    metac_location_t loc =
        currentToken ? LocationFromToken(self, currentToken) : invalidLocation;
    metac_declaration_t* result = 0;

    decl_type_t* type = 0;

    if (tokenType == tok_eof)
        return 0;

    // Let's deal with labels right at the start.
    if (MetaCParser_PeekMatch(self, tok_identifier, 1))
    {
        metac_token_t* peek2 = MetaCParser_PeekToken(self, 2);
        if (peek2->TokenType == tok_colon)
        {
            metac_token_t* idToken = MetaCParser_Match(self, tok_identifier);
            metac_token_t* colon = MetaCParser_Match(self, tok_colon);
            decl_label_t* label = AllocNewDeclaration(decl_label, &result);
            label->LocationIdx = MetaCLocationStorage_Store(&self->LocationStorage,
                MetaCLocationStorage_FromPair(&self->Lexer->LocationStorage,
                                              idToken->LocationId, colon->LocationId));

            label->Identifier = RegisterIdentifier(self, idToken);
            result->Hash = CRC32C_VALUE(CRC32C_COLON, label->Identifier);
            self->CurrentLabel = label;
            return result;
        }
    }
    // Right after let's deal with comments
    else if (tokenType == tok_comment_multi
          || tokenType == tok_comment_single)
    {
        metac_token_t* tok = MetaCParser_Match(self, tokenType);
        decl_comment_t* comment = AllocNewDeclaration(decl_comment, &result);
        comment->Text = tok->CommentBegin;
        comment->Length = tok->CommentLength;
        self->CurrentComment = *tok;
        result->Hash = crc32c(CRC32C_SLASH_SLASH, comment->Text, comment->Length);
        return result;
    }

    metac_storageclasses_t stc2 = ParseStorageClasses(self);
    // check for duplicates
    U32(stc) |= U32(stc2);

    if (IsTypeToken(tokenType))
    {
         type = MetaCParser_ParseTypeDeclaration(self, parent, 0);
         assert(type->Hash != 0);
         // let's assume that this type might be all there is
         result = (metac_declaration_t*)type;
    }

    if (tokenType == tok_kw_typedef)
    {
        MetaCParser_Match(self, tok_kw_typedef);
        currentToken = MetaCParser_PeekToken(self, 1);
            tokenType =
        (currentToken ? currentToken->TokenType : tok_invalid);
        uint32_t hash = typedef_key;

        decl_type_typedef_t* typdef = AllocNewDeclaration(decl_type_typedef, &result);

        // typedefs are exactly like variables
        decl_variable_t* var = (decl_variable_t*)MetaCParser_ParseDeclaration(self, (metac_declaration_t*) typdef);
        typdef->Type = var->VarType;
        typdef->Identifier = var->VarIdentifier;
        assert(typdef->Type->Hash != 0);
        hash = CRC32C_VALUE(hash, typdef->Type->Hash);
        hash = CRC32C_VALUE(hash, typdef->Identifier);

        result->Hash = hash;
        assert(result->Hash == HashDecl(result));
        goto LendDecl;
    }

    U32(stc2) |= ParseStorageClasses(self);

    if (type)
    {
        if (MetaCParser_PeekMatch(self, tok_lParen, 1))
        {
            // this might be a function pointer
            MetaCParser_Match(self, tok_lParen);
            decl_variable_t* fPtrVar;
            self->OpenParens++;
            if (MetaCParser_PeekMatch(self, tok_star, 1))
            {
                MetaCParser_Match(self, tok_star);
                // this is quite likely a function pointer
                metac_type_modifiers typemods = ParseTypeModifiers(self);
                // TODO acutally use those mods;
                metac_token_t* fPtrid = MetaCParser_PeekToken(self, 1);
                // Eat all type modifiers which might be there
                if (fPtrid->TokenType == tok_identifier)
                {
                    MetaCParser_Match(self, tok_identifier);
                    // this would be the function pointer name then
                    fPtrVar = AllocNewDeclaration(decl_variable, &result);
                    fPtrVar->VarInitExpression = (metac_expression_t*)emptyPointer;

                    fPtrVar->VarIdentifier = RegisterIdentifier(self, fPtrid);
                    MetaCParser_Match(self, tok_rParen);
                    self->OpenParens--;
                    // we eat the paren before we do the parameterList
                    //TODO maybe paramter list parsing should eat both parens
                    MetaCParser_Match(self, tok_lParen);
                    decl_type_t* returnType = type;
                    decl_parameter_list_t paramterList =
                        ParseParameterList(self, 0);

                    decl_type_functiontype_t* functionType =
                        AllocNewDeclaration(decl_type_functiontype, &fPtrVar->VarType);

                    uint32_t hash = CRC32C_PARENSTARPAREN;

                    functionType->ReturnType = returnType;
                    hash = CRC32C_VALUE(hash, returnType->TypeHeader.Hash);
                    functionType->ParameterCount = paramterList.ParameterCount;
                    const uint32_t pCount = functionType->ParameterCount;
                    hash = CRC32C_VALUE(hash, functionType->ParameterCount);
                    functionType->Parameters = paramterList.List;
                    hash = CRC32C_VALUE(hash, paramterList.Hash);
                    functionType->Hash = hash;

                    assert(fPtrVar->VarType == (decl_type_t*)functionType);
                    fPtrVar->Hash = CRC32C_VALUE(CRC32C_STAR, hash);
                }
            }
        }
        else if (MetaCParser_PeekMatch(self, tok_identifier, 1))
        {
            metac_token_t* afterId = MetaCParser_PeekToken(self, 2);
            if (afterId && afterId->TokenType == tok_lParen)
            {
                decl_function_t* funcDecl = ParseFunctionDeclaration(self, type);
                result = (metac_declaration_t*) funcDecl;
            }
            else
            {
                decl_variable_t* varDecl = AllocNewDeclaration(decl_variable, &result);
                metac_token_t* idToken = MetaCParser_Match(self, tok_identifier);
                metac_identifier_ptr_t identifier = RegisterIdentifier(self, idToken);
//            varDecl.LocationIdx =
//                MetaCLocationStorage_StartLoc(&parser.locationStorage,
//                    MetaCLocationStorage_StartLine(&parser.lexer.locationStorage, type.LocationIdx));

                varDecl->VarType = type;
                varDecl->VarIdentifier = identifier;
                varDecl->VarInitExpression = (metac_expression_t*)_emptyPointer;

                varDecl->Hash = CRC32C_VALUE(varDecl->VarType->Hash, varDecl->VarIdentifier);

                while (MetaCParser_PeekMatch(self, tok_lBracket, 1))
                {
                    varDecl->VarType = (decl_type_t*)
                        ParseArraySuffix(self, varDecl->VarType);
                }
                while (MetaCParser_PeekMatch(self, tok_full_slice, 1))
                {
                    MetaCParser_Match(self, tok_full_slice);
                    printf("saw a [] ...  we should do something about that\n");
                    //varDecl->VarType = (decl_type_t*)ParseArraySuffix(self, varDecl->VarType);
                }

                // check for bitfield decl
                if (MetaCParser_PeekMatch(self, tok_colon, 1))
                {
                    //TODO make sure this is only done for structs
                    MetaCParser_Match(self, tok_colon);
                    metac_token_t* bitSz = MetaCParser_Match(self, tok_uint);
                    printf("ignoring bitfield spec : %d\n", bitSz->ValueI64);
                }

                if (MetaCParser_PeekMatch(self, tok_assign, 1))
                {
                    MetaCParser_Match(self, tok_assign);
                    varDecl->VarInitExpression = MetaCParser_ParseExpression(self, expr_flags_none, 0);
                }
            }
        }
    }
    else
    {
        ParseErrorF(loc, "A declaration is expected to start with a type CurrentToken %s\n", MetaCTokenEnum_toChars(tokenType));
    }
LendDecl:
    // eat a semicolon if there is one this is more a repl kindof thing
    if (MetaCParser_PeekMatch(self, tok_semicolon, 1))
        MetaCParser_Match(self, tok_semicolon);

    return result;
}

static decl_type_array_t* ParseArraySuffix(metac_parser_t* self, decl_type_t* type)
{
    decl_type_array_t* arrayType = 0;
    uint32_t hash = 0;

    if (MetaCParser_PeekMatch(self, tok_lBracket, 1))
    {
        MetaCParser_Match(self, tok_lBracket);
        arrayType =
            AllocNewDeclaration(decl_type_array, &arrayType);
        //TODO ErrorMessage array must have numeric dimension
        arrayType->ElementType = type;

        arrayType->Dim = MetaCParser_ParseExpression(self, expr_flags_none, 0);
        uint32_t dimToHash = 0;
        if (arrayType->Dim->Kind == exp_signed_integer)
        {
            uint32_t dimToHash = (uint32_t)arrayType->Dim->ValueU64;
            hash = CRC32C_VALUE(CRC32C_BRACKETS, dimToHash);
        }
        else
        {
            hash = CRC32C_BRACKETS;
        }
        hash = CRC32C_VALUE(hash, type->Hash);

        MetaCParser_Match(self, tok_rBracket);
        type = (decl_type_t*)arrayType;
    }
    arrayType->Hash = hash;
    assert(arrayType);
    return arrayType;
}


#define ErrorStatement() \
    (metac_statement_t*)0
static inline void PrintStatement(metac_printer_t* self, metac_statement_t* stmt);

metac_statement_t* MetaCParser_ParseStatement(metac_parser_t* self,
                                              metac_statement_t* parent,
                                              metac_statement_t* prev)
{
    metac_statement_t* result = 0;

    metac_token_t* currentToken = MetaCParser_PeekToken(self, 1);
    metac_token_enum_t tokenType =
        (currentToken ? currentToken->TokenType : tok_invalid);
    metac_location_t loc = LocationFromToken(self, currentToken);
    metac_token_t* peek2;
    uint32_t hash = 0;

    while (tokenType == tok_semicolon)
    {
        MetaCParser_Match(self, tok_semicolon);
        currentToken = MetaCParser_PeekToken(self, 1);
        tokenType =
            (currentToken ? currentToken->TokenType : tok_invalid);
        if (tokenType != tok_semicolon)
        {
            stmt_empty_t* s = AllocNewStatement(stmt_empty, &result);
            s->LocationIdx = MetaCLocationStorage_Store(&self->LocationStorage,
                loc
            );
            s->Hash = CRC32C_SEMICOLON;
            return result;
        }
    }

    if (tokenType == tok_invalid)
    {
        return ErrorStatement();
    }

    if (self->CurrentBlockStatement)
        self->CurrentBlockStatement->StatementCount++;

    if (tokenType == tok_comment_multi
     || tokenType == tok_comment_single)
    {
        self->CurrentComment = *MetaCParser_Match(self, tokenType);
        stmt_comment_t* comment = AllocNewStatement(stmt_comment, &result);
        comment->Text = self->CurrentComment.CommentBegin;
        comment->Length = self->CurrentComment.CommentLength;
        comment->Hash = ~0;
    }
    else if (tokenType == tok_kw_if)
    {
        stmt_if_t* if_stmt = AllocNewStatement(stmt_if, &result);
        MetaCParser_Match(self, tok_kw_if);
        hash = if_key;
        if (!MetaCParser_PeekMatch(self, tok_lParen, 0))
        {
            ParseError(loc, "execpected ( after if\n");
            return ErrorStatement();
        }
        MetaCParser_Match(self, tok_lParen);
        if_stmt->IfCond =
            MetaCParser_ParseExpression(self, expr_flags_none, 0);
        hash = CRC32C_VALUE(hash, if_stmt->IfCond);
        MetaCParser_Match(self, tok_rParen);
        if_stmt->IfBody = MetaCParser_ParseStatement(self, (metac_statement_t*)result, 0);
        hash = CRC32C_VALUE(hash, if_stmt->IfBody->Hash);

        if (MetaCParser_PeekMatch(self, tok_kw_else, 1))
        {
            MetaCParser_Match(self, tok_kw_else);
            if_stmt->ElseBody = (metac_statement_t*)MetaCParser_ParseStatement(self, (metac_statement_t*)result, 0);
            hash = CRC32C_VALUE(hash, if_stmt->ElseBody->Hash);
        }
        else
        {
            if_stmt->ElseBody = (metac_statement_t*)_emptyPointer;
        }
        result->Hash = hash;
        goto LdoneWithStatement;
    }
    else if (tokenType == tok_kw_while)
    {
        stmt_while_t * while_stmt = AllocNewStatement(stmt_while, &result);
        MetaCParser_Match(self, tok_kw_while);
        MetaCParser_Match(self, tok_lParen);
        while_stmt->WhileExp =
            MetaCParser_ParseExpression(self, expr_flags_none, 0);
        hash = CRC32C_VALUE(hash, while_stmt->WhileExp->Hash);
        MetaCParser_Match(self, tok_rParen);
        while_stmt->WhileBody =
            MetaCParser_ParseStatement(self, (metac_statement_t*)while_stmt, 0);
        hash = CRC32C_VALUE(hash, while_stmt->WhileBody->Hash);
        while_stmt->Hash = hash;
    }
    else if (tokenType == tok_kw_for)
    {
        MetaCParser_Match(self, tok_kw_for);
        hash = for_key;
        stmt_for_t* for_ = AllocNewStatement(stmt_for, &result);
        MetaCParser_Match(self, tok_lParen);

        //TODO FIXME forInit can be an expression as well
        if (!MetaCParser_PeekMatch(self, tok_semicolon, 1))
        {
            metac_token_t* peek = MetaCParser_PeekToken(self, 1);
            metac_token_t* peek2 = MetaCParser_PeekToken(self, 2);

            if (IsDeclarationFirstToken(peek->TokenType)
             && IsDeclarationFirstToken(peek2->TokenType))
            {
                for_->ForInit = (metac_node_t)MetaCParser_ParseDeclaration(self, 0);
            }
            else
            {
                for_->ForInit = (metac_node_t)MetaCParser_ParseExpression(self, expr_flags_none, 0);
                MetaCParser_Match(self, tok_semicolon);
            }

            hash = CRC32C_VALUE(hash, for_->ForInit->Hash);
        }
        else
        {
            for_->ForInit = (metac_node_t)emptyPointer;
            MetaCParser_Match(self, tok_semicolon);
        }
        if (!MetaCParser_PeekMatch(self, tok_semicolon, 1))
        {
            for_->ForCond = MetaCParser_ParseExpression(self, expr_flags_none, 0);
            hash = CRC32C_VALUE(hash, for_->ForCond->Hash);
        }
        else
        {
            for_->ForCond = (metac_expression_t*)emptyPointer;
        }
        MetaCParser_Match(self, tok_semicolon);

        if (!MetaCParser_PeekMatch(self, tok_rParen, 1))
        {
            for_->ForPostLoop = MetaCParser_ParseExpression(self, expr_flags_none, 0);
            hash = CRC32C_VALUE(hash, for_->ForPostLoop->Hash);
        }
        else
        {
            for_->ForPostLoop = (metac_expression_t*)emptyPointer;
        }
        MetaCParser_Match(self, tok_rParen);
        for_->ForBody = MetaCParser_ParseStatement(self, (metac_statement_t*)for_, 0);
        result->Hash = hash;
    }
    else if (tokenType == tok_kw_switch)
    {
        hash = switch_key;
        stmt_switch_t* switch_ = AllocNewStatement(stmt_switch, &result);

        MetaCParser_Match(self, tok_kw_switch);
        MetaCParser_Match(self, tok_lParen);
        switch_->SwitchExp =
            MetaCParser_ParseExpression(self, expr_flags_none, 0);
        hash = CRC32C_VALUE(hash, switch_->SwitchExp->Hash);
        MetaCParser_Match(self, tok_rParen);
        if (!MetaCParser_PeekMatch(self, tok_lBrace, 0))
        {
            ParseError(loc, "parsing switch failed\n");
            return ErrorStatement();
        }

        switch_->SwitchBody =
            (metac_statement_t*)MetaCParser_ParseBlockStatement(self, result, 0);
        hash = CRC32C_VALUE(hash, switch_->SwitchBody->Hash);
        switch_->Hash = hash;
    }
    else if (tokenType == tok_identifier
        && (peek2 = MetaCParser_PeekToken(self, 2))
        && (peek2->TokenType == tok_colon))
    {
        hash = CRC32C_COLON;
        metac_token_t* label_tok = MetaCParser_Match(self, tok_identifier);
        MetaCParser_Match(self, tok_colon);

        stmt_label_t* label = AllocNewStatement(stmt_label, &result);

        label->Label = RegisterIdentifier(self, label_tok);
        hash = CRC32C_VALUE(hash, label->Label);
        label->Hash = hash;
    }
    else if (tokenType == tok_kw_goto)
    {
        stmt_goto_t* goto_ = AllocNewStatement(stmt_goto, &result);
        hash = goto_key;

        MetaCParser_Match(self, tok_kw_goto);
        metac_token_t* label = MetaCParser_Match(self, tok_identifier);
        goto_->GotoLabel = RegisterIdentifier(self, label);
        goto_->Hash = CRC32C_VALUE(hash, label->IdentifierPtr);
    }
    else if (tokenType == tok_kw_break)
    {
        stmt_break_t* break_ = AllocNewStatement(stmt_break, &result);
        hash = break_key;
        MetaCParser_Match(self, tok_kw_break);
        result->Hash = hash;
    }
    else if (tokenType == tok_kw_continue)
    {
        stmt_continue_t* continue_ = AllocNewStatement(stmt_continue, &result);
        hash = continue_key;
        MetaCParser_Match(self, tok_kw_continue);
        result->Hash = hash;
    }
    else if (tokenType == tok_kw_case || tokenType == tok_kw_default)
    {
        bool isCase = tokenType == tok_kw_case;
        hash = (isCase ? case_key : default_key);
        stmt_case_t* case_ = AllocNewStatement(stmt_case, &result);

        MetaCParser_Match(self, tokenType);
        if (isCase)
        {
            case_->CaseExp =
                MetaCParser_ParseExpression(self, expr_flags_none, 0);

            hash = CRC32C_VALUE(hash, case_->CaseExp->Hash);
        }
        else
        {
            case_->CaseExp = (metac_expression_t*)emptyPointer;
        }
        MetaCParser_Match(self, tok_colon);
        metac_token_t* peek = MetaCParser_PeekToken(self, 1);

        case_->CaseBody = (metac_statement_t*)_emptyPointer;
        metac_statement_t** nextStmtP = &case_->CaseBody;

        do
        {
            metac_statement_t* nextStmt =
                MetaCParser_ParseStatement(self, (metac_statement_t*)case_, 0);
            hash = CRC32C_VALUE(hash, nextStmt->Hash);
            nextStmt->Next = (metac_statement_t*)_emptyPointer;
            (*nextStmtP) = nextStmt;
            nextStmtP = &nextStmt->Next;
            peek = MetaCParser_PeekToken(self, 1);
            peek2 = MetaCParser_PeekToken(self, 2);
        } while((peek->TokenType != tok_kw_case
            && peek->TokenType != tok_rBrace
            && peek->TokenType != tok_kw_default
            && (peek2 == 0 || peek2->TokenType != tok_colon)));

        case_->Hash = hash;
    }
    else if (tokenType == tok_kw_return)
    {
        hash = return_key;
        stmt_return_t* return_ = AllocNewStatement(stmt_return, &result);
        MetaCParser_Match(self, tok_kw_return);
        if (MetaCParser_PeekMatch(self, tok_semicolon, 1))
        {
            return_->ReturnExp = (metac_expression_t*)_emptyPointer;
        }
        else
        {
            return_->ReturnExp = MetaCParser_ParseExpression(self, expr_flags_none, 0);
            hash = CRC32C_VALUE(hash, return_->ReturnExp->Hash);
        }
        return_->Hash = hash;
    }
    else if (tokenType == tok_kw_yield)
    {
        hash = yield_key;
        stmt_yield_t* yield_ = AllocNewStatement(stmt_yield, &result);
        MetaCParser_Match(self, tok_kw_yield);
        if (MetaCParser_PeekMatch(self, tok_semicolon, 1))
        {
            yield_->YieldExp = (metac_expression_t*)_emptyPointer;
        }
        else
        {
            yield_->YieldExp = MetaCParser_ParseExpression(self, expr_flags_none, 0);
            hash = CRC32C_VALUE(hash, yield_->YieldExp->Hash);
        }
        yield_->Hash = hash;
    }
    else if (tokenType == tok_kw_do)
    {
        hash = do_key;
        stmt_do_while_t* doWhile = AllocNewStatement(stmt_do_while, &result);
        MetaCParser_Match(self, tok_kw_do);

        doWhile->DoWhileBody = MetaCParser_ParseStatement(self, cast(metac_statement_t*)doWhile, prev);
        hash = CRC32C_VALUE(hash, doWhile->DoWhileBody->Hash);
        MetaCParser_Match(self, tok_kw_while);

        MetaCParser_Match(self, tok_lParen);
        doWhile->DoWhileExp = MetaCParser_ParseExpression(self, expr_flags_none, 0);
        hash = CRC32C_VALUE(hash, doWhile->DoWhileExp->Hash);
        MetaCParser_Match(self, tok_rParen);

        doWhile->Hash = hash;
    }
    else if (tokenType == tok_lBrace)
    {
        result = (metac_statement_t*)MetaCParser_ParseBlockStatement(self, parent, prev);
    }
    else if (IsDeclarationFirstToken(tokenType))
    {
        metac_token_t* peek2 = MetaCParser_PeekToken(self, 2);
        metac_token_t* peek3 = MetaCParser_PeekToken(self, 3);
        if (peek2 && IsDeclarationToken(peek2->TokenType)
            /*&& peek3 && peek3->TokenType != tok_assign*/)
        {
            metac_declaration_t* decl = MetaCParser_ParseDeclaration(self, 0);
            stmt_decl_t* declStmt = AllocNewStatement(stmt_decl, &result);
            assert(decl && decl->Hash);
            declStmt->Declaration = decl;
            //result = MetaCParser_ParseDeclarationStatement(self, parent);
            declStmt->Hash = decl->Hash;
        }
    }

    if (result && !result->Hash)
    {
        printf("Hash for %s unimplemented\n", StatementKind_toChars(result->StmtKind));
    }

    // if we didn't parse as a declaration try an expression as the last resort
    if (!result || result == emptyPointer)
    {
        metac_expression_t* exp = MetaCParser_ParseExpression(self, expr_flags_none, 0);
        stmt_exp_t* expStmt = AllocNewStatement(stmt_exp, &result);
        expStmt->Expression = exp;
        result->Hash = exp->Hash;
        //result = MetaCParser_ParseExpressionStatement(self, parent);
    }
LdoneWithStatement:
    if (prev)
        prev->Next = result;
    // printf("ParsedStatement:[%u] %s\n", result->Serial, MetaCPrinter_PrintStatement(&self->DebugPrinter, result));
    MetaCPrinter_Reset(&self->DebugPrinter);

    if(tokenType != tok_lBrace && MetaCParser_PeekMatch(self, tok_semicolon, 1))
    {
        // XXX it shouldn't stay this way ... but for now we want
        // to parse more function bodies.
        MetaCParser_Match(self, tok_semicolon);
    }


    return result;
}

static inline void MetaCParser_PushBlockStatement(metac_parser_t* self,
                                                  stmt_block_t* stmt)
{
    self->CurrentBlockStatement =
        self->BlockStatementStack[self->BlockStatementStackCount++] = stmt;
}

static inline void MetaCParser_PopBlockStatement(metac_parser_t* self,
                                                 stmt_block_t* stmt)
{
    assert(stmt == self->CurrentBlockStatement);

    if (--self->BlockStatementStackCount > 0)
    {
        self->CurrentBlockStatement =
            self->BlockStatementStack[self->BlockStatementStackCount - 1];
    }
    else
        self->CurrentBlockStatement = 0;
}

static stmt_block_t* MetaCParser_ParseBlockStatement(metac_parser_t* self,
                                                     metac_statement_t* parent,
                                                     metac_statement_t* prev)
{
    metac_token_t* lBrace = MetaCParser_Match(self, tok_lBrace);
    metac_location_t loc = LocationFromToken(self, lBrace);

    metac_statement_t* firstStatement = 0;
    metac_statement_t* nextStatement = 0;
    stmt_block_t* result;
    AllocNewStatement(stmt_block, &result);
    result->Hash = ~0;
    uint32_t hash = ~0;
    uint32_t nStatements = 0;
    MetaCParser_PushBlockStatement(self, result);

    for (;;)
    {
        metac_token_t* peekToken = MetaCParser_PeekToken(self, 1);
        metac_location_t loc = LocationFromToken(self, peekToken);

        if (peekToken && peekToken->TokenType == tok_rBrace)
        {
            if (!firstStatement)
            {
                firstStatement = (metac_statement_t*)_emptyPointer;
            }
            break;
        }
        nStatements++;

        if (!firstStatement)
        {
            firstStatement = MetaCParser_ParseStatement(self, (metac_statement_t*)result, firstStatement);
            nextStatement = firstStatement;
            if (nextStatement)
            {
                assert(nextStatement->Hash);
                hash = CRC32C_VALUE(hash, nextStatement->Hash);
            }
            else
            {
                ParseError(loc, "Statement expected");
            }
        }
        else
        {
            MetaCParser_ParseStatement(self, (metac_statement_t*)result, nextStatement);
            result->Hash = CRC32C_VALUE(result->Hash, nextStatement->Hash);
            if (nextStatement->Next && nextStatement->Next != emptyPointer)
            {
                nextStatement = nextStatement->Next;
            }
        }
    }

    result->Body = firstStatement;
    result->Hash = hash;
    result->StatementCount = nStatements;

    metac_token_t* rBrace = MetaCParser_Match(self, tok_rBrace);
    MetaCLocation_Expand(&loc, LocationFromToken(self, rBrace));
    result->LocationIdx = MetaCLocationStorage_Store(
        &self->LocationStorage, loc);

    MetaCParser_PopBlockStatement(self, result);

    return result;
}

/// static lexer for using in the g_lineParser
metac_lexer_t g_lineLexer = {
    g_lineLexer.inlineTokens,     0, ARRAY_SIZE(g_lineLexer.inlineTokens),
    {g_lineLexer.inlineLocations, 0, ARRAY_SIZE(g_lineLexer.inlineLocations)}
};
/// There can only be one LineParser as it uses static storage

/*
void LineLexerInit(void)
{
    g_lineParser.CurrentTokenIndex = 0;
    g_lineLexer.TokenCount = 0;
    g_lineLexer.LocationStorage.LocationSize = 0;

    ACCEL_INIT(g_lineLexer, Identifier, IDENTIFIER_LENGTH_SHIFT, 9);
    ACCEL_INIT(g_lineLexer, String, STRING_LENGTH_SHIFT, 9);

    if (g_lineParser.SpecialNamePtr_Compiler.v == 0)
    {
        ACCEL_INIT(g_lineParser, Identifier, IDENTIFIER_LENGTH_SHIFT, 9);
        ACCEL_INIT(g_lineParser, String, STRING_LENGTH_SHIFT, 9);
        InitSpecialIdentifier(&g_lineParser);
    }

    if (!g_lineParser.BlockStatementStack)
    {
        g_lineParser.BlockStatementStackCapacity = 8;
        g_lineParser.BlockStatementStackCount = 0;
        g_lineParser.BlockStatementStack = (stmt_block_t**)
            malloc(sizeof(stmt_block_t*) * g_lineParser.BlockStatementStackCapacity);
    }

    if (!g_lineParser.DebugPrinter.StringMemory)
    {
        MetaCPrinter_Init(&g_lineParser.DebugPrinter,
                          &g_lineParser.IdentifierTable,
                          &g_lineParser.StringTable);
    }

#if !defined(NO_PREPROCESSOR)
    g_lineParser.Preprocessor = 0;
#endif
}
*/
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

const char* MetaCNodeKind_toChars(metac_node_kind_t type)
{
    const char* result = 0;

#define CASE_MACRO(NODE_TYPE) \
    case NODE_TYPE : {result = #NODE_TYPE;} break;

    switch(type)
    {
        FOREACH_NODE_KIND(CASE_MACRO)
    }

    return result;

#undef CASE_MACRO
}

#include "metac_printer.h"

#include "metac_lpp.h"
#  ifdef TEST_PARSER
void TestParseExprssion(void)
{
    metac_printer_t printer;
    metac_lpp_t LPP;
    MetaCLPP_Init(&LPP);

    MetaCPrinter_Init(&printer,
        &LPP.Parser.IdentifierTable,
        &LPP.Parser.StringTable
    );
    metac_expression_t* expr;

    expr = MetaCLPP_ParseExpressionFromString(&LPP, "12 - 16 - 99");
    assert(!strcmp(MetaCPrinter_PrintExpression(&printer, expr), "((12 - 16) - 99)"));

    expr = MetaCLPP_ParseExpressionFromString(&LPP, "2 * 12 + 10");
    assert(!strcmp(MetaCPrinter_PrintExpression(&printer, expr), "((2 * 12) + 10)"));

    expr = MetaCLPP_ParseExpressionFromString(&LPP, "2 + 10 * 2");
    assert(!strcmp(MetaCPrinter_PrintExpression(&printer, expr), "(2 + (10 * 2))"));

    expr = MetaCLPP_ParseExpressionFromString(&LPP, "a = b(c)");
    assert(!strcmp(MetaCPrinter_PrintExpression(&printer, expr), "(a = b(c))"));

    expr = MetaCLPP_ParseExpressionFromString(&LPP, "((x + ((((a + b))))) + d)");
    assert(!strcmp(MetaCPrinter_PrintExpression(&printer, expr), "((x + ((((a + b))))) + d)"));
}

void TestParseDeclaration(void)
{
    metac_printer_t printer;
    metac_lpp_t LPP;
    MetaCLPP_Init(&LPP);

    MetaCPrinter_Init(&printer,
        &LPP.Parser.IdentifierTable,
        &LPP.Parser.StringTable
    );
    metac_expression_t* expr;

    metac_declaration_t* decl = MetaCLPP_ParseDeclarationFromString(&LPP, "int f(double x);");
    // assert(!strcmp(PrintDeclaration(&g_lineParser, decl, 0, 0), "int f(double x);"));
}

int main(int argc, char* argv[])
{
    TestParseExprssion();
}

#  endif
#endif // _METAC_PARSER_C_
