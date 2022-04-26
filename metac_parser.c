#ifndef _METAC_PARSER_C_
#define _METAC_PARSER_C_

#ifndef ACCEL
#  error "You must compile the parser with ACCEL set"
#  error "Known values are ACCEL_TABLE and ACCEL_TREE"
#else
#  if ACCEL == ACCEL_TABLE
#    include "metac_identifier_table.c"
#  elif ACCEL == ACCEL_TREE
#    include "metac_identifier_tree.c"
#  else
#    error "Unknow ACCEL value "
#    define DO_NOT_COMPILE
#  endif
#endif

#ifndef DO_NOT_COMPILE

#include "metac_lexer.c"
#include "metac_parser.h"

#include <string.h>
#include <stdlib.h>
#include <assert.h>

#include "3rd_party/tracy/TracyC.h"

const void* _emptyPointer = (const void*)0x1;
#define emptyPointer ((void*)_emptyPointer)

static noinline void _newMemRealloc(void** memP, uint32_t* capacity, const uint32_t elementSize);
const char* MetaCExpressionKind_toChars(metac_expression_kind_t type);
#define ARRAY_SIZE(A) \
     ((unsigned int)(sizeof((A)) / sizeof((A)[0])))

void MetaCParser_Init(metac_parser_t* self)
{
    self->CurrentTokenIndex = 0;
#ifdef IDENTIFIER_TABLE
    IdentifierTableInit(&self->IdentifierTable);
#endif
#ifdef IDENTIFIER_TREE
    IdentifierTreeInit(&self->IdentifierTree);
#endif
    self->Defines = self->inlineDefines;
    self->DefineCount = 0;
    self->DefineCapacity = ARRAY_SIZE(self->inlineDefines);
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
                              identifierKey, identifierString,
                              LENGTH_FROM_IDENTIFIER_KEY(identifierKey));
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
                                  stringKey, string,
                                  LENGTH_FROM_STRING_KEY(stringKey));
}

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

metac_token_t* MetaCParser_NextToken(metac_parser_t* self)
{
#define define_key 0x6a491b
#define ifdef_key 0x581ce0
#define endif_key 0x506843

#define NextToken() \
    ((self->CurrentTokenIndex < self->Lexer->TokenSize) ? \
    self->Lexer->Tokens + self->CurrentTokenIndex++ : 0)

#define PeekMatch(TOKEN_TYPE) \
    ( ((result = NextToken()), (result && result->TokenType == TOKEN_TYPE)) ? \
    ( result ) : ( self->CurrentTokenIndex--, (metac_token_t*)0) )

    metac_token_t* result = 0;
    assert(self->Lexer->TokenSize);

    result = NextToken();
    if(result)
    {
        if (result->TokenType == tok_identifier)
        {
            metac_define_t* matchingDefine = 0;

            const uint32_t idKey = result->IdentifierKey;
            for(int defineIdx = 0;
                defineIdx < self->DefineCount;
                defineIdx++)
            {
                metac_define_t* define = self->Defines + defineIdx;

                if (define->IdentifierKey == idKey)
                {
                    const char* defineString =
                        IDENTIFIER_PTR(MEMBER_SUFFIX(&self->Identifier), *define);
                    const char* IdString =
                        IDENTIFIER_PTR(MEMBER_SUFFIX(&self->Lexer->Identifier), *result);

                    if (!memcmp(IdString, defineString,
                        LENGTH_FROM_IDENTIFIER_KEY(idKey)))
                    {
                        matchingDefine = define;
                        break;
                    }
                }
            }

            if (matchingDefine)
            {
                const char* defineName =
                        IDENTIFIER_PTR(MEMBER_SUFFIX(&self->Identifier), *matchingDefine);
                // result = tok_plus;
                printf("Define %s matched we should do something\n", defineName);
            }
        }
        if (IsMacro(self, result))
        {
            HandleMacro(self, result);
        }
        else if(result && result->TokenType == tok_hash)
        {
            result = NextToken();
            if (!result || result->TokenType != tok_identifier)
            {
LexpectedIdent:
                ParseError(self->LexerState, "Expected Identifier after #");
                return result;
            }
            if (result->TokenType == tok_identifier)
            {
                switch(result->IdentifierKey)
                {
                case define_key :
                    result = NextToken();

                    if(result->TokenType == tok_identifier)
                    {
                        metac_token_t* define_name = result;
                        uint32_t define_idx = self->CurrentTokenIndex;
                        result = NextToken();
                        int nParameters = 0;
                        const bool isMacro = (result->TokenType == tok_lParen);
                        if (isMacro)
                        {
                            PeekMatch(tok_lParen);
                            for (;;)
                            {
                                if (PeekMatch(tok_dotdotdot))
                                {
                                    nParameters |= (1 << 31);
                                    if (!PeekMatch(tok_rParen))
                                    {
                                        ParseError(self->LexerState, "')' expected after ...");
                                        return result;
                                    }
                                    break;
                                }
                                if (PeekMatch(tok_rParen))
                                    break;

                                if (!PeekMatch(tok_identifier))
                                    goto LexpectedIdent;
                                if (!PeekMatch(tok_comma))
                                {
                                    ParseErrorF(self->LexerState,
                                        "Expected ',' after define parameter %s",
                                        IDENTIFIER_PTR(MEMBER_SUFFIX(&self->Identifier),
                                                       *result));
                                    return result;
                                }
                                nParameters++;
                            }
                        }
                        AddDefine(self, define_name, nParameters);
                        result = NextToken();
                    }
                    break;
                default:
                    ParseErrorF(self->LexerState, "Expected define ifdef endif or got: %s",
                        IDENTIFIER_PTR(MEMBER_SUFFIX(&self->Identifier), *result));
                }
            }
        }
    }
    else
    {
        // TODO Error
    }

    return result;
}

metac_token_t* MetaCParser_PeekToken(metac_parser_t* self, int32_t p)
{
    metac_token_t* result = 0;
    assert(self->Lexer->TokenSize);

    if (cast(uint32_t)(self->CurrentTokenIndex + (p - 1)) < self->Lexer->TokenSize)
    {
        result = self->Lexer->Tokens + self->CurrentTokenIndex + (p - 1);
        if (IsMacro(self, result))
        {
            HandleMacro(self, result);
        }
        else if(result && result->TokenType == tok_hash)
        {
            HandlePreprocessor(self);
        }
    }
    else
    {
        // TODO Error
    }

    return result;
}

static inline uint32_t MetaCParser_HowMuchLookahead(metac_parser_t* self)
{
    return (self->Lexer->TokenSize - self->CurrentTokenIndex);
}

#define MetaCParser_Match(SELF, TYPE) \
    (MetaCParser_Match_((SELF), (TYPE), __FILE__, __LINE__))

metac_token_t* MetaCParser_Match_(metac_parser_t* self, metac_token_enum_t type,
                                 const char* filename, uint32_t lineNumber)
{
    metac_token_t* token = MetaCParser_NextToken(self);
    metac_token_enum_t got = (token ? token->TokenType : tok_eof);
    if (got != type)
    {
        metac_location_t loc = self->Lexer->LocationStorage.Locations[token->LocationId - 4];

        printf("[%s:%u] Expected: %s -- Got: %s {line: %u: col: %u}\n",
            filename, lineNumber,
            MetaCTokenEnum_toChars(type), MetaCTokenEnum_toChars(got),
            loc.StartLine, loc.StartColumn);
    }
    return token;
}

static uint32_t _newExp_size = 0;
static uint32_t _newExp_capacity = 0;
static metac_expression_t* _newExp_mem = 0;

static uint32_t _newStmt_size = 0;
static uint32_t _newStmt_capacity = 0;
static metac_statement_t* _newStmt_mem = 0;

static uint32_t _newDecl_size = 0;
static uint32_t _newDecl_capacity = 0;
static metac_declaration_t* _newDecl_mem = 0;

static uint32_t _nodeCounter = 1;

#ifndef ALIGN4
#  define ALIGN4(N) \
      (((N) + 3) & ~3)
#endif

metac_expression_kind_t BinExpTypeFromTokenType(metac_token_enum_t tokenType)
{
    metac_expression_kind_t result = exp_invalid;
    if (((tokenType >= FIRST_BINARY_TOKEN(TOK_SELF)) | (tokenType <= LAST_BINARY_TOKEN(TOK_SELF))))
    {
        return cast(metac_expression_kind_t)(cast(int)tokenType -
                (cast(int)FIRST_BINARY_TOKEN(TOK_SELF) -
                 cast(int)FIRST_BINARY_EXP(TOK_SELF)));
    }

    return exp_invalid;
}

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
        case exp_rem_ass   : return "%=";
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

static noinline void _newMemRealloc(void** memP, uint32_t* capacityP, const uint32_t elementSize)
{
    uint32_t capacity;
    if (!*memP)
    {
        capacity = cast(int)(1024 / 1.6f);
    }
    else
    {
        capacity = *capacityP;
    }

    {
        capacity = ALIGN4(cast(uint32_t) ((capacity - 1) * 1.6f));
        *memP = realloc(*memP, ((capacity) * elementSize));
    }

    *capacityP = capacity;
}

#ifndef ATOMIC
#define INC(v) \
    (v++)
#else
#define INC(v)
    (__builtin_atomic_fetch_add(&v, __ATOMIC_RELEASE))
#endif

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
        result = _newExp_mem + INC(_newExp_size);
        result->Kind = kind;
        result->Serial = INC(_nodeCounter);
    }

    return result;
}

#define AllocNewDeclaration(KIND, RESULT_PTR) \
    (KIND ## _t*) AllocNewDeclaration_(KIND, sizeof(KIND ##_t), ((void**)(RESULT_PTR)), __LINE__)

metac_declaration_t* AllocNewDeclaration_(metac_declaration_kind_t kind, size_t nodeSize, void** result_ptr, uint32_t line)
{
    metac_declaration_t* result = 0;

    if (_newDecl_capacity <= _newDecl_size)
    {
        _newMemRealloc(
            (void**)&_newDecl_mem,
            &_newDecl_capacity,
            sizeof(metac_declaration_t)
        );
    }

    {
        (*result_ptr) = result = _newDecl_mem + INC(_newDecl_size);
        result->DeclKind = kind;
        result->Serial = INC(_nodeCounter);
        memset(&result->Serial, 0, nodeSize - offsetof(metac_declaration_t, Serial));
        result->AllocInLine = line;
    }

    return result;
}

#define ASSERT_VALID_DECL(DECL_P) \
            assert(((metac_declaration_t*)(DECL_P)) >= _newDecl_mem  \
                && ((metac_declaration_t*)(DECL_P)) <= (_newDecl_mem + _newDecl_size))

#define AllocNewStatement(KIND, RESULT_PTR) \
    (KIND ## _t*) AllocNewStatement_(KIND, sizeof(KIND ##_t), ((void**)(RESULT_PTR)))

metac_statement_t* AllocNewStatement_(metac_statement_kind_t kind, size_t nodeSize, void** result_ptr)
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
        (*result_ptr) = result = _newStmt_mem + INC(_newStmt_size);
        result->StmtKind = kind;
        result->Serial = INC(_nodeCounter);
        result->Next = _emptyPointer;
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

bool MetaCParser_PeekMatch(metac_parser_t* self, metac_token_enum_t expectedType, bool optional)
{
    metac_token_t* peekToken =
        MetaCParser_PeekToken(self, 1);
    bool result = true;

    if (!peekToken || peekToken->TokenType != expectedType)
    {
        result = false;
        if (!optional)
        {
            ParseErrorF(self->LexerState, "expected %s but got %s",
                MetaCTokenEnum_toChars(expectedType),
                MetaCTokenEnum_toChars(peekToken ? peekToken->TokenType : tok_eof)
            );
        }
    }

    return result;
}

bool IsPostfixOperator(metac_token_enum_t t)
{
    return (t == tok_plusplus || t == tok_minusminus);
}

bool IsBinaryOperator(metac_token_enum_t t)
{
    return (t >= FIRST_BINARY_TOKEN(TOK_SELF) && t <= LAST_BINARY_TOKEN(TOK_SELF));
}

uint32_t Mix(uint32_t a, uint32_t b)
{
  return (a ^ b) + 0x9e3779b9 + (a << 6) + (a >> 2);

}

uint32_t OpToPrecedence(metac_expression_kind_t exp)
{
    if (exp == exp_comma)
    {
        return 1;
    }
    else if (exp >= exp_assign && exp < exp_spaceship)
    {
        return 2;
    }
    //else if (exp_ternary)
    //{
    //  return 3;
    //}
    else if (exp == exp_andand)
    {
        return 4;
    }
    else if (exp == exp_oror)
    {
        return 6;
    }
    else if (exp == exp_or)
    {
        return 7;
    }
    else if (exp == exp_xor)
    {
        return 8;
    }
    else if (exp == exp_eq || exp == exp_neq)
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
    else if (exp == exp_call || exp == exp_index || exp == exp_dot
          || exp == exp_arrow || exp == exp_compl)
    {
        return 16;
    }
    else if (exp == exp_paren
          || exp == exp_signed_integer
          || exp == exp_string
          || exp == exp_identifier
          || exp == exp_char)
    {
        return 17;
    }
    assert(0);
    return 0;
}

bool IsPrimaryExpressionToken(metac_token_enum_t tokenType)
{
    switch(tokenType)
    {
    case tok_lParen:
    case tok_unsignedNumber:
    case tok_stringLiteral:
    case tok_char:
    case tok_identifier:
        return true;
    default:
        return false;
    }
}


metac_expression_t* MetaCParser_ParsePrimaryExpression(metac_parser_t* self)
{
    metac_expression_t* result = 0;

    metac_token_t* currentToken = MetaCParser_NextToken(self);
    metac_token_enum_t tokenType =
        (currentToken ? currentToken->TokenType : tok_eof);


    if (tokenType == tok_unsignedNumber)
    {
        result = AllocNewExpression(exp_signed_integer);
        result->ValueI64 = currentToken->ValueU64;
        result->Hash = crc32c(~0, &result->ValueU64, sizeof(result->ValueU64));
        //PushOperand(result);
    }
    else if (tokenType == tok_stringLiteral)
    {
        // result = GetOrAddStringLiteral(_string_table, currentToken);

        result = AllocNewExpression(exp_string);
        result->StringPtr = RegisterString(self, currentToken);
        result->StringKey = currentToken->StringKey;
        result->Hash = currentToken->StringKey;
        //PushOperand(result);
    }
    else if (tokenType == tok_char)
    {
        result = AllocNewExpression(exp_char);
        const uint32_t length = currentToken->charLength;
        const char* chars = currentToken->chars;
        const uint32_t hash = crc32c(~0, chars, length);

        (*(uint64_t*)result->Chars) =
            (*(uint64_t*) chars);
        result->CharKey = CHAR_KEY(hash, length);
    }
    else if (tokenType == tok_identifier)
    {
        result = AllocNewExpression(exp_identifier);

        result->IdentifierPtr = RegisterIdentifier(self, currentToken);
        result->IdentifierKey = currentToken->IdentifierKey;
        result->Hash = currentToken->IdentifierKey;
        //PushOperand(result);
    }
    else if (tokenType == tok_lParen)
    {
        result = AllocNewExpression(exp_paren);
        {
            if (!MetaCParser_PeekMatch(self, tok_rParen, 1))
                result->E1 = MetaCParser_ParseExpression(self, 0);
        }
        //PushOperator(exp_paren);
        result->Hash = Mix(crc32c(~0, "()", 2), result->E1->Hash);
        //PushOperand(result);
        MetaCParser_Match(self, tok_rParen);
        //PopOperator(exp_paren);
    }
    else
    {
        assert(0); // Not a primary Expression;
    }

    return result;
}

metac_expression_t* MetaCParser_ParsePostfixExpression(metac_parser_t* self,
                                                       metac_expression_t* left)
{
    metac_expression_t* result = 0;

    metac_token_t* peek = MetaCParser_PeekToken(self, 1);

    metac_token_enum_t peekTokenType = peek->TokenType;

    if (peekTokenType == tok_plusplus)
    {
        MetaCParser_Match(self, peekTokenType);
        metac_expression_t* E1 = left;
        result = AllocNewExpression(exp_post_increment);
        result->E1 = E1;
    }
    else if (peekTokenType == tok_minusminus)
    {
        MetaCParser_Match(self, peekTokenType);

        metac_expression_t* E1 = left;
        result = AllocNewExpression(exp_post_decrement);
        result->E1 = E1;
    }
    else
        assert(!"Unknown postfix expression, this function should never have been called");

    return result;
}

metac_expression_t* MetaCParser_ParseUnaryExpression(metac_parser_t* self)
{
    metac_expression_t* result = 0;

    metac_token_t* currentToken = MetaCParser_PeekToken(self, 1);
    metac_token_enum_t tokenType =
        (currentToken ? currentToken->TokenType : tok_eof);


    if (tokenType == tok_kw_eject)
    {
        MetaCParser_Match(self, tok_kw_eject);
        result = AllocNewExpression(exp_eject);
        //PushOperator(exp_eject);
        result->E1 = MetaCParser_ParseExpression(self, 0);
        result->Hash = Mix(
            crc32c(~0, "eject", sizeof("eject") - 1),
            result->E1->Hash
        );
        //PushOperand(result);
        //PopOperator(exp_eject);
    }
    else if (tokenType == tok_kw_inject)
    {
        MetaCParser_Match(self, tok_kw_inject);
        result = AllocNewExpression(exp_inject);
        //PushOperator(exp_inject);
        result->E1 = MetaCParser_ParseExpression(self, 0);
        result->Hash = Mix(
            crc32c(~0, "inject", sizeof("inject") - 1),
            result->E1->Hash
        );
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
            ParseError(self->LexerState, "Expected typeof to be followed by '('");
        }

        metac_expression_t* parenExp = MetaCParser_ParseExpression(self, 0);
        //PopOperator(exp_typeof);
        assert(parenExp->Kind == exp_paren);
        result->E1 = parenExp->E1;
    }
    else if (tokenType == tok_kw_assert)
    {
        MetaCParser_Match(self, tok_kw_assert);
        result = AllocNewExpression(exp_assert);
        //PushOperator(exp_assert);
        metac_token_t* nextToken = MetaCParser_PeekToken(self, 1);
        if (!nextToken || nextToken->TokenType != tok_lParen)
        {
            ParseError(self->LexerState, "Expected assert to be followed by '('");
        }
        metac_expression_t* parenExp = MetaCParser_ParseExpression(self, 0);
        //PopOperator(exp_assert);
        assert(parenExp->Kind == exp_paren);
        result->E1 = parenExp->E1;
    }
    else if (tokenType == tok_minus)
    {
        MetaCParser_Match(self, tok_minus);
        result = AllocNewExpression(exp_umin);
        //PushOperator(exp_addr);
        result->E1 = MetaCParser_ParseExpression(self, 0);
        result->Hash = Mix(
            crc32c(~0, "-", 1),
            result->E1->Hash
        );
    }
    else if (tokenType == tok_and)
    {
        MetaCParser_Match(self, tok_and);
        result = AllocNewExpression(exp_addr);
        //PushOperator(exp_addr);
        result->E1 = MetaCParser_ParseExpression(self, 0);
        result->Hash = Mix(
            crc32c(~0, "&", sizeof("&") - 1),
            result->E1->Hash
        );
        //PushOperand(result);
        //PopOperator(exp_addr);
    }
    else if (tokenType == tok_star)
    {
        MetaCParser_Match(self, tok_star);
        result = AllocNewExpression(exp_ptr);
        //PushOperator(exp_ptr);
        result->E1 = MetaCParser_ParseExpression(self, 0);
        result->Hash = Mix(
            crc32c(~0, "*", sizeof("*") - 1),
            result->E1->Hash
        );
        //PushOperand(result);
    }
    else if (tokenType == tok_bang)
    {
        MetaCParser_Match(self, tok_bang);
        result = AllocNewExpression(exp_not);
        result->E1 = MetaCParser_ParseExpression(self, 0);
        result->Hash = Mix(
            crc32c(~0, "!", 1),
            result->E1->Hash
        );
    }
    else if (tokenType == tok_cat)
    {
        MetaCParser_Match(self, tok_cat);
        result = AllocNewExpression(exp_compl);
        result->E1 = MetaCParser_ParseExpression(self, 0);
        result->Hash = Mix(
            crc32c(~0, "~", 1),
            result->E1->Hash
        );
    }
    else if (IsPrimaryExpressionToken(tokenType))
    {
        result = MetaCParser_ParsePrimaryExpression(self);
    }
    else
    {
        printf("Unexpected Token: %s\n", MetaCTokenEnum_toChars(tokenType));
        assert(0);
    }

    metac_token_t* peek_post = MetaCParser_PeekToken(self, 1);
    metac_token_enum_t postTokenType =
        (peek_post ? peek_post->TokenType : tok_invalid);

    if (IsPostfixOperator(postTokenType))
    {
        result = MetaCParser_ParsePostfixExpression(self, result);
    }

    return result;
}

metac_expression_t* MetaCParser_ParseBinaryExpression(metac_parser_t* self,
                                                      metac_expression_t* left,
                                                      uint32_t min_prec)
{
    metac_expression_t* result = 0;

    metac_token_t* peekToken = MetaCParser_PeekToken(self, 1);
    metac_token_enum_t peekTokenType = (peekToken ? peekToken->TokenType : tok_eof);

    metac_expression_kind_t exp_left;
    metac_expression_kind_t exp_right;

    if (IsBinaryOperator(peekTokenType))
    {
        while(IsBinaryOperator(peekTokenType)
           && OpToPrecedence(BinExpTypeFromTokenType(peekTokenType)) >= min_prec)
        {
            exp_right = BinExpTypeFromTokenType(peekTokenType);
            uint32_t opPrecedence = OpToPrecedence(exp_right);
            MetaCParser_Match(self, peekTokenType);
            metac_expression_t* rhs = MetaCParser_ParseUnaryExpression(self);
            peekToken = MetaCParser_PeekToken(self, 1);
            peekTokenType = (peekToken ? peekToken->TokenType : tok_eof);
            while(IsBinaryOperator(peekTokenType)
               && opPrecedence <
                  OpToPrecedence(BinExpTypeFromTokenType(peekTokenType)))
            {
                rhs = MetaCParser_ParseBinaryExpression(self, rhs, opPrecedence + 0);
                peekToken = MetaCParser_PeekToken(self, 1);
                peekTokenType = (peekToken ? peekToken->TokenType : tok_eof);
            }
            result = AllocNewExpression(exp_right);
            result->E1 = left;
            result->E2 = rhs;
            left = result;
        }
    }
    else if (peekTokenType == tok_lBracket)
    {
        MetaCParser_Match(self, tok_lBracket);

        metac_expression_t* E1 = left;
        result = AllocNewExpression(exp_index);
        result->E1 = E1;
        result->E2 = MetaCParser_ParseExpression(self, 0);

        MetaCParser_Match(self, tok_rBracket);
    }
    else if (peekTokenType == tok_lParen)
    {
        MetaCParser_Match(self, tok_lParen);
        metac_token_t* peekToken = MetaCParser_PeekToken(self, 2);
        metac_expression_t* E1 = left;
        assert(peekToken);
        metac_expression_t* E2 = 0;

        uint32_t nArguments = 0;
        uint32_t lookaheadLeft = MetaCParser_HowMuchLookahead(self);
        uint32_t currentLookahead = 1;

        //if (peekTokenType->)

        while (peekToken->TokenType != tok_rParen)
        {
            nArguments++;
            E2 = MetaCParser_ParseExpression(self, 0);
        }
        result = AllocNewExpression(exp_call);
        result->E1 = E1;
        result->E2 = E2;

        //PopOperator(exp_call);
    }
    else
    {
        assert(!"Unexpected Token");
    }

    return result;
}

bool IsBinaryExp(metac_expression_kind_t kind)
{
    return ((kind >= FIRST_BINARY_EXP(TOK_SELF)) && (kind <= LAST_BINARY_EXP(TOK_SELF)));
}

bool IsBinaryAssignExp(metac_expression_kind_t kind)
{
    IsBinaryExp(kind - (exp_add_ass - exp_add));
}


static inline bool IsTypeToken(metac_token_enum_t tokenType)
{
    bool  result =
           (   tokenType == tok_kw_const
            || (tokenType >= tok_kw_auto && tokenType <= tok_kw_double)
            || tokenType == tok_kw_unsigned
            || tokenType == tok_star
            || tokenType == tok_kw_struct
            || tokenType == tok_kw_enum
            || tokenType == tok_kw_union
            || tokenType == tok_identifier );
    return result;
}


static bool CouldBeCast(metac_parser_t* self, metac_token_enum_t tok)
{
    if (tok != tok_lParen)
        return false;

    // first we see if the next could be a type token
    // because if it isn't then we are certainly not as cast
    metac_token_t* peek2 = MetaCParser_PeekToken(self, 2);
    metac_token_enum_t tokenType = peek2 ? peek2->TokenType : tok_invalid;

    if (!IsTypeToken(tokenType))
        return false;

    return false;
}

metac_expression_t* MetaCParser_ParseExpression(metac_parser_t* self,
                                                metac_expression_t* prev)
{
    metac_expression_t* result = 0;
    metac_token_t* currentToken = MetaCParser_PeekToken(self, 1);
    metac_token_enum_t tokenType =
        (currentToken ? currentToken->TokenType : tok_invalid);

    if (tokenType == tok_lParen && CouldBeCast(self, tokenType))
    {
        // Not implemented right now
        assert(0);
        // exp_cast_t* = AllocNewExpression(exp_cast, &result);

    }

    if (IsPrimaryExpressionToken(tokenType))
    {
        result = MetaCParser_ParsePrimaryExpression(self);
    }
    else  if (!prev /*|| prec_left > prec_right*/)
    {
        result = MetaCParser_ParseUnaryExpression(self);
    }
    else
    {
        result = MetaCParser_ParseBinaryExpression(self, prev, 0);
    }


//    printf("TokenType: %s\n", MetaCTokenEnum_toChars(tokenType));

    metac_token_t* peekNext = MetaCParser_PeekToken(self, 1);
    if (peekNext)
    {
        tokenType = peekNext->TokenType;
        if (IsBinaryOperator(tokenType))
        {
            uint32_t prec = OpToPrecedence(BinExpTypeFromTokenType(tokenType));
            result = MetaCParser_ParseBinaryExpression(self, result, prec);
        }
        else if (IsPostfixOperator(tokenType))
        {
            result = MetaCParser_ParsePostfixExpression(self, result);
        }
        else if (peekNext->TokenType == tok_lParen)
        {
            result = MetaCParser_ParseBinaryExpression(self, result, OpToPrecedence(exp_call));
        }
        else if (tokenType == tok_lBracket)
        {
            result = MetaCParser_ParseBinaryExpression(self, result, OpToPrecedence(exp_index));
        }
        else if (tokenType == tok_rBracket || tokenType == tok_rParen)
        {
            // there's nothing to see here crray on
        }
        //else assert(!"Stray Input");
    }


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

#define U32(VAR) \
	(*(uint32_t*)(&VAR))

decl_type_t* MetaCParser_ParseTypeDeclaration(metac_parser_t* self, metac_declaration_t* parent, metac_declaration_t* prev)
{
    decl_type_t* result = 0;

    decl_type_t* type = AllocNewDeclaration(decl_type, &result);
    metac_type_modifiers typeModifiers = typemod_none;
    metac_token_t* currentToken = 0;

LnextToken:
    currentToken = MetaCParser_NextToken(self);
    metac_token_enum_t tokenType =
        (currentToken ? currentToken->TokenType : tok_invalid);

    bool postType = false;


    while(IsTypeToken(tokenType))
    {
        if (tokenType == tok_identifier)
        {
            type->TypeKind = type_identifier;
            type->TypeIdentifier = RegisterIdentifier(self, currentToken);
            U32(type->TypeModifiers) |= typeModifiers;
            break;
        }
        if (tokenType == tok_kw_const)
        {
            U32(typeModifiers) |= typemod_const;
            goto LnextToken;
        }
        else if (tokenType >= tok_kw_auto && tokenType <= tok_kw_double)
        {
            type->TypeKind = (metac_type_kind_t)(type_auto + (tokenType - tok_kw_auto));
            U32(type->TypeModifiers) |= typeModifiers;
            break;
        }
        else if (tokenType == tok_kw_unsigned)
        {
            U32(typeModifiers) |= typemod_unsigned;
            goto LnextToken;
        }
        else if (tokenType == tok_star)
        {
            assert(0); // this is not supposed to happen
        }
        else if (tokenType == tok_kw_struct)
        {
            bool isPredeclated = true;

            decl_type_struct_t* struct_ = AllocNewDeclaration(decl_type_struct, &result);
            type = (decl_type_t*)struct_;
            struct_->TypeKind = type_struct;

            if (MetaCParser_PeekMatch(self, tok_identifier, 1))
            {
                metac_token_t* structName = MetaCParser_NextToken(self);
                struct_->Identifier = RegisterIdentifier(self, structName);
            }
            else
            {
                struct_->Identifier = empty_identifier;
            }


            if (MetaCParser_PeekMatch(self, tok_lBrace, 1))
            {
                MetaCParser_Match(self, tok_lBrace);
                decl_field_t **nextMemberPtr = &struct_->Fields;

                isPredeclated = false;
                while(!MetaCParser_PeekMatch(self, tok_rBrace, 1))
                {
                    decl_field_t* field =
                        AllocNewDeclaration(decl_field, (metac_declaration_t**)nextMemberPtr);
                    field->Next = 0;
                    field->Field.Type = MetaCParser_ParseTypeDeclaration(self, (metac_declaration_t*)result, 0);
                    assert(IsDeclType((metac_declaration_t*)field->Field.Type));

                    metac_token_t* memberName = MetaCParser_Match(self, tok_identifier);
                    if (!field->Field.Type || !memberName || memberName->TokenType != tok_identifier)
                        return ErrorTypeDeclaration();

                    field->Field.Identifier = RegisterIdentifier(self, memberName);

                    MetaCParser_Match(self, tok_semicolon);
                    nextMemberPtr = &field->Next;
                    struct_->FieldCount++;
                }
                MetaCParser_Match(self, tok_rBrace);
            }
            else
            {
                printf("We just have a decl\n");
            }
            break;
        }
        else if (tokenType == tok_kw_enum)
        {
        }
    }

    currentToken = MetaCParser_PeekToken(self, 1);
    tokenType =
        (currentToken ? currentToken->TokenType : tok_invalid);

    while(tokenType == tok_star)
    {
        MetaCParser_Match(self, tok_star);
        decl_type_t* elementType = result;
        decl_type_ptr_t* ptr = AllocNewDeclaration(decl_type_ptr, &result);
        ptr->ElementType = elementType;

        currentToken = MetaCParser_PeekToken(self, 1);
        tokenType =
            (currentToken ? currentToken->TokenType : tok_invalid);
    }

    return result;
}
static decl_type_array_t* ParseArraySuffix(metac_parser_t* self, decl_type_t* type);
static stmt_block_t* MetaCParser_ParseBlockStatement(metac_parser_t* self,
                                                     metac_statement_t* parent,
                                                     metac_statement_t* prev);

metac_declaration_t* MetaCParser_ParseDeclaration(metac_parser_t* self, metac_declaration_t* parent)
{
    metac_token_t* currentToken = MetaCParser_PeekToken(self, 1);
    metac_token_enum_t tokenType =
        (currentToken ? currentToken->TokenType : tok_invalid);

	metac_declaration_t* result = 0;
    bool isStatic = false;

    decl_type_t* type = 0;

    if (MetaCParser_PeekMatch(self, tok_kw_static, true))
    {
        isStatic = true;
        MetaCParser_Match(self, tok_kw_static);
        currentToken = MetaCParser_PeekToken(self, 1);
        tokenType = currentToken ? currentToken->TokenType : tok_eof;
    }

    if (IsTypeToken(tokenType))
    {
         type = MetaCParser_ParseTypeDeclaration(self, parent, 0);
    }

    if (tokenType == tok_kw_typedef)
    {
        MetaCParser_Match(self, tok_kw_typedef);
        currentToken = MetaCParser_PeekToken(self, 1);
            tokenType =
        (currentToken ? currentToken->TokenType : tok_invalid);
        decl_typedef_t* typdef = AllocNewDeclaration(decl_typedef, &result);

        typdef->Type = MetaCParser_ParseTypeDeclaration(self, (metac_declaration_t*) typdef, 0);
        metac_token_t* name = MetaCParser_Match(self, tok_identifier);
        if (!name || name->TokenType != tok_identifier)
        {
            printf("Expecting an identifier to follow the type definition of a typedef\n");
            return ErrorDeclaration();
        }
        typdef->Identifier = RegisterIdentifier(self, name);
        goto LendDecl;
    }

    if (type)
    {
        if (MetaCParser_PeekMatch(self, tok_identifier, 1))
        {
            metac_token_t* id = MetaCParser_Match(self, tok_identifier);
            assert(id);
            metac_identifier_ptr_t identifier = RegisterIdentifier(self, id);

            // id paren ... it's a function :D
            if (MetaCParser_PeekMatch(self, tok_lParen, true))
            {
                MetaCParser_Match(self, tok_lParen);
                decl_function_t* funcDecl = AllocNewDeclaration(decl_function, &result);
                printf("We've got a function yay!\n");
                funcDecl->ReturnType = type;
                funcDecl->Identifier = identifier;
				funcDecl->Parameters = (decl_parameter_t*) _emptyPointer;

                decl_parameter_t** nextParam = &funcDecl->Parameters;

                while (!MetaCParser_PeekMatch(self, tok_rParen, true))
                {
                    assert((*nextParam) == emptyPointer);

                    decl_parameter_t* param;
                    AllocNewDeclaration(decl_parameter, &param);
                    (*nextParam) = param;

                    param->Type = MetaCParser_ParseTypeDeclaration(self, result, 0);

                    param->Identifier = empty_identifier;
                    if (MetaCParser_PeekMatch(self, tok_identifier, 1))
                    {
                        metac_token_t* nameToken = MetaCParser_Match(self, tok_identifier);
                        param->Identifier = RegisterIdentifier(self, nameToken);

                            // follow parameter
                        while(MetaCParser_PeekMatch(self, tok_lBracket, true))
                        {
                            param->Type = (decl_type_t*)ParseArraySuffix(self, param->Type);
                        }
                    }

                    nextParam = &param->Next;
                    (*nextParam) = (decl_parameter_t*) _emptyPointer;

                    if (MetaCParser_PeekMatch(self, tok_comma, true))
                    {
                        MetaCParser_Match(self, tok_comma);
                    }
                    else
                    {
                        assert(MetaCParser_PeekMatch(self, tok_rParen, true));
                    }
                }
                MetaCParser_Match(self, tok_rParen);
                funcDecl->FunctionBody = (stmt_block_t*) _emptyPointer;

                if (MetaCParser_PeekMatch(self, tok_lBrace, true))
                {
                    funcDecl->FunctionBody = MetaCParser_ParseBlockStatement(self, 0, 0);
                }
            }
            else
            {
                decl_variable_t* varDecl = AllocNewDeclaration(decl_variable, &result);
//            varDecl.LocationIdx =
//                MetaCLocationStorage_StartLoc(&parser.locationStorage,
//                    MetaCLocationStorage_StartLine(&parser.lexer.locationStorage, type.LocationIdx));

                varDecl->Type = type;
                varDecl->Identifier = identifier;
                while (MetaCParser_PeekMatch(self, tok_lBracket, true))
                {
                    varDecl->Type = (decl_type_t*)ParseArraySuffix(self, varDecl->Type);
                }
            }
        }
    }
    else
    {
        ParseError(self->LexerState, "A declaration is expected to start with  a type");
    }

LendDecl:
    // eat a semicolon if there is one this is more a repl kindof thing
    if (MetaCParser_PeekMatch(self, tok_semicolon, true))
        MetaCParser_Match(self, tok_semicolon);

	return result;
}

static decl_type_array_t* ParseArraySuffix(metac_parser_t* self, decl_type_t* type)
{
    decl_type_array_t* arrayType = 0;
    if (MetaCParser_PeekMatch(self, tok_lBracket, true))
    {
        MetaCParser_Match(self, tok_lBracket);
        decl_type_t* paramType = type;
        arrayType =
            AllocNewDeclaration(decl_type_array, &arrayType);
        //TODO ErrorMessage array must have numeric dimension
        arrayType->ElementType = type;
        arrayType->Dim = MetaCParser_ParseExpression(self, 0);
        MetaCParser_Match(self, tok_rBracket);
    }
    assert(arrayType);
    return arrayType;
}


#define ErrorStatement() \
    (metac_statement_t*)0

static metac_statement_t* MetaCParser_ParseStatement(metac_parser_t* self,
                                                     metac_statement_t* parent,
                                                     metac_statement_t* prev)
{
    metac_statement_t* result = 0;

    metac_token_t* currentToken = MetaCParser_PeekToken(self, 1);
    metac_token_enum_t tokenType =
        (currentToken ? currentToken->TokenType : tok_invalid);

    if (tokenType == tok_invalid)
    {
        return ErrorStatement();
    }
    else if (tokenType == tok_kw_if)
    {
        stmt_if_t* if_stmt = AllocNewStatement(stmt_if, &result);
        MetaCParser_Match(self, tok_kw_if);
        if (!MetaCParser_PeekMatch(self, tok_lParen, 0))
        {
            ParseError(self->LexerState, "execpected ( after if\n");
            return ErrorStatement();
        }
        metac_expression_t* condExpP =
            MetaCParser_ParseExpression(self, 0);
        assert(condExpP->Kind == exp_paren);
        if_stmt->IfCond = condExpP->E1;
        if_stmt->IfBody = MetaCParser_ParseStatement(self, (metac_statement_t*)result, 0);
        if (MetaCParser_PeekMatch(self, tok_kw_else, 1))
        {
            MetaCParser_Match(self, tok_kw_else);
            if_stmt->ElseBody = (metac_statement_t*)MetaCParser_ParseStatement(self, (metac_statement_t*)result, 0);
        }
        else
        {
            if_stmt->ElseBody = _emptyPointer;
        }
        goto LdoneWithStatement;
    }
    else if (tokenType == tok_kw_switch)
    {
        uint32_t switchHash =
            crc32c(~0, "switch", sizeof("switch") - 1);
        stmt_switch_t* switch_ = AllocNewStatement(stmt_switch, &result);

        MetaCParser_Match(self, tok_kw_switch);
        MetaCParser_Match(self, tok_lParen);
        metac_expression_t* cond =
            MetaCParser_ParseExpression(self, 0);
        switchHash = Mix(switchHash, cond->Hash);
        MetaCParser_Match(self, tok_rParen);
        if (!MetaCParser_PeekMatch(self, tok_lBrace, 0))
        {
            ParseError(self->LexerState, "parsing switch failed\n");
            return ErrorStatement();
        }

        metac_statement_t* caseBlock =
            (metac_statement_t*)MetaCParser_ParseBlockStatement(self, result, 0);
        switchHash = Mix(switchHash, caseBlock->Hash);
    }
    else if (tokenType == tok_identifier)
    {
        metac_token_t* peek2 = MetaCParser_PeekToken(self, 2);
        if (peek2 && peek2->TokenType == tok_colon)
        {
            metac_token_t* label = MetaCParser_Match(self, tok_identifier);
            MetaCParser_Match(self, tok_colon);

            stmt_label_t* result = AllocNewStatement(stmt_label, &result);

            result->Label = RegisterIdentifier(self, label);
        }
    }
    else if (tokenType == tok_kw_goto)
    {
        stmt_goto_t* result = AllocNewStatement(stmt_goto, &result);
        uint32_t gotoHash = crc32c(~0, "goto", sizeof("goto") - 1);

        MetaCParser_Match(self, tok_kw_goto);
        metac_token_t* label = MetaCParser_Match(self, tok_identifier);
        result->Label = RegisterIdentifier(self, label);
        result->Hash = Mix(gotoHash, label->IdentifierKey);
    }
    else if (tokenType == tok_kw_case)
    {
        uint32_t caseHash =
            crc32c(~0, "case", sizeof("case") - 1);
        stmt_case_t* case_ = AllocNewStatement(stmt_case, &result);

        MetaCParser_Match(self, tok_kw_case);
        metac_expression_t* caseExp =
            MetaCParser_ParseExpression(self, 0);
        MetaCParser_Match(self, tok_colon);

        caseHash = Mix(caseHash, caseExp->Hash);
        result->Next =
            MetaCParser_ParseStatement(self, result, 0);
        if (result->Next != _emptyPointer)
        {
            caseHash = Mix(caseHash, result->Next->Hash);
        }
        result->Hash = caseHash;
    }
    else if (tokenType == tok_kw_return)
    {
        stmt_return_t* return_ = AllocNewStatement(stmt_return, &result);
        MetaCParser_Match(self, tok_kw_return);
        return_->Expression = MetaCParser_ParseExpression(self, 0);
    }
    else if (tokenType == tok_kw_yield)
    {
        stmt_yield_t* yield_ = AllocNewStatement(stmt_yield, &result);
        MetaCParser_Match(self, tok_kw_yield);
        yield_->Expression = MetaCParser_ParseExpression(self, 0);
    }
    else if (tokenType == tok_lBrace)
    {
        result = (metac_statement_t*)MetaCParser_ParseBlockStatement(self, parent, prev);
    }
    else if (IsTypeToken(tokenType))
    {
        metac_token_t* peek2 = MetaCParser_PeekToken(self, 2);
        if (peek2 && IsTypeToken(peek2->TokenType))
        {
            metac_declaration_t* decl = MetaCParser_ParseDeclaration(self, 0);
            stmt_decl_t* declStmt = AllocNewStatement(stmt_decl, &result);
            declStmt->Declaration = decl;
            //result = MetaCParser_ParseDeclarationStatement(self, parent);
        }
    }

    // if we didn't parse as a declaration try an expression as the last resort
    if (!result || result == emptyPointer)
    {
        metac_expression_t* exp = MetaCParser_ParseExpression(self, 0);
        stmt_exp_t* expStmt = AllocNewStatement(stmt_exp, &result);
        expStmt->Expression = exp;
        //result = MetaCParser_ParseExpressionStatement(self, parent);
    }
LdoneWithStatement:
    if (prev)
        prev->Next = result;

    if(tokenType != tok_lBrace && MetaCParser_PeekMatch(self, tok_semicolon, true))
    {
        // XXX it shouldn't stay this way ... but for now we want
        // to parse more function bodies.
        MetaCParser_Match(self, tok_semicolon);
    }


    return result;
}


static stmt_block_t* MetaCParser_ParseBlockStatement(metac_parser_t* self,
                                                     metac_statement_t* parent,
                                                     metac_statement_t* prev)
{
    MetaCParser_Match(self, tok_lBrace);

    metac_statement_t* firstStatement = 0;
    metac_statement_t* nextStatement = 0;
    stmt_block_t* result = AllocNewStatement(stmt_block, &result);

    for (;;)
    {
        metac_token_t* peekToken = MetaCParser_PeekToken(self, 1);

        if (peekToken && peekToken->TokenType == tok_rBrace)
        {
            if (!firstStatement)
            {
                firstStatement = _emptyPointer;
            }
            break;
        }

        if (!firstStatement)
        {
            firstStatement = MetaCParser_ParseStatement(self, (metac_statement_t*)result, firstStatement);
            nextStatement = firstStatement;
            if (nextStatement)
            {
                result->Hash = Mix(result->Hash, nextStatement->Hash);
            }
            else
            {
                ParseError(self->LexerState, "Statement expected");
            }
        }
        else
        {
            MetaCParser_ParseStatement(self, (metac_statement_t*)result, nextStatement);
            result->Hash = Mix(result->Hash, nextStatement->Hash);
            if (nextStatement->Next && nextStatement->Next != emptyPointer)
            {
                nextStatement = nextStatement->Next;
            }
        }
    }

    result->Body = firstStatement;

    MetaCParser_Match(self, tok_rBrace);

    return result;
}
/// static lexer for using in the g_lineParser
static metac_lexer_t g_lineLexer = {
    g_lineLexer.inlineTokens,     0, ARRAY_SIZE(g_lineLexer.inlineTokens),
    {g_lineLexer.inlineLocations, 0, ARRAY_SIZE(g_lineLexer.inlineLocations)}
};
/// There can only be one LineParser as it uses static storage
metac_parser_t g_lineParser = { &g_lineLexer };

bool g_exernalIdentifierTable = false;

void LineLexerInit(void)
{
    g_lineParser.CurrentTokenIndex = 0;
    g_lineLexer.TokenSize = 0;
    g_lineLexer.LocationStorage.LocationSize = 0;

    ACCEL_INIT(g_lineLexer, Identifier);
    ACCEL_INIT(g_lineLexer, String);
    if (!g_exernalIdentifierTable)
    {
        ACCEL_INIT(g_lineParser, Identifier);
        ACCEL_INIT(g_lineParser, String);
    }

    if (!g_lineParser.Defines)
    {
        g_lineParser.Defines = g_lineParser.inlineDefines;
        g_lineParser.DefineCapacity = ARRAY_SIZE(g_lineParser.inlineDefines);
    }
}

metac_expression_t* MetaCParser_ParseExpressionFromString(const char* exp)
{
    assert(g_lineLexer.TokenCapacity == ARRAY_SIZE(g_lineLexer.inlineTokens));
    LineLexerInit();
    LexString(&g_lineLexer, exp);

    metac_expression_t* result = MetaCParser_ParseExpression(&g_lineParser, 0);

    return result;
}

metac_statement_t* MetaCParser_ParseStatementFromString(const char* stmt)
{
    assert(g_lineLexer.TokenCapacity == ARRAY_SIZE(g_lineLexer.inlineTokens));
    LineLexerInit();
    LexString(&g_lineLexer, stmt);

    metac_statement_t* result = MetaCParser_ParseStatement(&g_lineParser, 0, 0);

    return result;
}

metac_declaration_t* MetaCParser_ParseDeclarationFromString(const char* decl)
{
    assert(g_lineLexer.TokenCapacity == ARRAY_SIZE(g_lineLexer.inlineTokens));
    LineLexerInit();
    LexString(&g_lineLexer, decl);

    metac_declaration_t* result = MetaCParser_ParseDeclaration(&g_lineParser, 0);

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

#include "metac_printer.h"

#  ifdef TEST_PARSER
void TestParseExprssion(void)
{
    metac_expression_t* expr = MetaCParser_ParseExpressionFromString("12 - 16 - 99");
    assert(!strcmp(PrintExpression(&g_lineParser, expr), "((12 - 16 )- 99 )"));
}

void TestParseDeclaration(void)
{
    metac_declaration_t* decl = MetaCParser_ParseDeclarationFromString("int f(double x);");
    // assert(!strcmp(PrintDeclaration(&g_lineParser, decl, 0, 0), "int f(double x);"));
}

int main(int argc, char* argv[])
{
    TestParseExprssion();
}

#  endif
#endif // ifndef DO_NOT_COMPILE
#endif // _METAC_PARSER_C_
