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
#    error "Unknow ACCEL value " #ACCEL
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

void _newMemRealloc(void** memP, uint32_t* capacity, const uint32_t elementSize);
const char* MetaCExpressionKind_toChars(metac_expression_kind_t type);
const metac_identifier_ptr_t empty_identifier = {~0u};
#define ARRAY_SIZE(A) \
     ((unsigned int)(sizeof((A)) / sizeof((A)[0])))

void MetaCParserInit(metac_parser_t* self)
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

void MetaCParserInitFromLexer(metac_parser_t* self, metac_lexer_t* lexer)
{
    self->Lexer = lexer;
    MetaCParserInit(self);
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

        return GetOrAddIdentifier(MEMBER_SUFFIX(&self->Identifier),
                                  identifierString,
                                  token->IdentifierKey);
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

metac_token_t* MetaCParserNextToken(metac_parser_t* self)
{
#define define_key 0x6a491b
#define ifdef_key 0x581ce0
#define endif_key 0x506843

#define NextToken() \
    ((self->CurrentTokenIndex < self->Lexer->tokens_size) ? \
    self->Lexer->tokens + self->CurrentTokenIndex++ : 0)

#define PeekMatch(TOKEN_TYPE) \
    ( ((result = NextToken()), (result && result->TokenType == TOKEN_TYPE)) ? \
    ( result ) : ( self->CurrentTokenIndex--, (metac_token_t*)0) )

    metac_token_t* result = 0;
    assert(self->Lexer->tokens_size);

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

metac_token_t* MetaCParserPeekToken(metac_parser_t* self, int32_t p)
{
    metac_token_t* result = 0;
    assert(self->Lexer->tokens_size);

    if (cast(uint32_t)(self->CurrentTokenIndex + (p - 1)) < self->Lexer->tokens_size)
    {
        result = self->Lexer->tokens + self->CurrentTokenIndex + (p - 1);
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


metac_token_t* MetaCParserMatch(metac_parser_t* self, metac_token_enum_t type)
{
    metac_token_t* token = MetaCParserNextToken(self);
    metac_token_enum_t got = (token ? token->TokenType : tok_eof);
    if (got != type)
    {
        printf("Expected: %s -- Got: %s\n",
            MetaCTokenEnum_toChars(type), MetaCTokenEnum_toChars(got));
    }
    return token;
}

uint32_t _newExp_size = 0;
uint32_t _newExp_capacity = 0;
metac_expression_t* _newExp_mem = 0;

uint32_t _newStmt_size = 0;
uint32_t _newStmt_capacity = 0;
metac_statement_t* _newStmt_mem = 0;

uint32_t _newDecl_size = 0;
uint32_t _newDecl_capacity = 0;
metac_declaration_t* _newDecl_mem = 0;

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

        case exp_assign    : return "==";

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

void _newMemRealloc(void** memP, uint32_t* capacityP, const uint32_t elementSize)
{
    uint32_t capacity;
    if (!*memP)
    {
        capacity = 1024 / 1.6f;
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

#define AllocNewDeclaration(KIND, RESULT_PTR) \
    (KIND ## _t*) AllocNewDeclaration_(KIND, sizeof(KIND ##_t), ((void**)(RESULT_PTR)))

metac_declaration_t* AllocNewDeclaration_(metac_declaration_kind_t kind, size_t nodeSize, void** result_ptr)
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
        (*result_ptr) = result = _newDecl_mem + _newDecl_size++;
        result->DeclKind = kind;
    }

    return result;
}

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
        (*result_ptr) = result = _newStmt_mem + _newStmt_size++;
        result->StmtKind = kind;
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
            ParseErrorF(self->LexerState, "expected %s but got %s",
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

static inline bool IsTypeToken(metac_token_enum_t tokenType)
{
    bool  result =
           (   tokenType == tok_kw_const
            || (tokenType >= tok_kw_auto && tokenType <= tok_kw_double)
            || tokenType == tok_kw_unsigned
            || tokenType == tok_kw_struct
            || tokenType == tok_kw_enum
            || tokenType == tok_identifier );
    return result;
}

static inline bool IsDeclType(metac_declaration_t* decl)
{
    metac_declaration_kind_t kind = decl->DeclKind;
    return (kind == decl_type
         || kind == decl_struct
         || kind == decl_enum
         || kind == decl_union);
}

#define ErrorDeclaration() \
    (metac_declaration_t*)0

#define U32(VAR) \
	(*(uint32_t*)(&VAR))
metac_declaration_t* MetaCParserParseTypeDeclaration(metac_parser_t* self, metac_declaration_t* parent, metac_declaration_t* prev)
{
    metac_declaration_t* result = 0;

    decl_type_t* type = AllocNewDeclaration(decl_type, &result);
    metac_type_modifiers typeModifiers = typemod_none;
    metac_token_t* currentToken = 0;

LnextToken:
    currentToken = MetaCParserNextToken(self);
    metac_token_enum_t tokenType =
        (currentToken ? currentToken->TokenType : tok_invalid);

    while(IsTypeToken(tokenType))
    {
        if (tokenType == tok_identifier)
        {
            type->TypeKind = type_identifier;
            type->Identifier = RegisterIdentifier(self, currentToken);
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
        else if (tokenType == tok_kw_struct)
        {
            uint32_t memberCount = 0;
            bool isPredeclated = true;

            decl_struct_t* struct_ = AllocNewDeclaration(decl_struct, &result);
            type = (decl_type_t*)struct_;
            struct_->TypeKind = type_struct;

            if (MetaCParserPeekMatch(self, tok_identifier, 1))
            {
                metac_token_t* structName = MetaCParserNextToken(self);
                struct_->Identifier = RegisterIdentifier(self, structName);
            }
            else
            {
                struct_->Identifier = empty_identifier;
            }


            if (MetaCParserPeekMatch(self, tok_lBrace, 1))
            {
                MetaCParserMatch(self, tok_lBrace);
                decl_field_t **nextMemberPtr = &struct_->Fields;

                isPredeclated = false;
                while(!MetaCParserPeekMatch(self, tok_rBrace, 1))
                {
                    decl_field_t* field =
                        AllocNewDeclaration(decl_field, (metac_declaration_t**)nextMemberPtr);
                    field->Next = 0;
                    field->Type = (decl_type_t*)MetaCParserParseTypeDeclaration(self, result, 0);
					assert(IsDeclType((metac_declaration_t*)field->Type));

                    metac_token_t* memberName = MetaCParserMatch(self, tok_identifier);
                    if (!field->Type || !memberName || memberName->TokenType != tok_identifier)
                        return ErrorDeclaration();

                    field->Identifier = RegisterIdentifier(self, memberName);

                    MetaCParserMatch(self, tok_semicolon);
                    nextMemberPtr = &field->Next;
                    struct_->FieldCount++;
                }
                MetaCParserMatch(self, tok_rBrace);
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

    return result;
}


metac_declaration_t* MetaCParserParseDeclaration(metac_parser_t* self, metac_declaration_t* parent)
{
    metac_token_t* currentToken = MetaCParserPeekToken(self, 1);
    metac_token_enum_t tokenType =
        (currentToken ? currentToken->TokenType : tok_invalid);

	metac_declaration_t* result = 0;
    if (tokenType == tok_kw_struct)
    {
        return MetaCParserParseTypeDeclaration(self, parent, 0);
    }
    else if (tokenType == tok_kw_enum)
    {
        return MetaCParserParseTypeDeclaration(self, parent, 0);
    }
    else if (tokenType == tok_kw_typedef)
    {
/*
        typedef struct metac_identifier_table_slot_t
        {
            uint32_t HashKey;
            metac_identifier_ptr_t Ptr;
        } metac_identifier_table_slot_t;
*/

        MetaCParserMatch(self, tok_kw_typedef);
        currentToken = MetaCParserPeekToken(self, 1);
            tokenType =
        (currentToken ? currentToken->TokenType : tok_invalid);
        decl_typedef_t* typdef = AllocNewDeclaration(decl_typedef, &result);

        if (tokenType == tok_kw_struct)
        {
            printf("struct defined in typdef\n");
        }
        typdef->Type = MetaCParserParseTypeDeclaration(self, (metac_declaration_t*) typdef, 0);
        metac_token_t* name = MetaCParserMatch(self, tok_identifier);
        if (!name || name->TokenType != tok_identifier)
        {
            printf("Expecting an identifier to follow the type definition of a typedef\n");
            return ErrorDeclaration();
        }
        typdef->Identifier = RegisterIdentifier(self, name);
    }
    else if (tokenType <= tok_kw_auto && tokenType >= tok_kw_double)
    {
        // Peek until the next identifer and see if there is a lParen after to
        metac_token_t* peekId = 0;
        uint32_t peekCount = 2;
        do {
            (peekId = MetaCParserPeekToken(self, peekCount++));
        } while(!peekId || peekId->TokenType == tok_identifier);
        if (peekId)
        {
            RegisterIdentifier(self, peekId);
        }

        decl_variable_t* var = AllocNewDeclaration(decl_variable, &result);
        metac_declaration_t* type = MetaCParserParseTypeDeclaration(self, parent, 0);

    }
    else if (tokenType == tok_identifier)
    {

    }

	return result;
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
        if (!MetaCParserPeekMatch(self, tok_rParen, 1));
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
        result->StringKey = currentToken->StringKey;
        result->Hash = currentToken->StringKey;
        PushOperand(result);
    }
    else if (tokenType == tok_identifier)
    {
        result = AllocNewExpression(exp_identifier);

        result->IdentifierPtr = RegisterIdentifier(self, currentToken);
        result->IdentifierKey = currentToken->IdentifierKey;
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
            ParseError(self->LexerState, "Expected typeof to be followed by '('");
        }

        metac_expression_t* parenExp = MetaCParserParseExpression(self, 0);
        PopOperator(exp_typeof);
        assert(parenExp->Kind == exp_paren);
        result->E1 = parenExp->E1;
    }
    else if (tokenType == tok_kw_assert)
    {
        result = AllocNewExpression(exp_assert);
        PushOperator(exp_assert);
        metac_token_t* nextToken = MetaCParserPeekToken(self, 1);
        if (!nextToken || nextToken->TokenType != tok_lParen)
        {
            ParseError(self->LexerState, "Expected assert to be followed by '('");
        }
        metac_expression_t* parenExp = MetaCParserParseExpression(self, 0);
        PopOperator(exp_assert);
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
        else if (peekNext->TokenType == tok_plusplus)
        {
            metac_expression_t* E1 = result;
            PushOperator(exp_post_increment);
            result = AllocNewExpression(exp_post_increment);
            result->E1 = E1;
            PopOperator(exp_post_increment);
        }
        else if (peekNext->TokenType == tok_minusminus)
        {
            metac_expression_t* E1 = result;
            PushOperator(exp_post_decrement);
            result = AllocNewExpression(exp_post_decrement);
            result->E1 = E1;
            PopOperator(exp_post_decrement);
        }
        else if (peekNext->TokenType == tok_lBracket)
        {
            MetaCParserMatch(self, tok_lBracket);
            metac_expression_t* E1 = result;
            // PushOperator(exp_index);
            result = AllocNewExpression(exp_dotdot);
            result->E1 = E1;
            result->E2 = MetaCParserParseExpression(self, 0);
            MetaCParserMatch(self, tok_rBracket);
            // PopOperator(exp_index);
        }
    }
    return result;
}
#undef PushOperand
#undef PushOperator
#undef PopOperator
#undef PopOperand

static stmt_block_t* MetaCParserParseBlockStatement(metac_parser_t* self, metac_statement_t* parent);

#define ErrorStatement() \
    (metac_statement_t*)0

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
        stmt_if_t* result = AllocNewStatement(stmt_if, &result);
        MetaCParserMatch(self, tok_kw_if);
        MetaCParserPeekMatch(self, tok_lParen, 0);
        metac_expression_t* condExpP =
            MetaCParserParseExpression(self, 0);
        assert(condExpP->Kind == exp_paren);
        result->IfCond = condExpP;
        result->IfBody = (metac_statement_t*)MetaCParserParseBlockStatement(self, (metac_statement_t*)result);
        if (MetaCParserPeekMatch(self, tok_kw_else, 1))
        {
            result->ElseBody = (metac_statement_t*)MetaCParserParseBlockStatement(self, (metac_statement_t*)result);
        }
    }
    else if (tokenType == tok_kw_switch)
    {
        uint32_t switchHash =
            crc32c(~0, "switch", sizeof("switch") - 1);
        stmt_switch_t* result = AllocNewStatement(stmt_switch, &result);

        MetaCParserMatch(self, tok_kw_switch);
        MetaCParserMatch(self, tok_lParen);
        metac_expression_t* cond =
            MetaCParserParseExpression(self, 0);
        switchHash = Mix(switchHash, cond->Hash);
        MetaCParserMatch(self, tok_rParen);
        if (!MetaCParserPeekMatch(self, tok_lBrace, 0))
        {
            ParseError(self->LexerState, "parsing switch failed\n");
            return ErrorStatement();
        }

        metac_statement_t* caseBlock =
            (metac_statement_t*)MetaCParserParseBlockStatement(self, (metac_statement_t*)result);
        switchHash = Mix(switchHash, caseBlock->Hash);
    }
    else if (tokenType == tok_identifier)
    {
        metac_token_t* peek2 = MetaCParserPeekToken(self, 2);
        if (peek2 && peek2->TokenType == tok_colon)
        {
            MetaCParserMatch(self, tok_identifier);
            MetaCParserMatch(self, tok_colon);

            stmt_label_t* result = AllocNewStatement(stmt_label, &result);
            result->Label = GetOrAddIdentifier(MEMBER_SUFFIX(&self->Identifier),
                                               IDENTIFIER_PTR(MEMBER_SUFFIX(&self->Lexer->Identifier), *currentToken),
                                               currentToken->IdentifierKey);
        }
    }
    else if (tokenType == tok_kw_goto)
    {
        stmt_goto_t* result = AllocNewStatement(stmt_goto, &result);
        uint32_t gotoHash = crc32c(~0, "goto", sizeof("goto") - 1);

        MetaCParserMatch(self, tok_kw_goto);
        metac_token_t* label = MetaCParserMatch(self, tok_identifier);
#ifdef IDENTIFIER_TABLE
        result->Label = GetOrAddIdentifier(&self->IdentifierTable,
                                           IDENTIFIER_PTR(&self->Lexer->IdentifierTable, *label),
#endif
#ifdef IDENTIFIER_TREE
        result->Label = GetOrAddIdentifier(&self->IdentifierTree,
                                           IDENTIFIER_PTR(&self->Lexer->IdentifierTree, *label),
#endif

                                           label->IdentifierKey);
        result->Hash = Mix(gotoHash, label->IdentifierKey);
    }
    else if (tokenType == tok_kw_case)
    {
        uint32_t caseHash =
            crc32c(~0, "case", sizeof("case") - 1);
        stmt_case_t* result = AllocNewStatement(stmt_case, &result);

        MetaCParserMatch(self, tok_kw_case);
        metac_expression_t* caseExp =
            MetaCParserParseExpression(self, 0);
        MetaCParserMatch(self, tok_colon);

        caseHash = Mix(caseHash, caseExp->Hash);
        MetaCParserMatch(self, tok_rParen);
        result->Next =
            MetaCParserParseStatement(self, (metac_statement_t*)result);
        if (result->Next)
        {
            caseHash = Mix(caseHash, result->Next->Hash);
        }
        result->Hash = caseHash;
    }
    else if (tokenType == tok_lBrace)
    {
        result = (metac_statement_t*)MetaCParserParseBlockStatement(self, parent);
    }
    if(tokenType != tok_lBrace)
        MetaCParserMatch(self, tok_semicolon);
    return result;
}


static stmt_block_t* MetaCParserParseBlockStatement(metac_parser_t* self, metac_statement_t* parent)
{
    MetaCParserMatch(self, tok_lBrace);

    metac_statement_t* firstStatement = 0;
    metac_statement_t* nextStatement = 0;
    stmt_block_t* result = AllocNewStatement(stmt_block, &result);

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
            firstStatement = MetaCParserParseStatement(self, (metac_statement_t*)result);
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
            nextStatement->Next = MetaCParserParseStatement(self, (metac_statement_t*)result);
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
    g_lineLexer.inlineTokens,
    0,
    (sizeof(g_lineLexer.inlineTokens) / sizeof(g_lineLexer.inlineTokens[0]))
};
/// There can only be one LineParser as it uses static storage
metac_parser_t g_lineParser = { &g_lineLexer };

metac_expression_t* MetaCParserParseExpressionFromString(const char* exp)
{
    assert(g_lineLexer.tokens_capacity == ARRAY_SIZE(g_lineLexer.inlineTokens));
    g_lineParser.CurrentTokenIndex = 0;
    g_lineLexer.tokens_size = 0;
#ifdef IDENTIFIER_TABLE
    IdentifierTableInit(&g_lineLexer.IdentifierTable);
#endif
#ifdef IDENTIFIER_TREE
    IdentifierTreeInit(&g_lineLexer.IdentifierTree);
#endif

#ifdef IDENTIFIER_TABLE
    if (!g_lineParser.IdentifierTable.Slots)
    {
        IdentifierTableInit(&g_lineParser.IdentifierTable);
    }
#endif
#ifdef IDENTIFIER_TREE
    if (!g_lineParser.IdentifierTree.Root)
    {
        IdentifierTreeInit(&g_lineParser.IdentifierTree);
    }
#endif

    if (!g_lineParser.Defines)
    {
        g_lineParser.Defines = g_lineParser.inlineDefines;
        g_lineParser.DefineCapacity = ARRAY_SIZE(g_lineParser.inlineDefines);
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
#ifdef IDENTIFIER_TABLE
    IdentifierTableInit(&g_lineLexer.IdentifierTable);
    if (!g_lineParser.IdentifierTable.Slots)
    {
        IdentifierTableInit(&g_lineParser.IdentifierTable);
    }
#endif
#ifdef IDENTIFIER_TREE
    IdentifierTreeInit(&g_lineLexer.IdentifierTree);
    if (!g_lineParser.IdentifierTree.Root)
    {
        IdentifierTreeInit(&g_lineParser.IdentifierTree);
    }
#endif
    LexString(&g_lineLexer, exp);

    metac_statement_t* result = MetaCParserParseStatement(&g_lineParser, 0);

    return result;
}

metac_declaration_t* MetaCParserParseDeclarationFromString(const char* exp)
{
    assert(g_lineLexer.tokens_capacity == ARRAY_SIZE(g_lineLexer.inlineTokens));
    g_lineParser.CurrentTokenIndex = 0;
    g_lineLexer.tokens_size = 0;
#ifdef IDENTIFIER_TABLE
    IdentifierTableInit(&g_lineLexer.IdentifierTable);
    if (!g_lineParser.IdentifierTable.Slots)
    {
        IdentifierTableInit(&g_lineParser.IdentifierTable);
    }
#endif
#ifdef IDENTIFIER_TREE
    IdentifierTreeInit(&g_lineLexer.IdentifierTree);
    if (!g_lineParser.IdentifierTree.Root)
    {
        IdentifierTreeInit(&g_lineParser.IdentifierTree);
    }
#endif
    LexString(&g_lineLexer, exp);

    metac_declaration_t* result = MetaCParserParseDeclaration(&g_lineParser, 0);

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
void PrintSpace(metac_parser_t* self)
{
    printf(" ");
}

void PrintNewline(metac_parser_t* self)
{
    printf("\n");
}

bool IsBinaryExp(metac_expression_kind_t type)
{
    return (type >= FIRST_BINARY_EXP(TOK_SELF) && type <= LAST_BINARY_EXP(TOK_SELF));
}

void static inline PrintIndent(metac_parser_t* self, uint32_t indent)
{
    for (int i = 0; i < indent; i++)
        printf("    ");
}

void PrintString(metac_parser_t* self, const char* string)
{
    printf("%s", string);
}

void PrintIdentifier(metac_parser_t* self,
                     metac_identifier_ptr_t idPtr)
{
    if (idPtr.v == empty_identifier.v)
        assert(0); // One is not supposed to print the empty identifier
    printf("%s", IdentifierPtrToCharPtr(MEMBER_SUFFIX(&self->Identifier),
                                        idPtr));
}

void PrintKeyword(metac_parser_t* self,
                  metac_token_enum_t keyword)
{
    printf("%s", MetaCTokenEnum_toChars(keyword) + sizeof("tok_kw"));
}

void PrintToken(metac_parser_t* self,
                  metac_token_enum_t tokenType)
{
#define CASE_(KW) \
    case KW :

    switch(tokenType)
    {
        FOREACH_KEYWORD_TOKEN(CASE_)
            assert(0);

        case tok_lBrace :
            PrintString(self, "{");
        break;
        case tok_rBrace :
            PrintString(self, "}");
        break;
    }
}


void PrintDeclaration(metac_parser_t* self, metac_declaration_t* decl,
					  uint32_t indent, uint32_t level)
{
    PrintIndent(self, indent);

    switch (decl->DeclKind)
    {
        case decl_typedef:
        {
            decl_typedef_t* typdef = (decl_typedef_t*) decl;
            printf("typedef ");
            level++;
            PrintDeclaration(self, (metac_declaration_t*)typdef->Type, indent, level);
            PrintIdentifier(self, typdef->Identifier);
            level--;
        } break;
        case decl_type:
        {
            decl_type_t* type = (decl_type_t*) decl;
            // printf("TypeKind: %d\n", (int) type->Kind);
            if (type->TypeModifiers)
            {
                uint32_t modifiers = type->TypeModifiers;
                if (modifiers & typemod_const)
                {
                    PrintString(self, "const ");
                }
                if (modifiers & typemod_unsigned)
                {
                    PrintString(self, "unsigned ");
                }

            }
            if (type->TypeKind >= type_auto && type->TypeKind <= type_double)
            {
				metac_token_enum_t tok = (metac_token_enum_t)
					((type->TypeKind - type_type) + tok_kw_type);
                PrintKeyword(self, tok);
            }
            else if (type->TypeKind == type_identifier)
            {
                PrintIdentifier(self, type->Identifier);

                //printf("type_identifier: %d\n", type->Identifier.v);
            }
        }  break;
        case decl_struct :
        {
            decl_struct_t* struct_ = (decl_struct_t*) decl;
            PrintKeyword(self, tok_kw_struct);
            if (struct_->Identifier.v != empty_identifier.v)
            {
                PrintSpace(self);
                PrintIdentifier(self, struct_->Identifier);
            }
            PrintSpace(self);
            PrintToken(self, tok_lBrace);
            PrintNewline(self);
            ++level;
            ++indent;
            decl_field_t* f = struct_->Fields;
            for(int memberIndex = 0;
                memberIndex < struct_->FieldCount;
                memberIndex++)
            {
                PrintDeclaration(self, (metac_declaration_t*)f, indent, level);
                printf("\n");
                f = f->Next;
            }
            --indent;
            --level;
            PrintIndent(self, indent);
            PrintToken(self, tok_rBrace);
            if (indent)
                printf("\n");
            else
                printf(" ");
        } break;
        case decl_field :
        {
            decl_field_t* field = (decl_field_t*) decl;
            PrintDeclaration(self, (metac_declaration_t*)field->Type, indent, level);
            PrintSpace(self);
            PrintIdentifier(self, field->Identifier);
        } break;
    }
    if (!indent) printf(";\n");
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
        const char* ident = IdentifierPtrToCharPtr(
#ifdef IDENTIFIER_TABLE
                &self->IdentifierTable,
#endif
#ifdef IDENTIFIER_TREE
                &self->IdentifierTree,
#endif
                exp->IdentifierPtr
            );
        expStringLength += sprintf(scratchpad + expStringLength, "%s ",
            ident
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

        const char* op = BinExpTypeToChars((metac_binary_expression_kind_t)exp->Kind);
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
    else if (exp->Kind == exp_post_increment || exp->Kind == exp_post_decrement)
    {
        const char* op = 0;
        if (exp->Kind == exp_post_increment)
            op = "++";
        else if (exp->Kind == exp_post_decrement)
            op = "--";

        assert(op);

        if (!IsBinaryExp(exp->E1->Kind))
            scratchpad[expStringLength++] = '(';

        const char* e1  = PrintExpression(self, exp->E1);
        uint32_t e1_length = strlen(e1);
        memcpy(scratchpad + expStringLength, e1, e1_length);
        free((void*)e1);
        expStringLength += e1_length;

        if (!IsBinaryExp(exp->E1->Kind))
            scratchpad[expStringLength++] = ')';

        expStringLength += sprintf(scratchpad + expStringLength, "%s ", op);
    }
    else if (exp->Kind == exp_inject || exp->Kind == exp_eject || exp->Kind == exp_typeof || exp->Kind == exp_assert)
    {
        {
            const char* op = 0;
            if (exp->Kind == exp_inject)
                op = "inject";
            else if (exp->Kind == exp_eject)
                op = "eject";
            else if (exp->Kind == exp_typeof)
                op = "typeof";
            else if (exp->Kind == exp_assert)
                op = "assert";

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

#  ifdef TEST_PARSER
void TestParseExprssion(void)
{
    metac_expression_t* expr = MetaCParserParseExpression("12 - 16 - 99");
    assert(!strcmp(PrintExpression(self, "(12 - (16 - 99 ))")));
    ReorderExpression(exp);
    assert(!strcmp(PrintExpression(self, "((12 - 16 ) - 99 )")));
}
#  endif

#endif // ifndef DO_NOT_COMPILE
#endif // _METAC_PARSER_C_
