#include "../parser/metac_expr_parser.h"

#include "../debug/debug_server.h"

const char* BinExpTypeToChars(metac_binary_expr_kind_t t)
{
    switch(t)
    {
        case bin_exp_invalid: assert(0);

        case expr_comma     : return ",";
        case expr_dot       : return ".";
        case expr_dotdot    : return "..";
        case expr_arrow     : return "->";

        case expr_add       : return "+";
        case expr_sub       : return "-";
        case expr_mul       : return "*";
        case expr_div       : return "/";
        case expr_rem       : return "%";
        case expr_xor       : return "^";
        case expr_or        : return "|";
        case expr_and       : return "&";
        case expr_lsh       : return "<<";
        case expr_rsh       : return ">>";

        case expr_oror      : return "||";
        case expr_andand    : return "&&";

        case expr_assign    : return "=";

        case expr_add_ass   : return "+=";
        case expr_sub_ass   : return "-=";
        case expr_mul_ass   : return "*=";
        case expr_div_ass   : return "/=";
        case expr_rem_ass   : return "%=";
        case expr_xor_ass   : return "^=";
        case expr_or_ass    : return "|=";
        case expr_and_ass   : return "&=";
        case expr_lsh_ass   : return "<<=";
        case expr_rsh_ass   : return ">>=";

        case expr_eq        : return "==";
        case expr_neq       : return "!=";
        case expr_lt        : return "<";
        case expr_le        : return "<=";
        case expr_gt        : return ">";
        case expr_ge        : return ">=";
        case expr_spaceship : return "<=>";
        case expr_template_instance: return "!";
    }

    printf("Not a binary operator: %s\n",
        MetaCExprKind_toChars( (metac_expr_kind_t)t ));
    assert(0);
    return 0;
}

metac_expr_kind_t UnaExpTypeFromTokenType(metac_token_enum_t tokenType,
                                          metac_token_t* peek2)
{
    metac_expr_kind_t result = expr_invalid;

    if (tokenType == tok_kw_eject)
    {
        result = expr_eject;
    }
    else if (tokenType == tok_kw_inject)
    {
        result = expr_inject;
    }
    else if (tokenType == tok_kw_typeof)
    {
        result = expr_typeof;
    }
    else if (tokenType == tok_kw_sizeof)
    {
        result = expr_sizeof;
    }
    else if (tokenType == tok_kw_assert)
    {
        result = expr_assert;
    }
    else if (tokenType == tok_minus)
    {
        result = expr_umin;
    }
    else if (tokenType == tok_minusminus)
    {
        result = expr_decrement;
    }
    else if (tokenType == tok_plusplus)
    {
        result = expr_increment;
    }
    else if (tokenType == tok_and)
    {
        result = expr_addr;
    }
    else if (tokenType == tok_star)
    {
        result = expr_deref;
    }
    else if (tokenType == tok_dollar)
    {
        result = expr_outer;
    }
    else if (tokenType == tok_bang)
    {
        result = expr_not;
    }
    else if (tokenType == tok_tilde)
    {
        result = expr_compl;
    }
    else if (tokenType == tok_at)
    {
#define run_key 0x3809a6
        if (peek2 && peek2->TokenType == tok_identifier
         && peek2->IdentifierKey == run_key)
        {
            result = expr_run;
        }
    }

    return result;
}


metac_expr_kind_t BinExpTypeFromTokenType(metac_token_enum_t tokenType)
{
    metac_expr_kind_t result = expr_invalid;
    if (((tokenType >= FIRST_BINARY_TOKEN(TOK_SELF)) & (tokenType <= LAST_BINARY_TOKEN(TOK_SELF))))
    {
        result = cast(metac_expr_kind_t)(cast(int)tokenType -
                (cast(int)FIRST_BINARY_TOKEN(TOK_SELF) -
                 cast(int)FIRST_BINARY_EXP(TOK_SELF)));
    }

    if (tokenType == tok_lParen)
        result = expr_call;

    if (tokenType == tok_lBracket)
        result = expr_index;

    if (tokenType == tok_bang)
        result =  expr_template_instance;

    return result;
}

metac_expr_kind_t ExpTypeFromTokenType(metac_token_enum_t tokenType)
{
    if (tokenType == tok_uint)
    {
        return expr_signed_integer;
    }
    else if (tokenType == tok_float)
    {
        return expr_float;
    }
    else if (tokenType == tok_string)
    {
        return expr_string;
    }
    else if (tokenType == tok_lParen)
    {
        return expr_paren;
    }
    else if (tokenType == tok_kw_inject)
    {
        return expr_inject;
    }
    else if (tokenType == tok_kw_eject)
    {
        return expr_eject;
    }
    else if (tokenType == tok_kw_assert)
    {
        return expr_assert;
    }
    else if (tokenType == tok_dollar)
    {
        return expr_outer;
    }
    else if (tokenType == tok_and)
    {
        return expr_addr_or_and;
    }
    else if (tokenType == tok_star)
    {
        return expr_deref_or_mul;
    }
    else if (tokenType == tok_identifier)
    {
        return expr_identifier;
    }
    else
    {
        assert(0);
        return expr_invalid;
    }

}

static inline bool IsPostfixOperator(metac_token_enum_t t)
{
    return (t == tok_plusplus || t == tok_minusminus);
}

static inline bool IsBinaryOperator(metac_token_enum_t t, parse_expr_flags_t flags)
{
    if ((flags & (expr_flags_call | expr_flags_enum)) != 0
     && (t == tok_comma))
        return false;

    if (((flags & expr_flags_binary) != 0) && UnaExpTypeFromTokenType(t, 0))
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

void MetaCParser_PushOpenParens(metac_parser_t* self)
{
    ARENA_ARRAY_ADD(self->ExprParser.OpenParensStack,
                    self->ExprParser.OpenParens);
    self->ExprParser.OpenParens = 0;
}

void MetaCParser_PopOpenParens(metac_parser_t* self)
{
    assert(self->ExprParser.OpenParensStackCount >= 1);

    self->ExprParser.OpenParens =
        self->ExprParser.OpenParensStack[
            --self->ExprParser.OpenParensStackCount
        ];
}

static inline uint32_t OpToPrecedence(metac_expr_kind_t exp)
{
    if (exp == expr_comma)
    {
        return 1;
    }
    else if (exp >= expr_assign && exp <= expr_rsh_ass)
    {
        return 3;
    }
    else if (exp == expr_ternary)
    {
        return 5;
    }
    else if (exp == expr_oror)
    {
        return 7;
    }
    else if (exp == expr_andand)
    {
        return 9;
    }
    else if (exp == expr_or)
    {
        return 11;
    }
    else if (exp == expr_xor)
    {
        return 13;
    }
    else if (exp == expr_and)
    {
        return 15;
    }
    else if (exp == expr_eq || exp == expr_neq)
    {
        return 17;
    }
    else if (exp >= expr_lt && exp <= expr_ge)
    {
        return 19;
    }
    else if (exp == expr_rsh || exp == expr_lsh)
    {
        return 21;
    }
    else if (exp == expr_add || exp == expr_sub)
    {
        return 23;
    }
    else if (exp == expr_div || exp == expr_mul || exp == expr_rem)
    {
        return 25;
    }
    else if (exp == expr_deref
          || exp == expr_addr
          || exp == expr_increment
          || exp == expr_decrement)
    {
        return 27;
    }
    else if (exp == expr_post_increment
          || exp == expr_post_decrement)
    {
        return 29;
    }
    else if (exp == expr_call
          || exp == expr_index
          || exp == expr_compl
          || exp == expr_assert
          || exp == expr_template_instance)
    {
        return 30;
    }
    else if (exp == expr_unary_dot)
    {
        return 31;
    }
    else if (exp == expr_arrow || exp == expr_dot)
    {
        return 32;
    }
    else if (exp == expr_umin
          || exp == expr_sizeof
          || exp == expr_typeof
          || exp == expr_not
          || exp == expr_outer)
    {
        return 33;
    }
    else if (exp == expr_paren
          || exp == expr_signed_integer
          || exp == expr_string
          || exp == expr_float
          || exp == expr_identifier
          || exp == expr_char
          || exp == expr_tuple
          || exp == expr_type
          || exp == expr_function)
    {
        return 34;
    }
    else if (exp == expr_comma)
    {
        return 36;
    }

    fprintf(stderr, "There's no precedence for %s\n", MetaCExprKind_toChars(exp));
    assert(0);
    return 0;
}

static inline bool IsPrimaryExprToken(metac_token_enum_t tokenType)
{
#ifdef TYPE_EXP
    if (IsTypeToken(tokenType) && tokenType != tok_star && tokenType != tok_kw_typeof)
        return true;
#endif
    switch(tokenType)
    {
    case tok_lParen:
    case tok_uint:
    case tok_float:
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
            && tok != tok_lBracket && tok != tok_and)
        ||  tok == tok_dotdot
        ||  tok == tok_comma
/*
        ||  tok == tok_lBracket
*/
        ||  tok == tok_colon
        ||  tok == tok_question
        ||  tok == tok_kw_sizeof
        ||  tok == tok_equals_equals
        ||  tok == tok_plusplus
        ||  tok == tok_minusminus
        ||  tok == tok_semicolon
        ||  tok == tok_tilde
        ||  tok == tok_rParen);
}
static bool CouldBeFunctionLiteral(metac_parser_t* self, metac_token_enum_t tok) {
    if (tok != tok_lParen) return false;

    metac_token_t* peek = MetaCParser_PeekToken(self, 2);
    int parenDepth = 1;
    bool hasParamDecl = false;
    
    // Fast path for () {
    if (peek && peek->TokenType == tok_rParen) {
        metac_token_t* peek3 = MetaCParser_PeekToken(self, 3);
        return (peek3 && peek3->TokenType == tok_lBrace);
    }

    for (int i = 2; (peek = MetaCParser_PeekToken(self, i)) != NULL; i++) {
        switch(peek->TokenType) {
            case tok_lParen: {
                parenDepth++;
            } break;
            case tok_rParen: {
                parenDepth--;
                if (parenDepth == 0) {
                    metac_token_t* after = MetaCParser_PeekToken(self, i + 1);
                    // Must have seen Type + Ident AND be followed by {
                    return (after && after->TokenType == tok_lBrace && hasParamDecl);
                }
            } break;
            case tok_kw_struct:
            case tok_kw_union:
            case tok_kw_enum: {
                // Skip the keyword and the tag (e.g., 'struct S')
                // This ensures 'S' isn't mistaken for a parameter identifier
                metac_token_t* tag = MetaCParser_PeekToken(self, i + 1);
                if (tag && tag->TokenType == tok_identifier) {
                    i++; // Skip the identifier tag
                }
            } break;
            default: {
                if (!hasParamDecl) {
                    metac_token_t* next = MetaCParser_PeekToken(self, i + 1);
                    // Check for 'Type Identifier' sequence
                    if (next && IsTypeToken(peek->TokenType) && next->TokenType == tok_identifier) {
                        hasParamDecl = true;
                    }
                }
            } break;
        }

        if (peek->TokenType == tok_semicolon || peek->TokenType == tok_lBrace) 
            return false;
    }
    return false;
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
    bool seenLBracket = false;
    int seenLBrace = 0;
    int rParenPos = 0;
    for(int peekCount = 2;
        (peek = MetaCParser_PeekToken(self, peekCount)), peek;
        peekCount++)
    {
        if (peek->TokenType == tok_lParen)
            return false;

        else if (peek->TokenType == tok_rParen)
        {
            rParenPos = peekCount;
            break;
        }
        else if (peek->TokenType == tok_star)
        {
            seenStar = true;
            if (peekCount == 2)
                return false;
        }
        else if (peek->TokenType == tok_rBracket)
        {
            break;
        }
        else if (peek->TokenType == tok_lBrace)
        {
            seenLBrace++;
            continue;
        }
        else if (peek->TokenType == tok_rBrace)
        {
            seenLBrace--;
            if (seenLBrace != 0)
                continue;
        }
        else if (peek->TokenType == tok_comma)
        {
            if (seenLBrace == 0)
                return false;
            else
                continue;
        }
        else if (peek->TokenType == tok_lBracket)
        {
            seenLBracket = true;
            if (peekCount == 2)
                return false;
        }
        else if (IsBinaryOperator(peek->TokenType, expr_flags_none))
        {
            return false;
        }
        else if (!IsTypeToken(peek->TokenType))
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
        // printf("%s\n", MetaCTokenEnum_toChars(afterParen->TokenType));
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

static inline bool CouldBeType(metac_parser_t* self,
                               metac_token_enum_t tok, parse_expr_flags_t eflags)
{
    bool result = false;
    metac_token_t* peek = 0;
    uint32_t peekN = 2;
    typescan_flags_t flags = TypeScan_None;
    bool isStar = false;
    bool isAnd = false;
    bool inAggregate = false;

    if (eflags & expr_flags_pp)
        return false;

    if (tok == tok_identifier)
    {
        U32(flags) |= TypeScan_FirstWasIdentifier;
    }
    else if (tok == tok_star || tok == tok_uint || tok == tok_float)
    {
        return false;
    }

    while (IsTypeToken(tok))
    {
        result |= true;
        // if we see a * set the seen_star flag
        // if we then see an identifier or a number after the star
        // the expression cannot be a type literal
        if (tok == tok_star)
        {
            U32(flags) |= TypeScan_SeenStar;
            isStar = true;
        }
        else if (tok == tok_kw_enum || tok == tok_kw_struct)
        {
            inAggregate = true;
        }
        else if (tok == tok_and)
        {
            isAnd = true;
        }
        else if (tok == tok_lBrace && !inAggregate)
        {
#ifndef PARSE_TYPE_TUPLE_EXP
            result = false;
            break;
#endif
        }
        if (isStar)
        {
            isStar = false;
            metac_token_t* peek2 = MetaCParser_PeekToken(self, peekN);
            metac_token_enum_t tok2 = peek2 ? peek2->TokenType : tok_eof;

            if ((tok2 == tok_identifier)
             |  (tok2 == tok_uint)
             |  (tok2 == tok_float)
            /* |  ( IsPunctuationToken(tok2) &&
                  tok != tok_rParen)*/ )
             {
                result = false;
                break;
             }
        }
        else if (isAnd)
        {
            isAnd = false;
        }
        peek = MetaCParser_PeekToken(self, peekN++);
        tok = (peek ? peek->TokenType : tok_eof);
    }
    // if we just had a single identifier don't
    // treat it as a type as we can resolve single identifiers just fine
    // and they don't impact parsing
#if defined(PARSE_IDENTIFIER_EXPRESSIONS_BY_DEFAULT)
    if ((flags & TypeScan_FirstWasIdentifier) != 0)
    {
        result = false;
    }
#endif
    return result;
}


metac_expr_t* MetaCParser_ParsePrimaryExpr(metac_parser_t* self, parse_expr_flags_t flags)
{
    metac_expr_t* result = 0;

    metac_token_t* currentToken = MetaCParser_PeekToken(self, 1);
    if (currentToken)
    {
/*
        printf("ParsePrimaryExpr-currentToken: %s\n", MetaCTokenEnum_toChars(currentToken->TokenType));
        if (currentToken->TokenType == tok_identifier)
            printf("    id: %s\n", IdentifierPtrToCharPtr(&self->Lexer->IdentifierTable, currentToken->IdentifierPtr));
*/
    }
#if !defined(NO_PREPROCESSOR)
    if (self->Preprocessor)
    {
/*
        printf("Preproc is there -- InDefine: %u\n",
               self->Preprocessor->DefineTokenStackCount > 0);
*/
    }
#endif
    metac_token_enum_t tokenType =
        (currentToken ? currentToken->TokenType : tok_eof);

    metac_location_t loc = (currentToken ?
        LocationFromToken(self, currentToken) :
        invalidLocation);
    uint32_t hash = 0;

    if (tokenType == tok_lParen)
    {
        if (CouldBeFunctionLiteral(self, tokenType))
        {
            metac_token_t* peek2 = MetaCParser_PeekToken(self, 2);
            // Parse function literal: (ParamList?) BlockStatement
#define function_key 0x8cbe2f
            hash = function_key;
            result = AllocNewExpr(expr_function);
                
            // Check for empty params '()' vs '(...)'
            if (peek2 && peek2->TokenType == tok_rParen)
            {
                MetaCParser_Match(self, tok_lParen);
                MetaCParser_Match(self, tok_rParen);
                METAC_NODE(result->FunctionExp.Parameters) = emptyNode;
                result->FunctionExp.ParameterCount = 0;
                hash ^= CRC32C_PARENSTARPAREN;
            }
            else
            {
                decl_parameter_list_t paramList;
                paramList = ParseParameterList(self, result);
                result->FunctionExp.ParameterCount = paramList.ParameterCount;
                result->FunctionExp.Parameters = paramList.List;
                hash ^= paramList.Hash;
            }
            
            // Parse mandatory '{ ... }' block
            result->FunctionExp.Body = MetaCParser_ParseStmt(self, 0, 0);
            hash = CRC32C_VALUE(hash, result->FunctionExp.Body->Hash);
            MetaCLocation_Expand(&loc, LocationFromToken(self, MetaCParser_PeekToken(self, 0)));
 
            result->Hash = hash;
        }            
        else if (CouldBeCast(self, tokenType))
        {
            // printf("going to parse cast\n");
            hash = cast_key;
            result = AllocNewExpr(expr_cast);
            //typedef unsigned int b;
            metac_token_t* lParen = MetaCParser_Match(self, tok_lParen);
            metac_type_modifiers typeModifiers = ParseTypeModifiers(self);

            metac_token_t* peek = MetaCParser_PeekToken(self, 1);

            if (peek && IsTypeToken(peek->TokenType))
            {
                result->CastType = MetaCParser_ParseTypeDecl(self, 0, 0);
                result->CastType->TypeModifiers =  typeModifiers;
            }
            else
            {
                AllocNewDecl(decl_type, &result->CastType);
                result->CastType->TypeModifiers = typeModifiers;
                result->CastType->TypeKind = type_modifiers;
                result->CastType->Hash = CRC32C_VALUE(~0, typeModifiers);
            }
            hash = CRC32C_VALUE(hash, result->CastType->Hash);
            MetaCParser_Match(self, tok_rParen);
            result->CastExp = MetaCParser_ParseExpr(self, flags, 0);

            hash = CRC32C_VALUE(hash, result->CastExp->Hash);

            if (MetaCLocationPtr_IsValid(result->CastExp->LocationIdx))
            {
                MetaCLocation_Expand(&loc,
                    MetaCLocationStorage_FromPtr(&self->LocationStorage, result->CastExp->LocationIdx));
            }
            result->Hash = hash;
        } 
        else // None of the special 'cast()' or '() {}' cases match 
        {
            self->ExprParser.OpenParens++;

            MetaCParser_Match(self, tok_lParen);
            result = AllocNewExpr(expr_paren);
            result->Hash = CRC32C_PARENPAREN;
            if (!MetaCParser_PeekMatch(self, tok_rParen, 1))
            {
                result->E1 = MetaCParser_ParseExpr(self, expr_flags_none, 0);
                result->Hash = CRC32C_VALUE(result->Hash, result->E1->Hash);
            }
            else
            {
                METAC_NODE(result->E1) = emptyNode;
            }
        
            metac_token_t* endParen =
                MetaCParser_Match(self, tok_rParen);
            if (endParen)
            {
                MetaCLocation_Expand(&loc, LocationFromToken(self, endParen));

                self->ExprParser.OpenParens--;
            }
        }
    }
#ifdef TYPE_EXP
    else if (CouldBeType(self, tokenType, flags))
    {
        decl_type_t* type =
            MetaCParser_ParseTypeDecl(self, 0, 0);
        result = AllocNewExpr(expr_type);
        result->TypeExp = type;
        MetaCLocation_Expand(&loc,
            MetaCLocationStorage_FromPtr(&self->LocationStorage, result->TypeExp->LocationIdx));
        result->Hash = CRC32C_VALUE(type_key, result->TypeExp->Hash);
    }
#endif
    else if (tokenType == tok_uint)
    {
        MetaCParser_Match(self, tok_uint);
        result = AllocNewExpr(expr_signed_integer);
        result->ValueI64 = currentToken->ValueI64;
        result->NumberFlags = (number_flags_t)currentToken->NumberFlags;
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
    else if (tokenType == tok_float)
    {
        MetaCParser_Match(self, tok_float);
        result = AllocNewExpr(expr_float);
        result->ValueF23 = currentToken->ValueF23;
        int32_t val32 = *(int32_t*)&currentToken->ValueF23;
        result->Hash = CRC32C_VALUE(~0, val32);
    }
    else if (tokenType == tok_string)
    {
        // Match the initial string token
        currentToken = MetaCParser_Match(self, tok_string);

        // Extract the length and pointer to the string
        uint32_t stringLength = LENGTH_FROM_STRING_KEY(currentToken->StringKey);
        const char* currentStringP = IdentifierPtrToCharPtr(&self->Lexer->StringTable, currentToken->StringPtr);

        // Initialize hash and other variables
        uint32_t stringHash = crc32c_nozero(~0, currentStringP, stringLength);
        uint32_t currentLen = stringLength;
        uint32_t adjacentStrings = 1;

        // A buffer to hold concatenated strings
        char stringBuffer[8192];

        // A pointer that tracks the current position in the buffer
        char* stringBufferP = stringBuffer;

        // Allocate result expression
        result = AllocNewExpr(expr_string);

        // Process adjacent string tokens
        while (MetaCParser_PeekMatch(self, tok_string, 1))
        {
            // Match the next token and expand the location
            currentToken = MetaCParser_Match(self, tok_string);
            MetaCLocation_Expand(&loc, LocationFromToken(self, currentToken));

            // Concatenate the string into the buffer
            memcpy(stringBufferP, currentStringP, currentLen);
            stringBufferP += currentLen;  // Move the buffer pointer to the next free space

            // Update current string and length for the next token
            currentLen = LENGTH_FROM_STRING_KEY(currentToken->StringKey);
            currentStringP = IdentifierPtrToCharPtr(&self->Lexer->StringTable, currentToken->StringPtr);

            // Update the hash and total length
            stringHash = crc32c_nozero(stringHash, currentStringP, currentLen);
            stringLength += currentLen;

            // Increment the count of adjacent strings
            adjacentStrings++;

            // Check for buffer overflow
            if (stringLength >= sizeof(stringBuffer)) {
                assert(!"Concatenated string too long");
            }
        }

        // If there was only one string, no need to concatenate
        if (adjacentStrings == 1)
        {
            result->StringPtr = RegisterString(self, currentToken);
            result->StringKey = currentToken->StringKey;
            result->Hash = currentToken->StringKey;
        }
        else
        {
            // For multiple concatenated strings, copy the last part
            memcpy(stringBufferP, currentStringP, currentLen);

            // Verify the hash matches the concatenated string
            assert(crc32c_nozero(~0, stringBuffer, stringLength) == stringHash);

            // Register the concatenated string
            result->StringKey = STRING_KEY(stringHash, stringLength);
            result->StringPtr = GetOrAddIdentifier(&self->StringTable, result->StringKey, stringBuffer);
            result->Hash = result->StringKey;
        }
    }
    else if (tokenType == tok_char)
    {
        MetaCParser_Match(self, tok_char);
        result = AllocNewExpr(expr_char);
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
        result = AllocNewExpr(expr_identifier);
        MetaCParser_Match(self, tok_identifier);
        result->IdentifierPtr = RegisterIdentifier(self, currentToken);
        result->IdentifierKey = currentToken->IdentifierKey;
        result->Hash = currentToken->IdentifierKey;
        //PushOperand(result);
    }
    else if (tokenType == tok_lBrace)
    {
        // self->OpenBraces++;
        MetaCParser_Match(self, tok_lBrace);
        expr_tuple_t* tupleList = (expr_tuple_t*)emptyPointer;
        uint32_t hash = ~0;

        expr_tuple_t** nextElement = &tupleList;
        uint32_t nElements = 0;
        while (!MetaCParser_PeekMatch(self, tok_rBrace, 1))
        {
            nElements++;
            assert((*nextElement) == _emptyPointer);

            (*nextElement) = (expr_tuple_t*)AllocNewExpr(expr_tuple);
            metac_expr_t* exp = MetaCParser_ParseExpr(self, expr_flags_call, 0);
            hash = CRC32C_VALUE(hash, exp->Hash);
            ((*nextElement)->Expr) = exp;
            nextElement = &((*nextElement)->Next);
            (*nextElement) = (expr_tuple_t*) _emptyPointer;

            if(MetaCParser_PeekMatch(self, tok_comma, 1))
            {
                MetaCParser_Match(self, tok_comma);
            }
        }
        metac_token_t* endBrace =
            MetaCParser_Match(self, tok_rBrace);
        MetaCLocation_Expand(&loc, LocationFromToken(self, endBrace));

        result = AllocNewExpr(expr_tuple);
        result->Hash = hash;
        result->TupleExprList = tupleList;
        result->TupleExprCount = nElements;

        //PopOperator(expr_call);
    }
/*
    else if (tokenType == tok_pp___FUNCTION__)
    {
        MetaCParser_Match(self, tok_pp___FUNCTION__);
        result = AllocNewExpr(expr_string);
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

metac_expr_t* MetaCParser_ParsePostfixExpr(metac_parser_t* self,
                                                       metac_expr_t* left)
{
    metac_expr_t* result = 0;

    metac_token_t* peek = MetaCParser_PeekToken(self, 1);

    metac_token_enum_t peekTokenType = peek->TokenType;

    metac_location_t loc =
        MetaCLocationStorage_FromPtr(&self->LocationStorage, left->LocationIdx);

    if (peekTokenType == tok_plusplus)
    {
        MetaCParser_Match(self, peekTokenType);
        metac_expr_t* E1 = left;
        result = AllocNewExpr(expr_post_increment);
        result->E1 = E1;
        uint32_t hash = CRC32C_PLUSPLUS;
        result->Hash = CRC32C_VALUE(result->E1->Hash, hash);
    }
    else if (peekTokenType == tok_minusminus)
    {
        MetaCParser_Match(self, peekTokenType);

        metac_expr_t* E1 = left;
        result = AllocNewExpr(expr_post_decrement);
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

static inline metac_expr_t* ParseDotSpecialExpr(metac_parser_t* self,
                                                            metac_expr_kind_t k)
{
    metac_expr_t* result = 0;
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
            result = AllocNewExpr(k);
            uint32_t hash = CRC32C_VALUE(~0, k);
            result->E1 = MetaCParser_ParseExpr(self, expr_flags_none, 0);
            hash = CRC32C_VALUE(hash, result->E1->Hash);
            metac_location_t endLoc =
                MetaCLocationStorage_FromPtr(&self->LocationStorage, result->E1->LocationIdx);

            MetaCLocation_Expand(&loc, endLoc);
            result->Hash = hash;
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

static inline metac_expr_t* ParseUnaryDotExpr(metac_parser_t* self)
{
    metac_expr_t* result = 0;

    metac_token_t* peek;
    peek = MetaCParser_PeekToken(self, 1);
/*
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
                    result = ParseDotSpecialExpr(self, expr_dot_compiler);
                }
            } break;
            case context_key:
            {
                metac_identifier_ptr_t identifierPtr =
                    RegisterIdentifier(self, peek);
                if (self->SpecialNamePtr_Context.v == identifierPtr.v)
                {
                    MetaCParser_Match(self, tok_identifier);
                    result = ParseDotSpecialExpr(self, expr_dot_context);
                }
            } break;
            case target_key:
            {
                metac_identifier_ptr_t identifierPtr =
                    RegisterIdentifier(self, peek);
                if (self->SpecialNamePtr_Target.v == identifierPtr.v)
                {
                    MetaCParser_Match(self, tok_identifier);
                    result = ParseDotSpecialExpr(self, expr_dot_target);
                }
            } break;
        }
    }
*/
    if (peek->TokenType == tok_uint)
    {
        MetaCParser_Match(self, tok_uint);
        //bool static ParseFloat(const char** textP, int32_t* eatenCharsP, float* valueP)
        char vBuffer[32];
        const char* p = vBuffer;
        int32_t unused;
        float value;
        snprintf(vBuffer, sizeof(vBuffer), "0.%u", peek->ValueU32);
        ParseFloat(&p, &unused, &value);
        result = AllocNewExpr(expr_float);
        result->LocationIdx = MetaCLocationStorage_Store(&self->LocationStorage,
                                                         LocationFromToken(self, peek));
        result->ValueF23 = value;
        // result =
    }

    if (!result)
    {
        result = AllocNewExpr(expr_unary_dot);
#ifdef OLD_PARSER
        result->E1 = MetaCParser_ParseExpr(self, expr_flags_unary, result);
#else
        result->E1 = MetaCParser_ParseExpr2(self, expr_flags_unary);
#endif
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
#define CRC32C_HASH  0xe3069283

parse_expr_flags_t MetaCParser_TopExprFlags(metac_parser_t* self)
{
    parse_expr_flags_t result = expr_flags_none;
    const uint32_t flagsCount = self->ExprParser.ExprFlagsStackCount;
    if (flagsCount)
    {
        self->ExprParser.ExprFlagsStack[flagsCount - 1];
    }

    return result;
}


static inline metac_expr_t* ParseRunExpr(metac_parser_t* self, int vers)
{
    metac_expr_t* result = 0;

    uint32_t hash = run_key;
    bool isStmt = false;
    metac_token_t* peek = 0;

    MetaCParser_Match(self, tok_at);
    MetaCParser_Match(self, tok_identifier);
    result = AllocNewExpr(expr_run);
    peek = MetaCParser_PeekToken(self, 1);
    // if there is a { following the run expression
    // we need to scan for a ; before the closing }
    if (peek->TokenType == tok_lBrace)
    {
        uint32_t peekN = 2;
        while(peek && peek->TokenType != tok_rBrace)
        {
            peek = MetaCParser_PeekToken(self, peekN++);
            if (peek->TokenType == tok_semicolon)
            {
                isStmt = true;
            }
        }
    }

    if (isStmt)
    {
        metac_stmt_t* runStmt = MetaCParser_ParseStmt(self, 0, 0);
        // introduce function body expression
        assert(!"Not implemented yet");
    }
    else
    {
            result->E1 =
                MetaCParser_ParseExpr(self, MetaCParser_TopExprFlags(self), 0);
    }
    hash = CRC32C_VALUE(hash, result->E1->Hash);
    result->Hash = hash;

    return result;
}
#ifdef OLD_PARSER
metac_expr_t* MetaCParser_ParseUnaryExpr(metac_parser_t* self)
{
    metac_expr_t* result = 0;
    static const metac_location_t nullLoc = {0};
    parse_expr_flags_t eflags = MetaCParser_TopExprFlags(self);

    metac_token_t* currentToken = MetaCParser_PeekToken(self, 1);
    metac_token_enum_t tokenType =
        (currentToken ? currentToken->TokenType : tok_eof);
    metac_location_t loc = currentToken ? LocationFromToken(self, currentToken) : nullLoc;

    bool isPrimaryExp = false;

    if (tokenType == tok_dot)
    {
        MetaCParser_Match(self, tok_dot);
        result = ParseUnaryDotExpr(self);
    }
    else if (tokenType == tok_kw_typeof)
    {
        parse_expr_flags_t flags = cast(parse_expr_flags_t)
            ((eflags & expr_flags_pp) | expr_flags_typeof);

        MetaCParser_Match(self, tok_kw_typeof);
        result = AllocNewExpr(expr_typeof);
        //PushOperator(expr_typeof);
        MetaCParser_Match(self, tok_lParen);
        result->E1 = MetaCParser_ParseExpr(self, flags, 0);
        MetaCParser_Match(self, tok_rParen);
        //PopOperator(expr_typeof);
        result->Hash = CRC32C_VALUE(typeof_key, result->E1->Hash);

    }
    else if (tokenType == tok_kw_sizeof)
    {
        metac_token_t* nextToken;
        bool wasParen = false;
        parse_expr_flags_t flags = cast(parse_expr_flags_t)
            ((eflags & expr_flags_pp) | expr_flags_sizeof);

        MetaCParser_Match(self, tok_kw_sizeof);
        result = AllocNewExpr(expr_sizeof);
        nextToken = MetaCParser_PeekToken(self, 1);

        if (nextToken->TokenType == tok_lParen)
        {
            wasParen = true;
            MetaCParser_Match(self, tok_lParen);
        }

        result->E1 = MetaCParser_ParseExpr(self, flags, 0);
        if (wasParen)
        {
            MetaCParser_Match(self, tok_rParen);
        }
        result->Hash = CRC32C_VALUE(sizeof_key, result->E1->Hash);
    }
    else if (tokenType == tok_kw_assert)
    {
        metac_location_t startLoc = MetaCParser_CurrentLocation(self);

        metac_expr_t* assertValue = 0;
        metac_expr_t* assertMessage = 0;

        MetaCParser_Match(self, tok_kw_assert);
        result = AllocNewExpr(expr_assert);
        //PushOperator(expr_assert);
        MetaCParser_Match(self, tok_lParen);
        assertValue = MetaCParser_ParseExpr(self, expr_flags_none, 0);
        MetaCParser_Match(self, tok_rParen);
        if (assertValue->Kind == expr_comma)
        {
            // if the expression within the assert is a comma expression
            // we assert that the right hand side of the comma expr is a string.
            assertMessage = assertValue->E2;
            assertValue = assertValue->E1;
            if (assertMessage->Kind != expr_string)
            {
                metac_location_t loc = MetaCParser_CurrentLocation(self);
                MetaCLocation_Expand(&startLoc, loc);
                ParseError(startLoc, "the second argument of assert is supposed to be a string");
                return 0;
            }
        }
        //PopOperator(expr_assert);
        result->E1 = assertValue;
        result->Hash = CRC32C_VALUE(assert_key, result->E1->Hash);
    }
    else if (tokenType == tok_minus)
    {
        MetaCParser_Match(self, tok_minus);
        result = AllocNewExpr(expr_umin);
        //PushOperator(expr_addr);
        result->E1 = MetaCParser_ParseExpr(self,
            cast(parse_expr_flags_t)(expr_flags_unary | (eflags & expr_flags_pp)), 0);
        result->Hash = CRC32C_VALUE(CRC32C_MINUS, result->E1->Hash);
    }
    else if (tokenType == tok_minusminus)
    {
        MetaCParser_Match(self, tok_minusminus);
        result = AllocNewExpr(expr_decrement);
        //PushOperator(expr_addr);
        result->E1 = MetaCParser_ParseExpr(self,
            cast(parse_expr_flags_t)(expr_flags_unary | (eflags & expr_flags_pp)), 0);
        result->Hash = CRC32C_VALUE(CRC32C_MINUSMINUS, result->E1->Hash);
    }
    else if (tokenType == tok_plusplus)
    {
        MetaCParser_Match(self, tok_plusplus);
        result = AllocNewExpr(expr_increment);
        //PushOperator(expr_addr);
        result->E1 = MetaCParser_ParseExpr(self,
            cast(parse_expr_flags_t)(expr_flags_unary | (eflags & expr_flags_pp)), 0);
        result->Hash = CRC32C_VALUE(CRC32C_PLUSPLUS, result->E1->Hash);
    }
    else if (tokenType == tok_and)
    {
        MetaCParser_Match(self, tok_and);
        result = AllocNewExpr(expr_addr);
        //PushOperator(expr_addr);
        result->E1 = MetaCParser_ParseExpr(self, cast(parse_expr_flags_t)(expr_flags_unary | (eflags & expr_flags_pp)), 0);
        result->Hash = CRC32C_VALUE(CRC32C_AND, result->E1->Hash);
        //PushOperand(result);
        //PopOperator(expr_addr);
    }
    else if (tokenType == tok_star)
    {
        MetaCParser_Match(self, tok_star);
        result = AllocNewExpr(expr_deref);
        //PushOperator(expr_deref);
        result->E1 = MetaCParser_ParseExpr(self, cast(parse_expr_flags_t)(expr_flags_unary | (eflags & expr_flags_pp)), 0);
        result->Hash = CRC32C_VALUE(CRC32C_STAR, result->E1->Hash);
        //PushOperand(result);
    }
    else if (tokenType == tok_bang)
    {
        MetaCParser_Match(self, tok_bang);
        result = AllocNewExpr(expr_not);
        result->E1 = MetaCParser_ParseExpr(self,
            cast(parse_expr_flags_t)(expr_flags_unary | (eflags & expr_flags_pp)), 0);
        result->Hash = CRC32C_VALUE(CRC32C_BANG, result->E1->Hash);
    }
    else if (tokenType == tok_dollar)
    {
        MetaCParser_Match(self, tok_dollar);
        result = AllocNewExpr(expr_outer);
        result->E1 = MetaCParser_ParseExpr(self,
            cast(parse_expr_flags_t)(expr_flags_unary | (eflags & expr_flags_pp)), 0);
        result->Hash = CRC32C_VALUE(CRC32C_BANG, result->E1->Hash);
    }
    else if (tokenType == tok_hash)
    {
        MetaCParser_Match(self, tok_hash);
        result = AllocNewExpr(expr_stringize);
        result->E1 = MetaCParser_ParseExpr(self,
            cast(parse_expr_flags_t)(expr_flags_unary | (eflags & expr_flags_pp)), 0);
        result->Hash = CRC32C_VALUE(CRC32C_HASH, result->E1->Hash);
    }
    else if (tokenType == tok_tilde)
    {
        MetaCParser_Match(self, tok_tilde);
        result = AllocNewExpr(expr_compl);
        result->E1 = MetaCParser_ParseExpr(self,
            cast(parse_expr_flags_t)(expr_flags_unary | (eflags & expr_flags_pp)), 0);
        result->Hash = CRC32C_VALUE(
            crc32c_nozero(~0, "~", 1),
            result->E1->Hash
        );
    }
    else if (tokenType == tok_at)
    {
#define run_key 0x3809a6
        metac_token_t* peek = MetaCParser_PeekToken(self, 2);
        if (peek->TokenType == tok_identifier
         && peek->IdentifierKey == run_key)
        {
            result = ParseRunExpr(self, 1);
        }
    }
    else if (IsPrimaryExprToken(tokenType))
    {
        isPrimaryExp = true;
        result = MetaCParser_ParsePrimaryExpr(self, eflags);
    }
    else if (IsTypeToken(tokenType))
    {
        result = AllocNewExpr(expr_type);
        result->TypeExp = MetaCParser_ParseTypeDecl(self, 0, 0);
        result->Hash = CRC32C_VALUE(type_key, result->TypeExp->Hash);
    }
    else
    {
        if (tokenType != tok_eof && tokenType != tok_newline)
        {
            metac_location_t location =
                MetaCLocationStorage_FromPtr(&self->Lexer->LocationStorage, currentToken->LocationId);

            fprintf(stderr, "line: %d col: %d\n", location.StartLine, location.StartColumn);
        }
        fprintf(stderr, "Unexpected Token: %s\n", MetaCTokenEnum_toChars(tokenType));
        assert(0);
    }

    if (!isPrimaryExp)
    {
        metac_location_t endLoc =
            MetaCLocationStorage_FromPtr(&self->LocationStorage, result->E1->LocationIdx);

        MetaCLocation_Expand(&loc, endLoc);
    }

    result->LocationIdx =
        MetaCLocationStorage_Store(&self->LocationStorage, loc);

    metac_token_t* peek_post = MetaCParser_PeekToken(self, 1);
    metac_token_enum_t postTokenType =
        (peek_post ? peek_post->TokenType : tok_invalid);

    if (IsPostfixOperator(postTokenType))
    {
        result = MetaCParser_ParsePostfixExpr(self, result);
    }

    return result;
}
#endif

expr_argument_list_t MetaCParser_ParseArgumentList(metac_parser_t* self, parse_expr_flags_t flags)
{
    metac_location_t loc =
        LocationFromToken(self, MetaCParser_PeekToken(self, 0));

    metac_token_t* peekToken;
    expr_argument_list_t arguments = {(metac_expr_kind_t)0};
    expr_argument_t** nextArgument = &arguments.Arguments;
    uint32_t nArguments = 0;
    uint32_t hash = ~0;

    METAC_NODE(arguments.Arguments) = emptyNode;

    MetaCParser_Match(self, tok_lParen);
    peekToken = MetaCParser_PeekToken(self, 1);

    if (peekToken->TokenType == tok_rParen)
    {
        MetaCParser_Match(self, tok_rParen);
        return arguments;
    }
#ifndef OLD_PARSER
    MetaCParser_PushExprStackBottom(self, self->ExprParser.ExprStackCount);
    MetaCParser_PushOpStackBottom(self, self->ExprParser.OpStackCount);
    MetaCParser_PushOpenParens(self);
    MetaCParser_PushFlags(self, flags);
#endif
    for (;;)
    {
        nArguments++;
        assert((*nextArgument) == _emptyPointer);
        (*nextArgument) = cast(expr_argument_t*)AllocNewExpr(expr_argument);
#ifndef OLD_PARSER
        metac_expr_t* exp = MetaCParser_ParseExpr2(self, expr_flags_call);
#else
        metac_expr_t* exp = MetaCParser_ParseExpr(self, expr_flags_call, 0);
#endif
        ((*nextArgument)->Expr) = exp;
        hash = CRC32C_VALUE(hash, exp->Hash);
        (*nextArgument)->Hash = exp->Hash ^ CRC32C_PARENPAREN;
        (*nextArgument)->LocationIdx = exp->LocationIdx;
        nextArgument = &((*nextArgument)->Next);
        (*nextArgument) = (expr_argument_t*) _emptyPointer;


        if(MetaCParser_PeekMatch(self, tok_comma, 1))
        {
            MetaCParser_Match(self, tok_comma);
        }
        else
        {
            break;
        }
    }

    arguments.ArgumentCount = nArguments;
#ifndef OLD_PARSER
    MetaCParser_PopExprStackBottom(self);
    MetaCParser_PopOpStackBottom(self);
    MetaCParser_PopOpenParens(self);
    MetaCParser_PopFlags(self);
#endif
    if (arguments.ArgumentCount != 0)
    {
        arguments.Hash = hash;

        metac_token_t* rParen = MetaCParser_Match(self, tok_rParen);
        MetaCLocation_Expand(&loc, LocationFromToken(self, rParen));
        arguments.LocationIdx =
                MetaCLocationStorage_Store(&self->LocationStorage, loc);
    }

    return arguments;
}

#ifdef OLD_PARSER
metac_expr_t* MetaCParser_ParseBinaryExpr(metac_parser_t* self,
                                                      parse_expr_flags_t eflags,
                                                      metac_expr_t* left,
                                                      uint32_t min_prec)
{
    metac_expr_t* result = 0;

    metac_token_t* peekToken;
    metac_token_enum_t peekTokenType;
    metac_location_t loc =
        MetaCLocationStorage_FromPtr(&self->LocationStorage, left->LocationIdx);

    peekToken = MetaCParser_PeekToken(self, 1);
    peekTokenType = (peekToken ? peekToken->TokenType : tok_eof);
    // a * b + c * d
    // a * b | (a * b) + c |
    // here we pop the stack
/*
    if (peekTokenType == tok_lBracket)
    {
        MetaCParser_Match(self, tok_lBracket);

        metac_expr_t* E1 = left;
        result = AllocNewExpr(expr_index);
        result->E1 = E1;
        result->E2 = MetaCParser_ParseExpr(self, eflags, 0);

        MetaCParser_Match(self, tok_rBracket);
    }
    else
*/
/*
    if (peekTokenType == tok_lParen)
    {
        self->OpenParens++;
        MetaCParser_Match(self, tok_lParen);
        metac_expr_t* E1 = left;
        assert(peekToken);

        metac_token_t* peekToken = MetaCParser_PeekToken(self, 1);
        expr_argument_t* arguments = (expr_argument_t*) _emptyPointer;
        expr_argument_t** nextArgument = &arguments;
        uint32_t nArguments = 0;

        while (!MetaCParser_PeekMatch(self, tok_rParen, 1))
        {
            nArguments++;
            assert((*nextArgument) == _emptyPointer);

            (*nextArgument) = (expr_argument_t*)AllocNewExpr(expr_argument);
            ((*nextArgument)->Expr) = MetaCParser_ParseExpr(self, expr_flags_call, 0);
            nextArgument = &((*nextArgument)->Next);
            (*nextArgument) = (expr_argument_t*) _emptyPointer;
            if(MetaCParser_PeekMatch(self, tok_comma, 1))
            {
                MetaCParser_Match(self, tok_comma);
            }
        }
        metac_token_t* rParen = MetaCParser_Match(self, tok_rParen);
        self->OpenParens--;

        result = AllocNewExpr(expr_call);
        result->E1 = E1;
        result->E2 = (metac_expr_t*)arguments;
        MetaCLocation_Expand(&loc, LocationFromToken(self, rParen));
        //PopOperator(expr_call);
    }
    else*/ if (IsBinaryOperator(peekTokenType, eflags))
    {
        bool rhsIsArgs = false;
        metac_expr_kind_t expr_right;

        while(IsBinaryOperator(peekTokenType, eflags)
           && OpToPrecedence(BinExpTypeFromTokenType(peekTokenType)) > min_prec)
        {
            expr_right = BinExpTypeFromTokenType(peekTokenType);
            uint32_t opPrecedence = OpToPrecedence(expr_right);
            metac_token_t*  startTok = (expr_right == expr_call ? peekToken : MetaCParser_Match(self, peekTokenType));
            metac_location_t rhsLoc = LocationFromToken(self, startTok);
            metac_expr_t* rhs;

            if (expr_right == expr_index)
            {
                rhs = MetaCParser_ParseExpr(self, eflags, 0);
                metac_token_t* rBracket =
                    MetaCParser_Match(self, tok_rBracket);
                MetaCLocation_Expand(&rhsLoc,
                    LocationFromToken(self, rBracket));
            }
            else if (expr_right == expr_template_instance
                && MetaCParser_PeekMatch(self, tok_lParen, 1))
            {
                goto LparseArgumentList;
            }
            else if (expr_right == expr_call)
            {
            LparseArgumentList:
                rhs = (metac_expr_t*)MetaCParser_ParseArgumentList(self, expr_flags_call).Arguments;
                if ((metac_node_t)rhs != emptyPointer)
                {
                    rhsIsArgs = true;

                    MetaCLocation_Expand(&rhsLoc,
                        LocationFromIndex(self, rhs->LocationIdx.v));
                }
            }
            else
            {
                rhs = MetaCParser_ParseUnaryExpr(self);
            }
            peekToken = MetaCParser_PeekToken(self, 1);
            peekTokenType = (peekToken ? peekToken->TokenType : tok_eof);

            while(IsBinaryOperator(peekTokenType, eflags)
               && (OpToPrecedence(BinExpTypeFromTokenType(peekTokenType)) > opPrecedence))
            {
                rhs = MetaCParser_ParseBinaryExpr(self, eflags, rhs, opPrecedence + 0);
                peekToken = MetaCParser_PeekToken(self, 1);
                peekTokenType = (peekToken ? peekToken->TokenType : tok_eof);
            }

            result = AllocNewExpr(expr_right);
            result->E1 = left;
            result->E2 = rhs;
            if (rhs != emptyPointer)
            {
                result->Hash = CRC32C_VALUE(left->Hash, rhs->Hash);
                MetaCLocation_Expand(&rhsLoc,
                    MetaCLocationStorage_FromPtr(&self->LocationStorage, rhs->LocationIdx));
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
#endif
bool IsBinaryExp(metac_expr_kind_t kind)
{
    return ((kind >= FIRST_BINARY_EXP(TOK_SELF)) & (kind <= LAST_BINARY_EXP(TOK_SELF))
            | (kind == expr_index)
            | (kind == expr_call)
            | (kind == expr_template_instance) );
}

static bool IsUnaryExp(metac_expr_kind_t kind)
{
    switch(kind)
    {
        case expr_umin:
        case expr_not:
        case expr_compl:
        case expr_decrement:
        case expr_increment:
        case expr_post_decrement:
        case expr_post_increment:
        case expr_paren:
        case expr_cast:
        case expr_deref:
        case expr_addr:
        case expr_sizeof:
        case expr_typeof:
        case expr_inject:
        case expr_outer:
            return true;
        default:
            return false;
    }
}

bool IsBinaryAssignExp(metac_expr_kind_t kind)
{
   return (kind >= expr_add_ass && kind <= expr_rsh_ass);
}

bool IsPrefixExp(metac_expr_kind_t Kind)
{
    if (Kind == expr_increment || Kind == expr_decrement)
    {
        return true;
    }

    return false;
}

#ifdef OLD_PARSER
metac_expr_t* MetaCParser_ParseExpr(metac_parser_t* self,
                                    parse_expr_flags_t eflags,
                                    metac_expr_t* prev)
{
    metac_expr_t* result = 0;
    metac_token_t* currentToken = 0;
    metac_token_enum_t tokenType =
        (currentToken ? currentToken->TokenType : tok_invalid);
    metac_location_t loc =  {0};
    if (currentToken)
        loc = LocationFromToken(self, currentToken);

    // printf("ParseExpr -- currentToken: %s\n", MetaCTokenEnum_toChars(currentToken->TokenType));
    bool isDefined = false;

LParseExpTop:
    currentToken = MetaCParser_PeekToken(self, 1);
    tokenType =
        (currentToken ? currentToken->TokenType : tok_invalid);
    if (currentToken)
        loc = LocationFromToken(self, currentToken);

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

    if (IsPrimaryExprToken(tokenType))
    {
        result = MetaCParser_ParsePrimaryExpr(self, eflags);
    }
    else  if (!prev /*|| prec_left > prec_right*/)
    {
        result = MetaCParser_ParseUnaryExpr(self);
    }
    else
    {
        int32_t initialPrec = 0;
        if (prev->Kind == expr_unary_dot)
        {
            initialPrec = OpToPrecedence(prev->Kind);
        }
        result = MetaCParser_ParseBinaryExpr(self, eflags, prev, initialPrec);
    }

#if !defined(NO_PREPROCESSOR)
    if (isDefined)
    {
        if (result->Kind == expr_paren)
            result = result->E1;

        assert(result->Kind == expr_identifier);

        metac_expr_t* call = AllocNewExpr(expr_call);
        expr_argument_t* args = cast(expr_argument_t*) AllocNewExpr(expr_argument);
        metac_expr_t* definedIdExp = AllocNewExpr(expr_identifier);
        metac_location_t endLoc =
            MetaCLocationStorage_FromPtr(&self->LocationStorage, result->LocationIdx);
        MetaCLocation_Expand(&loc, endLoc);

        definedIdExp->IdentifierKey = defined_key;
        definedIdExp->IdentifierPtr = self->SpecialNamePtr_Defined;

        uint32_t hash = defined_key;
        call->LocationIdx = MetaCLocationStorage_Store(&self->LocationStorage, loc);
        call->Hash = CRC32C_VALUE(hash, result->Hash);
        args->Expr = result;
        args->Next = cast(expr_argument_t*) emptyPointer;

        call->E2 = cast(metac_expr_t*)args;
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
            result = MetaCParser_ParseBinaryExpr(self, eflags, result, min_prec);
            peekNext = MetaCParser_PeekToken(self, 1);
            tokenType = (peekNext ? peekNext->TokenType : tok_eof);
        }
        else if (IsPostfixOperator(tokenType))
        {
            result = MetaCParser_ParsePostfixExpr(self, result);
            if (!prev)
            {
                peekNext = MetaCParser_PeekToken(self, 1);
                tokenType = (peekNext ? peekNext->TokenType : tok_eof);
                if(IsBinaryOperator(tokenType, eflags))
                {
                    uint32_t prec = OpToPrecedence(result->Kind);
                    if (prec < min_prec)
                        return result;
                    result = MetaCParser_ParseBinaryExpr(self, eflags, result, min_prec);
                }
            }
        }
        else if (peekNext->TokenType == tok_lParen)
        {
            result = MetaCParser_ParseBinaryExpr(self, eflags, result, OpToPrecedence(expr_call));
        }
        else if (peekNext->TokenType == tok_bang)
        {
            result = MetaCParser_ParseBinaryExpr(self, eflags, result, OpToPrecedence(expr_template_instance));
        }
        else if (tokenType == tok_lBracket)
        {
            result = MetaCParser_ParseBinaryExpr(self, eflags, result, OpToPrecedence(expr_index));
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
            metac_expr_t* Econd = result;
            hash = CRC32C_VALUE(hash, Econd->Hash);
            metac_expr_t* E1 =
                MetaCParser_ParseExpr(self, expr_flags_none, 0);
            hash = CRC32C_VALUE(hash, E1->Hash);
            MetaCParser_Match(self, tok_colon);
            metac_expr_t* E2 =
                MetaCParser_ParseExpr(self, expr_flags_none, 0);
            hash = CRC32C_VALUE(hash, E2->Hash);
            result = AllocNewExpr(expr_ternary);
            result->E1 = E1;
            result->E2 = E2;
            result->Econd = Econd;
            result->Hash = hash;
            result->LocationIdx = MetaCLocationStorage_Store(&self->LocationStorage,
                MetaCLocationStorage_FromPair(&self->LocationStorage,
                                              result->Econd->LocationIdx.v, result->E2->LocationIdx.v));
        }

        //else assert(!"Stray Input");
    }

    assert(result->Hash != 0);
LreturnExp:
    return result;
}
#endif
