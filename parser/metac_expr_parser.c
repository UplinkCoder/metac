#include "../parser/metac_expr_parser.h"

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
        MetaCExprKind_toChars( cast(metac_expr_kind_t)t ));
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
    // sizeof is possibly special
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
#define run_key 0x3809a6
    else if (tokenType == tok_at)
    {
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

void MetaCParser_PushOpStackBottom(metac_parser_t* self, uint32_t bottom)
{
    ARENA_ARRAY_ADD(self->ExprParser.OpStackBottomStack, bottom);
}

void MetaCParser_PopOpStackBottom(metac_parser_t* self)
{
    self->ExprParser.OpStackBottomStackCount--;
}

uint32_t MetaCParser_TopOpStackBottom(metac_parser_t* self)
{
    return (self->ExprParser.OpStackBottomStackCount ?
        self->ExprParser.OpStackBottomStack[
            self->ExprParser.OpStackBottomStackCount - 1
        ] : 0);
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
    else if (exp == expr_arrow || exp == expr_dot)
    {
        return 31;
    }
    else if (exp == expr_umin
          || exp == expr_unary_dot
          || exp == expr_sizeof
          || exp == expr_not)
    {
        return 32;
    }
    else if (exp == expr_paren
          || exp == expr_signed_integer
          || exp == expr_string
          || exp == expr_float
          || exp == expr_identifier
          || exp == expr_char
          || exp == expr_tuple
          || exp == expr_type)
    {
        return 33;
    }

    fprintf(stderr, "There's no precedence for %s\n", MetaCExprKind_toChars(exp));
    assert(0);
    return 0;
}

static inline bool IsPrimaryExprToken(metac_token_enum_t tokenType)
{
#ifdef TYPE_EXP
    if (IsTypeToken(tokenType) && tokenType != tok_star)
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
            && tok != tok_lBracket)
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

    if (tokenType == tok_lParen && CouldBeCast(self, tokenType))
    {
        // printf("going to parse cast\n");
        hash = cast_key;
        result = AllocNewExpr(expr_cast);
        //typedef unsigned int b;
        metac_token_t* lParen = MetaCParser_Match(self, tok_lParen);
        metac_type_modifiers typeModifiers = ParseTypeModifiers(self);

        if (IsTypeToken(MetaCParser_PeekToken(self, 1)->TokenType))
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
        MetaCLocation_Expand(&loc, self->LocationStorage.Locations[result->CastExp->LocationIdx - 4]);
        result->Hash = hash;
    }
#ifdef TYPE_EXP
    else if (CouldBeType(self, tokenType, flags))
    {
        decl_type_t* type =
            MetaCParser_ParseTypeDecl(self, 0, 0);
        result = AllocNewExpr(expr_type);
        result->TypeExp = type;
        MetaCLocation_Expand(&loc,
            self->LocationStorage.Locations[result->TypeExp->LocationIdx - 4]);
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
        // result = GetOrAddStringLiteral(_string_table, currentToken);
        MetaCParser_Match(self, tok_string);
        result = AllocNewExpr(expr_string);
        result->StringPtr = RegisterString(self, currentToken);
        result->StringKey = currentToken->StringKey;
        result->Hash = currentToken->StringKey;
        //PushOperand(result);
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
    else if (tokenType == tok_lParen)
    {
        self->ExprParser.OpenParens++;
        MetaCParser_Match(self, tok_lParen);
        result = AllocNewExpr(expr_paren);
        {
            if (!MetaCParser_PeekMatch(self, tok_rParen, 1))
            {
                result->E1 = MetaCParser_ParseExpr(self, expr_flags_none, 0);
                // printf("E1: %s\n", MetaCExprKind_toChars(result->E1->Kind));
            }
            else
            {
                METAC_NODE(result->E1) = emptyNode;
            }
        }
        //PushOperator(expr_paren);
        result->Hash = CRC32C_VALUE(crc32c_nozero(~0, "()", 2), result->E1->Hash);
        //PushOperand(result);
        metac_token_t* endParen =
            MetaCParser_Match(self, tok_rParen);
        MetaCLocation_Expand(&loc, LocationFromToken(self, endParen));
        self->ExprParser.OpenParens--;
        //PopOperator(expr_paren);
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
        self->LocationStorage.Locations[left->LocationIdx - 4];

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
            metac_location_t endLoc = self->LocationStorage.Locations[result->E1->LocationIdx - 4];
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
    if (!result)
    {
        result = AllocNewExpr(expr_unary_dot);
        result->E1 = MetaCParser_ParseExpr(self, expr_flags_unary, 0);
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

parse_expr_flags_t MetaCParser_CurrentExprFlags(metac_parser_t* self)
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
        if (vers == 2)
            result->E1 = MetaCParser_ParseExpr2(self, MetaCParser_CurrentExprFlags(self));
        else
            result->E1 =
                MetaCParser_ParseExpr(self, MetaCParser_CurrentExprFlags(self), 0);
    }
    hash = CRC32C_VALUE(hash, result->E1->Hash);
    result->Hash = hash;

    return result;
}

metac_expr_t* MetaCParser_ParseUnaryExpr(metac_parser_t* self)
{
    metac_expr_t* result = 0;
    static const metac_location_t nullLoc = {0};
    parse_expr_flags_t eflags = MetaCParser_CurrentExprFlags(self);

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
    else if (tokenType == tok_kw_eject)
    {
        MetaCParser_Match(self, tok_kw_eject);
        result = AllocNewExpr(expr_eject);
        //PushOperator(expr_eject);
        result->E1 = MetaCParser_ParseExpr(self, expr_flags_none, 0);
        result->Hash = CRC32C_VALUE(eject_key, result->E1->Hash);
        //PushOperand(result);
        //PopOperator(expr_eject);
    }
    else if (tokenType == tok_kw_inject)
    {
        MetaCParser_Match(self, tok_kw_inject);
        result = AllocNewExpr(expr_inject);
        //PushOperator(expr_inject);
        result->E1 = MetaCParser_ParseExpr(self, expr_flags_none, 0);
        result->Hash = CRC32C_VALUE(inject_key, result->E1->Hash);
        //PushOperand(result);
        //PopOperator(expr_inject);
    }
    else if (tokenType == tok_kw_typeof)
    {
        MetaCParser_Match(self, tok_kw_typeof);
        result = AllocNewExpr(expr_typeof);
        //PushOperator(expr_typeof);
        metac_token_t* nextToken = MetaCParser_PeekToken(self, 1);
        if (!nextToken || nextToken->TokenType != tok_lParen)
        {
            ParseError(loc, "Expected typeof to be followed by '('");
        }

        metac_expr_t* parenExp = MetaCParser_ParseExpr(self, expr_flags_none, 0);
        //PopOperator(expr_typeof);
        assert(parenExp->Kind == expr_paren);
        result->E1 = parenExp->E1;
        result->Hash = CRC32C_VALUE(typeof_key, result->E1->Hash);

    }
    else if (tokenType == tok_kw_sizeof)
    {
        MetaCParser_Match(self, tok_kw_sizeof);
        result = AllocNewExpr(expr_sizeof);
        metac_token_t* nextToken = MetaCParser_PeekToken(self, 1);
        bool wasParen = false;
        if (nextToken->TokenType == tok_lParen)
        {
            wasParen = true;
            MetaCParser_Match(self, tok_lParen);
        }
        parse_expr_flags_t flags = cast(parse_expr_flags_t)
            ((eflags & expr_flags_pp) | expr_flags_sizeof);

        result->E1 = MetaCParser_ParseExpr(self, flags, 0);
        if (wasParen)
        {
            MetaCParser_Match(self, tok_rParen);
        }
        result->Hash = CRC32C_VALUE(sizeof_key, result->E1->Hash);
    }
    else if (tokenType == tok_kw_assert)
    {
        MetaCParser_Match(self, tok_kw_assert);
        result = AllocNewExpr(expr_assert);
        //PushOperator(expr_assert);
        metac_token_t* nextToken = MetaCParser_PeekToken(self, 1);
        if (!nextToken || nextToken->TokenType != tok_lParen)
        {
            ParseError(loc, "Expected assert to be followed by '('");
        }
        metac_expr_t* parenExp = MetaCParser_ParseExpr(self, expr_flags_none, 0);
        //PopOperator(expr_assert);
        assert(parenExp->Kind == expr_paren);
        result->E1 = parenExp->E1;
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
    else
    {
        if (tokenType != tok_eof && tokenType != tok_newline)
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
        result = MetaCParser_ParsePostfixExpr(self, result);
    }

    return result;
}

expr_argument_t* MetaCParser_ParseArgumentList(metac_parser_t* self)
{
    metac_location_t loc =
        LocationFromToken(self, MetaCParser_PeekToken(self, 0));

    metac_token_t* peekToken = MetaCParser_PeekToken(self, 1);
    expr_argument_t* arguments = (expr_argument_t*) _emptyPointer;
    expr_argument_t** nextArgument = &arguments;
    uint32_t nArguments = 0;
    uint32_t hash = ~0;

    if (peekToken->TokenType == tok_rParen)
    {
        MetaCParser_Match(self, tok_rParen);
        return arguments;
    }

    MetaCParser_PushExprStackBottom(self, self->ExprParser.ExprStackCount);
    MetaCParser_PushOpStackBottom(self, self->ExprParser.OpStackCount);

    for (;;)
    {
        nArguments++;
        assert((*nextArgument) == _emptyPointer);
        (*nextArgument) = (expr_argument_t*)AllocNewExpr(expr_argument);
        metac_expr_t* exp = MetaCParser_ParseExpr2(self, expr_flags_call);
        ((*nextArgument)->Expr) = exp;
        assert(exp->Hash);
        hash = CRC32C_VALUE(hash, exp->Hash);
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

    MetaCParser_PopExprStackBottom(self);
    MetaCParser_PopOpStackBottom(self);

    if (arguments != emptyPointer)
    {
        arguments->Hash = hash;
        metac_token_t* rParen = MetaCParser_PeekToken(self, 0);
        MetaCLocation_Expand(&loc, LocationFromToken(self, rParen));
        arguments->LocationIdx =
            MetaCLocationStorage_Store(&self->LocationStorage, loc);
    }

    return arguments;
}

metac_expr_t* MetaCParser_ParseBinaryExpr(metac_parser_t* self,
                                                      parse_expr_flags_t eflags,
                                                      metac_expr_t* left,
                                                      uint32_t min_prec)
{
    metac_expr_t* result = 0;

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
           && OpToPrecedence(BinExpTypeFromTokenType(peekTokenType)) >= min_prec)
        {
            expr_right = BinExpTypeFromTokenType(peekTokenType);
            uint32_t opPrecedence = OpToPrecedence(expr_right);
            metac_token_t*  startTok = MetaCParser_Match(self, peekTokenType);
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
                MetaCParser_Match(self, tok_lParen);
                goto LparseArgumentList;
            }
            else if (expr_right == expr_call)
            {
            LparseArgumentList:
                rhs = (metac_expr_t*)MetaCParser_ParseArgumentList(self);
                if ((metac_node_t)rhs != emptyPointer)
                    rhsIsArgs = true;

                metac_token_t* rParen =
                    MetaCParser_Match(self, tok_rParen);
                MetaCLocation_Expand(&rhsLoc,
                    LocationFromToken(self, rParen));
            }
            else
            {
                rhs = MetaCParser_ParseUnaryExpr(self);
            }
            peekToken = MetaCParser_PeekToken(self, 1);
            peekTokenType = (peekToken ? peekToken->TokenType : tok_eof);

            while(IsBinaryOperator(peekTokenType, eflags)
               && (OpToPrecedence(BinExpTypeFromTokenType(peekTokenType)) >= opPrecedence))
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

static bool IsBinaryExp(metac_expr_kind_t kind)
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
            return true;
        default:
            return false;
    }
}



bool IsBinaryAssignExp(metac_expr_kind_t kind)
{
   return (kind >= expr_add_ass && kind <= expr_rsh_ass);
}

void MetaCParser_PushExpr(metac_parser_t* self, metac_expr_t *e)
{
    // printf("Pushing Expr: %s\n", MetaCPrinter_PrintExpr(&self->DebugPrinter, e));
    ARENA_ARRAY_ADD(self->ExprParser.ExprStack, e);
}

void MetaCParser_PushExprStackBottom(metac_parser_t* self, uint32_t bottom)
{
    ARENA_ARRAY_ADD(self->ExprParser.ExprStackBottomStack, bottom);
}

void MetaCParser_PopExprStackBottom(metac_parser_t* self)
{
    self->ExprParser.ExprStackBottomStackCount--;
}

uint32_t MetaCParser_TopExprStackBottom(metac_parser_t* self)
{
    return (self->ExprParser.ExprStackBottomStackCount ?
        self->ExprParser.ExprStackBottomStack[
            self->ExprParser.ExprStackBottomStackCount - 1
        ] : 0);
}

metac_expr_t* MetaCParser_TopExpr(metac_parser_t* self)
{
    const uint32_t stackCount = self->ExprParser.ExprStackCount;
    metac_expr_t* result = 0;

    if (stackCount)
    {
        result = self->ExprParser.ExprStack[stackCount - 1];
    }
    return result;
}

metac_expr_t* MetaCParser_PopExpr(metac_parser_t* self)
{
    metac_expr_t* result = 0;

    metac_expr_t* top = MetaCParser_TopExpr(self);
/*
    printf("Poping ExprStack(%d): %s\n",
        self->ExprParser.ExprStackCount,
        top ? MetaCPrinter_PrintExpr(&self->DebugPrinter, top) : "null");
*/
    result = self->ExprParser.ExprStack[--self->ExprParser.ExprStackCount];

    return result;
}

void MetaCParser_PushOp(metac_parser_t* self, metac_expr_kind_t op)
{
    // printf("Pushing Op: %s\n", MetaCExprKind_toChars(op));
    ARENA_ARRAY_ADD(self->ExprParser.OpStack, op);
}

metac_expr_kind_t MetaCParser_PopOp(metac_parser_t* self)
{
    metac_expr_kind_t result = self->ExprParser.OpStack[--self->ExprParser.OpStackCount];
    // printf("Popping Op: %s\n", MetaCExprKind_toChars(result));

    return result;
}

metac_expr_kind_t MetaCParser_TopOp(metac_parser_t* self)
{
    const uint32_t stackCount = self->ExprParser.OpStackCount;
    metac_expr_kind_t result = expr_invalid;

    if (stackCount)
    {
        result = self->ExprParser.OpStack[stackCount - 1];
    }
    return result;
}

metac_expr_t* MetaCParser_ApplyOp(metac_parser_t* self, metac_expr_kind_t op)
{
    metac_expr_t* e = 0;
    assert(op != expr_invalid);
    e = AllocNewExpr(op);

    if(IsBinaryExp(op))
    {
        e->E2 = MetaCParser_PopExpr(self);
        e->E1 = MetaCParser_PopExpr(self);
        if (op == expr_call && ((void*)e->E2) == emptyPointer)
        {
            e->Hash = e->E1->Hash;
        }
        else
        {
            e->Hash = CRC32C_VALUE(e->E1->Hash, e->E2->Hash);
        }

    }
    else if (IsUnaryExp(op) || op == expr_assert)
    {
        e->E1 = MetaCParser_PopExpr(self);
        e->Hash = e->E1->Hash;
    }
    else if (op == expr_ternary)
    {
        e->E2 = MetaCParser_PopExpr(self);
        e->E1 = MetaCParser_PopExpr(self);
        e->Econd = MetaCParser_PopExpr(self);
    }
    else
    {
        assert(0);
    }
    MetaCParser_PushExpr(self, e);

    return e;
}


metac_expr_t* MetaCParser_ApplyOpsUntil(metac_parser_t* self, metac_expr_kind_t op)
{
    metac_expr_t* result = 0;

    for (;;)
    {
        uint32_t opPrec = OpToPrecedence(op);
        metac_expr_kind_t leftOp = MetaCParser_TopOp(self);
        uint32_t leftPrec = (leftOp != expr_invalid ? OpToPrecedence(leftOp) : 0);

        if (opPrec > leftPrec /*|| op == leftOp*/)
        {
            break;
        }

        result = MetaCParser_ApplyOp(self, leftOp);
        MetaCParser_PopOp(self);
    }

    return result;
}

bool IsPrefixExp(metac_expr_kind_t Kind)
{
    if (Kind == expr_increment || Kind == expr_decrement)
    {
        return true;
    }

    return false;
}

metac_expr_t* MetaCParser_ParseExpr2(metac_parser_t* self, parse_expr_flags_t flags)
{
    metac_expr_t* result = 0;

    metac_token_t* currentToken = 0;
    metac_token_enum_t tokenType = tok_invalid;
    metac_expr_kind_t leftOp = expr_invalid;
    metac_expr_kind_t op = expr_invalid;
    uint32_t prec = 0;
    uint32_t opPrec = 0;
    parse_expr_flags_t eflags = flags;
    metac_location_t loc =  {0};


    MetaCParser_PushExprStackBottom(self, self->ExprParser.ExprStackCount);
    MetaCParser_PushOpStackBottom(self, self->ExprParser.OpStackCount);

    currentToken = MetaCParser_PeekToken(self, 1);
    if (currentToken && currentToken->TokenType == tok_dot)
    {
        MetaCParser_Match(self, tok_dot);
        result = ParseUnaryDotExpr(self);
        MetaCParser_PushExpr(self, result);
    }

    for(;;)
    {
        assert (self->ExprParser.OpStackCount < 20);
        assert (self->ExprParser.ExprStackCount < 2000);

        currentToken = MetaCParser_PeekToken(self, 1);
        if (currentToken)
            loc = LocationFromToken(self, currentToken);
        tokenType =
            (currentToken ? currentToken->TokenType : tok_invalid);

        //TODO nested ternary expressions
        if (eflags & expr_flags_ternary)
        {
            if (tokenType == tok_colon)
            {
                MetaCParser_Match(self, tok_colon);
                // MetaCParser_ApplyOpsUntil(self, expr_ternary);
                U32(eflags) &= (~expr_flags_ternary);
                continue;
            }
        }

        if (IsPrimaryExprToken(tokenType))
        {
            // special case for lParen since it might be a call
            if (tokenType == tok_lParen)
            {
                uint32_t n_exprs =
                    (self->ExprParser.ExprStackCount -
                     MetaCParser_TopExprStackBottom(self));

                self->ExprParser.OpenParens++;
                // if lParen is a encountered with a non-empty
                // expression stack it's likely a call.
                if (n_exprs != 0
                    && !(eflags & expr_flags_binary))
                {
                    goto LParseCall;
                }
            }
            metac_expr_t* e = MetaCParser_ParsePrimaryExpr(self, expr_flags_none);
            MetaCParser_PushExpr(self, e);
            U32(eflags) &= ~expr_flags_binary;
            op = e->Kind;
            goto LParsePostfix;
        }
        else if (tokenType == tok_question)
        {
            leftOp = MetaCParser_TopOp(self);
            prec  = ((leftOp == expr_invalid) ? 0 : OpToPrecedence(leftOp));
            MetaCParser_Match(self, tok_question);
            op = expr_ternary;
            U32(eflags) |= expr_flags_ternary;
            opPrec = OpToPrecedence(op);
            if (opPrec > prec)
            {
                // We are pushing the operator below
                // So there's nothing to do here
            }
            else
            {
                MetaCParser_ApplyOpsUntil(self, op);
            }
            MetaCParser_PushOp(self, expr_ternary);
            continue;
        }
        else if (MetaCParser_TopExpr(self) != 0
            && IsBinaryOperator(tokenType, eflags))
LParseBinary:
        {
            int32_t oldStackCount = self->ExprParser.ExprStackCount;
            op = BinExpTypeFromTokenType(tokenType);
            {
                leftOp = MetaCParser_TopOp(self);
                prec = (leftOp != expr_invalid) ? OpToPrecedence(leftOp) : 0;
                opPrec = OpToPrecedence(op);
                assert(op != expr_invalid);

                if ((opPrec > prec) || (eflags & expr_flags_ternary))
                {
                    // if the precedence of the the operator
                    // is higher than what's on the top of the stack.
                    // push the operator to the stack and move on
                    MetaCParser_Match(self, tokenType);
                    MetaCParser_PushOp(self, op);
                    U32(eflags) |= expr_flags_binary;
                }
                else
LPopBinaryOp:
                {
                    // if not pop 2 of the expressions on the exp stack
                    // and combine it with the top of the opStack to from
                    // an new binary expression node
                    //leftOp = MetaCParser_PopOp(self);
                    //MetaCParser_ApplyOp(self, leftOp);
                    MetaCParser_ApplyOpsUntil(self, op);
                    // U32(eflags) &= ~expr_flags_binary;
                }
            }
//            assert(self->ExprParser.ExprStackCount < oldStackCount);
        }
        else if ((op = UnaExpTypeFromTokenType(tokenType, 0)) != expr_invalid)
        {
            leftOp = MetaCParser_TopOp(self);
            prec = (leftOp != expr_invalid) ? OpToPrecedence(leftOp) : 0;
            opPrec = OpToPrecedence(op);
            U32(eflags) &= (~expr_flags_binary);

            // we need more speical casing
/*
            printf("topOp('%s').prec: %d\n", MetaCExprKind_toChars(leftOp), prec);
            printf("eKind('%s').prec: %d\n", MetaCExprKind_toChars(eKind), ePrec);
*/
            if (opPrec > prec)
            {
                // if the percence of the the operator
                // is higher than what's on the top of the stack.
                //printf("eKind prec higher. Pushing up\n");
                MetaCParser_Match(self, tokenType);
                MetaCParser_PushOp(self, op);
            }
            else
LPopUnaryOp:
            {
                leftOp = MetaCParser_PopOp(self);
                metac_expr_t* e = MetaCParser_ApplyOp(self, leftOp);
                U32(eflags) &= ~expr_flags_binary;
            }
        }
        else if(tokenType == tok_rParen)
        {
            if (self->ExprParser.OpenParens != 0)
            {
                MetaCParser_Match(self, tok_rParen);
                --self->ExprParser.OpenParens;
            }
            else
            {
                goto LTerminate;
            }
        }
        else if (tokenType == tok_rBracket)
        {
            if (self->ExprParser.LBracketCount != 0)
            {
                MetaCParser_ApplyOpsUntil(self, expr_index);
                //assert(MetaCParser_TopOp(self) == expr_index);
                MetaCParser_Match(self, tok_rBracket);
                --self->ExprParser.LBracketCount;
                goto LParsePostfix;
            }
            else
            {
                goto LTerminate;
            }
            //goto LPopBinaryOp;
        }
        else
        // Termination condition return top of stack
LTerminate:
        {
            uint32_t n_exprs =
                (self->ExprParser.ExprStackCount - MetaCParser_TopExprStackBottom(self));
            uint32_t n_ops =
                (self->ExprParser.OpStackCount - MetaCParser_TopOpStackBottom(self));

            if (n_ops >= 1)
            {
                while (n_ops-- > 0)
                {
                    metac_expr_kind_t op = MetaCParser_PopOp(self);
                    MetaCParser_ApplyOp(self, op);
                }
                n_exprs =
                    (self->ExprParser.ExprStackCount - MetaCParser_TopExprStackBottom(self));
            }

            if (n_exprs == 1)
            {
                MetaCParser_PopExprStackBottom(self);
                MetaCParser_PopOpStackBottom(self);
                result = MetaCParser_PopExpr(self);
            }
            break;
        }
        // postfixn'
LParsePostfix:
        {
            currentToken = MetaCParser_PeekToken(self, 1);
            if (currentToken)
                loc = LocationFromToken(self, currentToken);
            tokenType =
                (currentToken ? currentToken->TokenType : tok_invalid);

            if (IsPostfixOperator(tokenType) && op != expr_invalid &&
                !IsPrefixExp(op))
            {
                leftOp = MetaCParser_TopOp(self);
                prec = (leftOp != expr_invalid) ? OpToPrecedence(leftOp) : 0;

                op = expr_invalid;

                if (tokenType == tok_plusplus)
                {
                    op = expr_post_increment;
                }
                else if (tokenType == tok_minusminus)
                {
                    op = expr_post_decrement;
                }
                else if (tokenType == tok_question)
                {
                    op = expr_ternary;
                    U32(eflags) |= expr_flags_ternary;
                }
                opPrec = OpToPrecedence(op);
                if (opPrec > prec)
                {
                    MetaCParser_Match(self, tokenType);
                    MetaCParser_PushOp(self, op);
                }
                else
                {
                    MetaCParser_Match(self, tokenType);
                    leftOp = MetaCParser_PopOp(self);
                    metac_expr_t *e1 =
                        MetaCParser_ApplyOp(self, leftOp);
                    MetaCParser_PushOp(self, op);
                }
            }
        }

        while (false)
LParseCall:
        {
            expr_argument_t* args = 0;
            metac_token_t* rParen;

            leftOp = MetaCParser_TopOp(self);
            prec = (leftOp != expr_invalid) ? OpToPrecedence(leftOp) : 0;

            MetaCParser_Match(self, tok_lParen);
            op = expr_call;
            opPrec = OpToPrecedence(op);
            args = MetaCParser_ParseArgumentList(self);
//            rParen = MetaCParser_Match(self, tok_rParen);

            if (opPrec > prec)
            {
                MetaCParser_PushOp(self, op);
                MetaCParser_PushExpr(self, (metac_expr_t*) args);
            }
            else
            {
                MetaCParser_ApplyOpsUntil(self, op);
                metac_expr_t *call = AllocNewExpr(expr_call);
                call->E1 = MetaCParser_PopExpr(self);
                call->E2 = (metac_expr_t*) args;
                MetaCParser_PushExpr(self, call);
            }
            goto LParsePostfix;

/*
            MetaCLocation_Expand(&rhsLoc,
                LocationFromToken(self, rParen));
*/
        }
    }

    return result;
}

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
        result = MetaCParser_ParseBinaryExpr(self, eflags, prev, 0);
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
        metac_location_t endLoc = self->LocationStorage.Locations[result->LocationIdx - 4];
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
                                              result->Econd->LocationIdx, result->E2->LocationIdx));
        }

        //else assert(!"Stray Input");
    }

    assert(result->Hash != 0);
LreturnExp:
    return result;
}
