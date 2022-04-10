#include "metal_lexer.h"

const char* MetalTokenEnum_toChars(metal_token_enum_t tok)
{
    const char* result;

    if (tok == tok_invalid)
        result = "invalid";

    if (tok == tok_identifier)
        result = "identifier";

    if (tok == tok_unsignedNumber)
        result = "unsigned number";

    if (tok == tok_stringLiteral)
        result = "string";

    if (tok == tok_lParen)
        result = "(";
    if (tok == tok_rParen)
        result = ")";
    if (tok == tok_lBrace)
        result = "{";
    if (tok == tok_rBrace)
        result = "}";
    if (tok == tok_lBracket)
        result = "[";
    if (tok == tok_rBracket)
        result = "]";

    if (tok == tok_comma)
        result = ",";
    if (tok == tok_dot)
        result = ".";
    if (tok == tok_dotdot)
        result = "..";
    if (tok == tok_comment_begin)
        result = "/*";
    if (tok == tok_comment_end)
        result = "*/";
    if (tok == tok_comment_single)
        result = "//";

    if (tok == tok_bang)
        result = "!";

    if (tok == tok_minus)
        result = "-";
    if (tok == tok_plus)
        result = "+";
    if (tok == tok_div)
        result = "/";
    if (tok == tok_star)
        result ="*";
    if (tok == tok_addr)
        result = "&";

    if (tok == tok_cat)
        result = "~";
    if (tok == tok_cat_ass)
        result = "~=";

    if (tok == tok_semicolon)
        result =";";
    if (tok == tok_colon)
        result = ":";
    if (tok == tok_dollar)
        result = "$";
    if (tok == tok_assign)
        result = "=";
    if (tok == tok_equalsEquals)
        result = "==";
    if (tok == tok_lessThan)
        result =  "<";
    if (tok == tok_lessEqual)
        result = "<=";
    if (tok == tok_full_slice)
        result = "[]";

    if (tok == tok_greaterThan)
        result = ">";
    if (tok == tok_greaterEqual)
        result = ">=";

    if (tok == tok_spaceShip)
        result = "<=>";

    if (tok == tok_kw_struct)
        result = "struct";
    if (tok == tok_kw_union)
        result = "union";
    if (tok == tok_kw_type)
        result = "type";
    if (tok == tok_kw_enum)
        result = "enum";
    if (tok == tok_kw_inject)
        result = "inject";
    if (tok == tok_kw_eject)
        result ="eject";
    if (tok == tok_kw_assert)
        result = "assert";
    if (tok == tok_kw_typedef)
        result = "typedef";

    if (tok == tok_eof)
        result = "EOF";

    if (tok == tok_max)
      result = "max";

    return result;
}
