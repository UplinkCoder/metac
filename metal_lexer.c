#include "metal_lexer.h"

static metal_token_enum_t lexFixedToken(const char _chrs[7]) {
    switch(_chrs[0]) {
        default :
            return tok_invalid;

        case '!' :
            return tok_bang;

        case '"' :
            return tok_quote;


        case '$' :
            switch (_chrs[1]) {
            default :
            return tok_dollar;
            case '(' :
                return tok_dollar_paren;
            }

        case '&' :
            return tok_addr;

        case '(' :
            return tok_lParen;

        case ')' :
            return tok_rParen;


        case '*' :
            switch (_chrs[1]) {
            default :
            return tok_star;
            case '/' :
                return tok_comment_end;
            }

        case '+' :
            return tok_plus;

        case ',' :
            return tok_comma;

        case '-' :
            return tok_minus;

        case '.' :
            switch (_chrs[1]) {
            default :
            return tok_dot;
            case '.' :
                return tok_dotdot;
            }


        case '/' :
            switch (_chrs[1]) {
            default :
            return tok_div;
            case '/' :
                return tok_comment_single;
            case '*' :
                return tok_comment_begin;
            }

        case ':' :
            return tok_colon;

        case ';' :
            return tok_semicolon;



        case '<' :
            switch (_chrs[1]) {
            default :
            return tok_lessThan;
            case '=' :
                switch (_chrs[2]) {
                default :
                    return tok_lessEqual;
                case '>' :
                    return tok_spaceShip;
                }
            }

        case '=' :
            switch (_chrs[1]) {
            default :
                return tok_assign;
            case '=' :
                return tok_equalsequals;
            }


        case '>' :
            switch (_chrs[1]) {
            default :
            return tok_greaterThan;
            case '=' :
                return tok_greaterEqual;
            }

        case '[' :
            switch (_chrs[1]) {
            default :
            return tok_lBracket;
            case ']' :
                return tok_full_slice;
            }

        case ']' :
            return tok_rBracket;

        case 'a' :
            switch (_chrs[1]) {
            default : return tok_invalid;
            case 's' :
                switch (_chrs[2]) {
                default : return tok_invalid;
                case 's' :
                    switch (_chrs[3]) {
                    default : return tok_invalid;
                    case 'e' :
                        switch (_chrs[4]) {
                        default : return tok_invalid;
                        case 'r' :
                            switch (_chrs[5]) {
                            default : return tok_invalid;
                            case 't' :
                                return tok_kw_assert;
                            }
                        }
                    }
                }
            }

        case 'e' : {
            switch (_chrs[1]) {
            default: return tok_invalid;
            case 'n' : {
                switch (_chrs[2]) {
                default : return tok_invalid;
                case 'u' :
                    switch (_chrs[3]) {
                    default : return tok_invalid;
                    case 'm' :
                        return tok_kw_enum;
                    }
                }
            }
            case 'j' :
                switch (_chrs[2]) {
                default : return tok_invalid;
                case 'e' :
                    switch (_chrs[3]) {
                    default : return tok_invalid;
                    case 'c' :
                        switch(_chrs[4]) {
                        default : return tok_invalid;
                        case 't' :
                            return tok_kw_eject;
                        }
                    }
                }
            }
        }

        case 'i' :
            switch (_chrs[1]) {
            default : return tok_invalid;
            case 'n' :
                switch (_chrs[2]) {
                default : return tok_invalid;
                case 'j' :
                    switch (_chrs[3]) {
                    default : return tok_invalid;
                    case 'e' :
                        switch (_chrs[4]) {
                        default : return tok_invalid;
                        case 'c' :
                            switch (_chrs[5]) {
                            default : return tok_invalid;
                            case 't' :
                                return tok_kw_inject;
                            }
                        }
                    }
                }
            }

        case 's' :
            switch (_chrs[1]) {
            default : return tok_invalid;
            case 't' :
                switch (_chrs[2]) {
                default : return tok_invalid;
                case 'r' :
                    switch (_chrs[3]) {
                    default : return tok_invalid;
                    case 'u' :
                        switch (_chrs[4]) {
                        default : return tok_invalid;
                        case 'c' :
                            switch (_chrs[5]) {
                            default : return tok_invalid;
                            case 't' :
                                return tok_kw_struct;
                            }
                        }
                    }
                }
            }


        case 't' : {
            switch (_chrs[1]) {
            case 'y' :
                switch (_chrs[2]) {
                case 'p' :
                    switch (_chrs[3]) {
                    case 'e' :
                        switch (_chrs[4]) {
                        case ' ' :
                        case '\t':
                        case '\0':
                            return tok_kw_type;
                        default:
                            return tok_invalid;
                        case 'd' :
                            switch (_chrs[5]) {
                            default :
                                return tok_invalid;
                            case 'e' :
                                switch (_chrs[6]) {
                                default :
                                    return tok_invalid;
                                case 'f' :
                                    return tok_kw_typedef;
                                }
                            }
                        }
                    }
                }
            }

        case 'u' :
            switch (_chrs[1]) {
            default : return tok_invalid;
            case 'n' :
                switch (_chrs[2]) {
                default : return tok_invalid;
                case 'i' :
                    switch (_chrs[3]) {
                    default : return tok_invalid;
                    case 'o' :
                        switch (_chrs[4]) {
                        default : return tok_invalid;
                        case 'n' :
                            return tok_kw_union;
                        }
                    }
                }
            }

        case '{' :
            return tok_lBrace;

        case '}' :
            return tok_rBrace;


        case '~' :
            switch (_chrs[1]) {
            default :
            return tok_cat;
            case '=' :
                return tok_cat_ass;
            }

        }
    }

    return tok_invalid;
}

#if TEST_LEXER
#include <assert.h>

void test_lexer()
{
    const char *test[] =
    {
        "(",
        ")",
        "{",
        "}",
        "[",
        "]",
        ",",
        ".",
        "..",
        "/*",
        "*/",
        "//",
        "\"",
        "!",
        "-",
        "+",
        "/",
        "*",
        "&",
        "~",
        "~=",
        ";",
        ":",
        "$",
        "$(",
        "=",
        "==",
        "<",
        "<=",
        "[]",
        ">",
        ">=",
        "<=>",
        "struct",
        "union",
        "type",
        "enum",
        "inject",
        "eject",
        "assert",
        "typedef"
    };

    int idx = 0;
    for(metal_token_enum_t tok = tok_lParen; idx < (sizeof(test) / sizeof(test[0])); (*(int*)&tok)++)
    {
        metal_token_enum_t lexed = lexFixedToken(test[idx++]);
        assert(lexed == tok);
    }
}

int main(int argc, char* argv[])
{
    test_lexer();

    return 0;
}
#endif
/*
    tok_lParen, // "("
    tok_rParen, // ")"
    tok_lBrace, // "{"
    tok_rBrace, // "}"
    tok_lBracket, // "["
    tok_rBracket, // "]"

    tok_comma, // ","
    tok_dot, // "."
    tok_dotdot, // ".."
    tok_comment_begin, // "\/*"
    tok_comment_end, // "*\/"
    tok_comment_single, // "//"
    tok_quote, // "\""
    tok_bang, // "!"
    tok_minus, // "-",
    tok_plus, // "+"
    tok_div, // "/"
    tok_star,// "*"
    tok_addr, // "&"

    tok_cat, // "~"
    tok_cat_ass, // "~="

    tok_semicolon,// ";"
    tok_colon, // ":"
    tok_dollar, // "$"
    tok_dollar_paren, // "$("
    tok_assign, // "="
    tok_equalsequals, // "=="
    tok_lessThan, //  "<"
    tok_lessEqual, // "<="
    tok_full_slice, // "[]"

    tok_greaterThan, // ">"
    tok_greaterEqual, // ">="

    tok_spaceShip, // "<=>"

    tok_kw_struct, // "struct"
    tok_kw_union, // "union"
    tok_kw_type,  // "type"
    tok_kw_enum, // "enum"
    tok_kw_inject, // "inject"
    tok_kw_eject,// "eject"
    tok_kw_assert, // "assert"
    tok_kw_typedef, // "typedef"
*/
