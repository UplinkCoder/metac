#include "metal_lexer.h"
#include "compat.h"

#include <assert.h>
#include <string.h>
#include <stdio.h>

static metal_token_enum_t MetalLexFixedLengthToken(const char _chrs[7]) {
    switch(_chrs[0]) {
        default :
            return tok_invalid;
        case '\0' :
            return tok_eof;

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

// keywords ------------- we might not want to lex em this way
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


        }
    }

    return tok_invalid;
}

static uint32_t StaticMetalTokenLength(metal_token_enum_t t)
{
    switch(t) {
        default :  return 1;
        case tok_dollar_paren : return 2; // $(
        case tok_comment_end : return 2; // */
        case tok_dotdot : return 2; // ..
        case tok_comment_begin : return 2; // /* */
        case tok_comment_single : return 2; // //
        case tok_lessEqual : return 2;// <=
        case tok_greaterEqual : return 2;// >=
        case tok_spaceShip : return 3;// <=>
        case tok_equalsequals : return 2; // ==
        case tok_full_slice : return 2; // []
        case tok_cat_ass : return 2; // ~=
        case tok_eof : return 0;

        case tok_kw_assert : return 6; // assert
        case tok_kw_eject : return 5; // eject
        case tok_kw_enum : return 4; // enum
        case tok_kw_inject : return 6; // inject
        case tok_kw_struct : return 6; // struct
        case tok_kw_type : return 4; // type
        case tok_kw_typedef : return 7; // typedef
        case tok_kw_union : return 5; // union
    }
}
uint32_t fastLog10(uint32_t val)
{
    return (val < 10) ? 0 : (val < 100) ? 1 : (val < 1000) ? 2 : (val < 10000) ? 3
        : (val < 100000) ? 4 : (val < 1000000) ? 5 : (val < 10000000) ? 6
        : (val < 100000000) ? 7 : (val < 1000000000) ? 8 : 9;
}

uint32_t MetalTokenLength(metal_token_t token)
{
    if (token.TokenType >= tok_lParen)
    {
        return StaticMetalTokenLength(token.TokenType);
    }
    else
    {
        if (token.TokenType == tok_unsignedNumber || token.TokenType == tok_signedNumber)
        {
            uint64_t v = (token.TokenType == tok_unsignedNumber ? token.ValueU64 : token.ValueU64);
            return (uint32_t)(fastLog10(v) + 1 + (token.TokenType == tok_signedNumber));
        }
        else if (token.TokenType == tok_identifier)
        {
            return token.Length;
        }
    }
}

void InitMetalLexer(metal_lexer_t* self)
{
    self->tokens_size = 0;
    self->tokens_capacity = (sizeof(self->inlineTokens) / sizeof(self->inlineTokens[0]));
    self->tokens = self->inlineTokens;
}

metal_lexer_state_t MetalLexerStateFromString(const char* str)
{
    uint32_t length = strlen(str);
    return MetalLexerStateFromBuffer(str, length + 1);
}

metal_lexer_state_t MetalLexerStateFromBuffer(const char* buffer, uint32_t bufferLength)
{
    assert(buffer[bufferLength] == '\0');

    metal_lexer_state_t result =
        {buffer, 1, 1, 0, bufferLength, (block_idx_t)0};

    return result;
}
#define ParseError(STATE, MSG) \
    fprintf(stderr, "ParseError: %s\n", MSG);

static inline bool IsIdentifierChar(char c)
{
    char upper_c = (c & ~32);
    return  (upper_c >= 'A' && upper_c <= 'Z') || c == '_';
}

static inline bool IsNumericChar(char c)
{
    return ((((unsigned)c) - '0') <= 9);
}

metal_token_t* MetalLexerLexNextToken(metal_lexer_t* self, metal_lexer_state_t* state,
                                      const char* text, uint32_t len)
{
    assert(text[len] == '\0');
    metal_token_t* result = 0;

    metal_token_t token = {0};
    token.TokenType = 0;
    token.Position = state->Position;
    token.SourceId = state->SourceId;

    assert(self->tokens_capacity > self->tokens_size);
    char c = *text++;

    while(c && (c == ' ' || c == '\t'))
        c = *text++;

    if ((token.TokenType = MetalLexFixedLengthToken(text)) == tok_invalid)
    {
        // const char* begin = text;
        if(c)
        {
             if (IsIdentifierChar(c))
             {
                 uint32_t length = 1;
                 token.Identfier = text - 1;
                 while(IsIdentifierChar(*text++)) {length++;}
                 token.TokenType = tok_identifier;
                 // token.Identfier = // AddIdentifier(lexer, )
                 token.Length = length;
             } else if (IsNumericChar(c))
             {
                 token.TokenType = tok_unsignedNumber;
                 bool isHex;
                 uint64_t value;
            LparseDigits:
                value = 0;
                isHex = false;

                if (c == '0')
                {
                    if ((c = *text++) == 'x')
                    {
                        isHex = true;
                        assert(0); // TODO Implement hex literal parsing.
                    }
                    else if (!IsNumericChar(c = *text++))
                    {
                        value = 0;
                        goto LParseNumberDone;
                    }
                    else
                    {
                        ParseError(state, "octal literal not supported");
                    }
                }

                value = c - '0';
                while(IsNumericChar((c = *text++)))
                {
                    value *= 10;
                    value += c - '0';
                }
            LParseNumberDone:
                token.ValueU64 = value;
             }
             else if (c == '-')
             {
                 if (IsNumericChar(c = *text++))
                 {
                     token.TokenType = tok_signedNumber;
                     goto LparseDigits;
                 }
             }
        }
    }

    if (token.TokenType)
    {
        result = self->tokens + self->tokens_size++;
        *result = token;
    }
    else
    {
        static metal_token_t stop_token = {0};
        result = &stop_token;
    }
    return result;
}

#if TEST_LEXER
#include <string.h>

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
        "typedef",
        "\0"
    };

    int idx = 0;

    for(metal_token_enum_t tok = tok_lParen; idx < (sizeof(test) / sizeof(test[0])); (*(int*)&tok)++)
    {
        const char* word = test[idx++];
        metal_token_enum_t lexed = MetalLexFixedLengthToken(word);
        assert(lexed == tok);
        assert(strlen(word) == MetalTokenLength(tok));
    }
}


int main(int argc, char* argv[])
{
    test_lexer();

    return 0;
}
#endif
