#include "metal_lexer.h"
#include "compat.h"

#include <assert.h>
#include <string.h>
#include <stdio.h>

#include "cache/crc32.c"

static metal_token_enum_t MetalLexFixedLengthToken(const char _chrs[7])
{
    switch (_chrs[0])
    {
    default:
        return tok_invalid;

    case '\0':
        return tok_eof;

    case '!':
        switch (_chrs[1])
        {
        default:
            return tok_bang;
        case '=':
            return tok_notEqual;
        }

    case '$':
            return tok_dollar;

    case '&':
        return tok_addr;

    case '(':
        return tok_lParen;

    case ')':
        return tok_rParen;

    case '*':
        switch (_chrs[1])
        {
        default:
            return tok_star;
        case '/':
            return tok_comment_end;
        }

    case '+':
        return tok_plus;

    case ',':
        return tok_comma;

    case '-':
        switch (_chrs[1])
        {
        default:
            return tok_minus;
        case '>':
            return tok_arrow;
        }

    case '.':
        switch (_chrs[1])
        {
        default:
            return tok_dot;
        case '.':
            return tok_dotdot;
        }

    case '/':
        switch (_chrs[1])
        {
        default:
            return tok_div;
        case '/':
            return tok_comment_single;
        case '*':
            return tok_comment_begin;
        }

    case ':':
        return tok_colon;

    case ';':
        return tok_semicolon;

    case '<':
        switch (_chrs[1])
        {
        default:
            return tok_lessThan;
        case '=':
            switch (_chrs[2])
            {
            default:
                return tok_lessEqual;
            case '>':
                return tok_spaceship;
            }
        }

    case '=':
        switch (_chrs[1])
        {
        default:
            return tok_assign;
        case '=':
            return tok_equalsEquals;
        }

    case '>':
        switch (_chrs[1])
        {
        default:
            return tok_greaterThan;
        case '=':
            return tok_greaterEqual;
        }

    case '[':
        switch (_chrs[1])
        {
        default:
            return tok_lBracket;
        case ']':
            return tok_full_slice;
        }

    case ']':
        return tok_rBracket;

    case '{':
        return tok_lBrace;

    case '}':
        return tok_rBrace;

    case '~':
        switch (_chrs[1])
        {
        default:
            return tok_cat;
        case '=':
            return tok_cat_ass;
        }

        // keywords ------------- we might not want to lex em this way
    case 'a':
        switch (_chrs[1])
        {
        default:
            return tok_invalid;
        case 's':
            switch (_chrs[2])
            {
            default:
                return tok_invalid;
            case 's':
                switch (_chrs[3])
                {
                default:
                    return tok_invalid;
                case 'e':
                    switch (_chrs[4])
                    {
                    default:
                        return tok_invalid;
                    case 'r':
                        switch (_chrs[5])
                        {
                        default:
                            return tok_invalid;
                        case 't':
                            return tok_kw_assert;
                        }
                    }
                }
            }
        }

    case 'e':
        {
            switch (_chrs[1])
            {
            default:
                return tok_invalid;
            case 'n':
                {
                    switch (_chrs[2])
                    {
                    default:
                        return tok_invalid;
                    case 'u':
                        switch (_chrs[3])
                        {
                        default:
                            return tok_invalid;
                        case 'm':
                            return tok_kw_enum;
                        }
                    }
                }
            case 'j':
                switch (_chrs[2])
                {
                default:
                    return tok_invalid;
                case 'e':
                    switch (_chrs[3])
                    {
                    default:
                        return tok_invalid;
                    case 'c':
                        switch (_chrs[4])
                        {
                        default:
                            return tok_invalid;
                        case 't':
                            return tok_kw_eject;
                        }
                    }
                }
            }
        }

    case 'i':
        switch (_chrs[1])
        {
        default:
            return tok_invalid;
        case 'n':
            switch (_chrs[2])
            {
            default:
                return tok_invalid;
            case 'j':
                switch (_chrs[3])
                {
                default:
                    return tok_invalid;
                case 'e':
                    switch (_chrs[4])
                    {
                    default:
                        return tok_invalid;
                    case 'c':
                        switch (_chrs[5])
                        {
                        default:
                            return tok_invalid;
                        case 't':
                            return tok_kw_inject;
                        }
                    }
                }
            }
        }

    case 's':
        switch (_chrs[1])
        {
        default:
            return tok_invalid;
        case 't':
            switch (_chrs[2])
            {
            default:
                return tok_invalid;
            case 'r':
                switch (_chrs[3])
                {
                default:
                    return tok_invalid;
                case 'u':
                    switch (_chrs[4])
                    {
                    default:
                        return tok_invalid;
                    case 'c':
                        switch (_chrs[5])
                        {
                        default:
                            return tok_invalid;
                        case 't':
                            return tok_kw_struct;
                        }
                    }
                }
            }
        }

    case 't':
        {
            switch (_chrs[1])
            {
            case 'y':
                switch (_chrs[2])
                {
                case 'p':
                    switch (_chrs[3])
                    {
                    case 'e':
                        switch (_chrs[4])
                        {
                        case ' ':
                        case '\t':
                        case '\0':
                            return tok_kw_type;
                        default:
                            return tok_invalid;
                        case 'd':
                            switch (_chrs[5])
                            {
                            default:
                                return tok_invalid;
                            case 'e':
                                switch (_chrs[6])
                                {
                                default:
                                    return tok_invalid;
                                case 'f':
                                    return tok_kw_typedef;
                                }
                            }
                        }
                    }
                }
            }

    case 'u':
            switch (_chrs[1])
            {
            default:
                return tok_invalid;
            case 'n':
                switch (_chrs[2])
                {
                default:
                    return tok_invalid;
                case 'i':
                    switch (_chrs[3])
                    {
                    default:
                        return tok_invalid;
                    case 'o':
                        switch (_chrs[4])
                        {
                        default:
                            return tok_invalid;
                        case 'n':
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
        case tok_comment_end : return 2; // */
        case tok_arrow : return 2; // ->
        case tok_dotdot : return 2; // ..
        case tok_comment_begin : return 2; // /* */
        case tok_comment_single : return 2; // //
        case tok_lessEqual : return 2;// <=
        case tok_greaterEqual : return 2;// >=
        case tok_spaceship : return 3;// <=>
        case tok_equalsEquals : return 2; // ==
        case tok_notEqual : return 2; // !=
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
    return (val < 10) ? 0
         : (val < 100) ? 1
         : (val < 1000) ? 2
         : (val < 10000) ? 3
         : (val < 100000) ? 4
         : (val < 1000000) ? 5
         : (val < 10000000) ? 6
         : (val < 100000000) ? 7
         : (val < 1000000000) ? 8 : 9;
}

uint32_t MetalTokenLength(metal_token_t token)
{
    if (token.TokenType >= tok_lParen)
    {
        return StaticMetalTokenLength(token.TokenType);
    }
    else
    {
        if (token.TokenType == tok_unsignedNumber)
        {
            uint64_t v = token.ValueU64;
            return (uint32_t)(fastLog10(v) + 1);
        }
        else if (token.TokenType == tok_identifier || token.TokenType == tok_stringLiteral)
        {
            return token.Length;
        }
    }

    assert(0);
    return 0;
}

void InitMetalLexer(metal_lexer_t* self)
{
    self->tokens_size = 0;
    self->tokens_capacity =
        (sizeof(self->inlineTokens) / sizeof(self->inlineTokens[0]));
    self->tokens = self->inlineTokens;
}

metal_lexer_state_t MetalLexerStateFromString(uint32_t sourceId, const char* str)
{
    uint32_t length = strlen(str);
    return MetalLexerStateFromBuffer(sourceId, str, length + 1);
}

metal_lexer_state_t MetalLexerStateFromBuffer(uint32_t sourceId, const char* buffer, uint32_t bufferLength)
{
    assert(buffer[bufferLength] == '\0');

    metal_lexer_state_t result;

    result.Text = buffer;
    result.Column = 1;
    result.Line = 1;
    result.Position = 0;
    result.Size = bufferLength;
    result.OuterBlock = cast(block_idx_t) 0;
    result.SourceId = sourceId;

    return result;
}

#define WRAP(...)

#define ParseErrorF(STATE, MSG, ...) \
    fprintf(stderr, "ParseError[%s:%u]: %u"  MSG  "\n", __FILE__, __LINE__, STATE->Position, __VA_ARGS__)

#define ParseError(STATE, MSG) \
    fprintf(stderr, "ParseError[%s:%u]: %u" MSG "\n", __FILE__, __LINE__, STATE->Position)

static inline bool IsIdentifierChar(char c)
{
    const char upper_c = (c & ~32);
    return (upper_c >= 'A' && upper_c <= 'Z') || c == '_';
}

static inline bool IsNumericChar(char c)
{
    return (((cast(unsigned) c) - '0') <= 9);
}

static inline metal_token_enum_t classify(char c)
{
    metal_token_enum_t result = tok_invalid;

    if (IsIdentifierChar(c))
    {
        result = tok_identifier;
    }
    else if (IsNumericChar(c))
    {
        result = tok_unsignedNumber;
    }
    else if (c == '"')
    {
        result = tok_stringLiteral;
    }

    return result;
}

static inline char EscapedChar(char c)
{
    switch(c)
    {
        case 'n'  : return '\n';
        case 'v'  : return '\v';
        case 't'  : return '\t';
        case 'r'  : return '\r';
        case '"'  : return '"';
        case 'a'  : return '\a';
        case '\'' : return '\'';
        case '\\' : return '\\';
        case '0'  : return '\0';
    }
    return 'E';
}

metal_token_t* MetalLexerLexNextToken(metal_lexer_t* self,
                                      metal_lexer_state_t* state,
                                      const char* text, uint32_t len)
{
    assert(text[len] == '\0');
    metal_token_t* result = 0;

    metal_token_t token = {(metal_token_enum_t)0};
    token.SourceId = state->SourceId;

    assert(self->tokens_capacity > self->tokens_size);
    uint32_t eaten_chars = 0;
    char c = *text++;

    while (c && (c == ' ' || c == '\t' || c == '\n' || c == '\r'))
    {
        c = *text++;
        if (c == '\n')
        {
            state->Line++;
            state->Column = 0;
        }
        if (c == '\r')
        {
            state->Column = 0;
        }
        state->Column++;
        eaten_chars++;
    }
    if (c)
    {
        text -= 1;
    }

    token.Position = state->Position + eaten_chars;
    if ((token.TokenType = MetalLexFixedLengthToken(text)) == tok_invalid)
    {
        // const char* begin = text;
        if (c)
        {
            if (IsIdentifierChar(c))
            {
                uint32_t length = 0;
                token.Identifier = text;

                while ((c = *text++) && (IsIdentifierChar(c) || IsNumericChar(c)))
                {
                    length++;
                    eaten_chars++;
                }
                token.TokenType = tok_identifier;

                token.Length = length;
                token.Crc32CLw16 = (crc32c(~0, token.Identifier, token.Length) & 0xFFFF);
            }
            else if (IsNumericChar(c))
            {
                token.TokenType = tok_unsignedNumber;
                bool isHex;
                uint64_t value;
            LparseDigits:
                value = 0;
                isHex = false;

                if (c == '0')
                {
                    ++text;
                    c = *(text++);
                    eaten_chars++;

                    if (c == 'x')
                    {
                        isHex = true;
                        assert(0); // TODO Implement hex literal parsing.
                    }
                    else if (!IsNumericChar(c))
                    {
                        value = 0;
                        goto LParseNumberDone;
                    }
                    else
                    {
                        ParseErrorF(state, "octal literal not supported %s", text - 1);
                    }
                }

                while (c && IsNumericChar((c = *text++)))
                {
                    eaten_chars++;
                    value *= 10;
                    value += c - '0';
                }
            LParseNumberDone:
                token.ValueU64 = value;
            }
            else if (c == '"')
            {
                ++text;
                // eaten_chars++;
                token.TokenType = tok_stringLiteral;
                token.String = text;
                c = *text++;
                // printf("c: %c\n", c);
                uint32_t string_length = 0;
                eaten_chars++;
                while(c && c != '"')
                {
                    string_length++;
                    if (c == '\\')
                    {
                        eaten_chars++;
                        c = EscapedChar(*text++);
                        if (c == 'E')
                        {
                            ParseError(state, "Invalid escape seqeunce");
                        }
                    }
                    c = *text++;
                    eaten_chars++;
                }
                if (c != '"')
                {
                    ParseError(state, "Unterminted string literal");
                }
                eaten_chars++;
                c = *text++;
                token.Length = string_length;
                token.Crc32CLw16 = (crc32c(~0, token.String, token.Length) & 0xFFFF);
            }
        }
    }
    else
    {
        eaten_chars += StaticMetalTokenLength(token.TokenType);
    }

    if (token.TokenType)
    {
        result = self->tokens + self->tokens_size++;
        *result = token;
    }
    else
    {
        static metal_token_t stop_token = {tok_eof};
        result = &stop_token;
    }
    state->Position += eaten_chars;
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
         "/*",
        "*/",

        "//",

        "!",
        "&",
        ";",
        ":",
        "$",
        "[]",

        "first_binary",
        ",",
        "->",
        ".",
        "..",

        "-",
        "+",
        "/",
        "*",

        "~",
        "~=",
        "=",
        "==",
        "!=",
        "<",
        "<=",
        ">",
        ">=",
        "<=>",

        "first_keyword",
        "struct",
        "union",
        "type",
        "enum",
        "inject",
        "eject",
        "assert",
        "typedef",

        "\0",

        (const char*) 0
    };

    int idx = 0;

    for (metal_token_enum_t tok = tok_lParen;
        idx < (sizeof(test) / sizeof(test[0]));
        (*(int*)&tok)++)
    {
        const char* word = test[idx++];
        if (!word)
        {
            assert(tok == tok_max);
            continue;
        }
        if (!memcmp(word, "first_", sizeof("first_") - 1))
        {
            continue;
        }
        metal_token_enum_t lexed = MetalLexFixedLengthToken(word);
        assert(lexed == tok);
        assert(strlen(word) == StaticMetalTokenLength(tok));
    }
}

int main(int argc, char* argv[])
{
    test_lexer();

    return 0;
}
#endif
