#include "metac_lexer.h"
#include "compat.h"

#include <assert.h>
#include <string.h>
#include <stdio.h>

#include "cache/crc32.c"

static metac_token_enum_t MetaCLexFixedLengthToken(const char _chrs[3])
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

    case '(':
        return tok_lParen;

    case ')':
        return tok_rParen;

    case '*':
        switch (_chrs[1])
        {
        default:
            return tok_star;
        case '=':
            return tok_mul_ass;
        case '/':
            return tok_comment_end;
        }

    case ',':
        return tok_comma;

    case '+':
        switch (_chrs[1])
        {
        default :
            return tok_plus;
        case '+' :
            return tok_plusplus;
        case '=':
            return tok_add_ass;
        }

    case '-':
        switch (_chrs[1])
        {
        default :
            return tok_minus;
        case '-' :
            return tok_minusminus;
        case '=':
            return tok_sub_ass;
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
        default :
            return tok_div;
        case '/':
            return tok_comment_single;
        case '*':
            return tok_comment_begin;
        case '=':
            return tok_div_ass;
        }

    case ':':
        return tok_colon;

    case ';':
        return tok_semicolon;

    case '<':
        switch (_chrs[1])
        {
        default:
            return tok_lt;
        case '<':
            switch (_chrs[2])
            {
                default:
                    return tok_lsh;
                case '=':
                    return tok_lsh_ass;
            }
        case '=':
            switch (_chrs[2])
            {
            default:
                return tok_le;
            case '>':
                return tok_spaceship;
            }
        }

    case '^':
        switch (_chrs[1])
        {
        default:
            return tok_xor;
        case '=':
            return tok_xor_ass;
        }

    case '|':
        switch (_chrs[1])
        {
        default:
            return tok_or;
        case '|':
            return tok_oror;
        case '=':
            return tok_or_ass;
        }

    case '&':
        switch (_chrs[1])
        {
        default:
            return tok_and;
        case '&':
            return tok_andand;
        case '=':
            return tok_and_ass;
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
            return tok_gt;
        case '>':
            switch (_chrs[2])
            {
                default:
                    return tok_rsh;
                case '=':
                    return tok_rsh_ass;
            }

        case '=':
            return tok_ge;
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
    }
    return tok_invalid;
}

const char* MetaCTokenEnum_toChars(metac_token_enum_t type)
{
    const char* result = 0;

#define CASE_MACRO(TOKEN) \
    case TOKEN : {result = #TOKEN;} break;

    switch (type)
    {
        FOREACH_TOKEN(CASE_MACRO)
    }

    return result;

#undef CASE_MACRO
}

static uint32_t StaticMetaCTokenLength(metac_token_enum_t t)
{
    switch (t) {
        default              : return 2;
        case tok_eof         : return 0;

        case tok_bang        : return 1;
        case tok_lParen      : return 1;
        case tok_rParen      : return 1;
        case tok_lBrace      : return 1;
        case tok_rBrace      : return 1;
        case tok_lBracket    : return 1;
        case tok_rBracket    : return 1;
        case tok_semicolon   : return 1;
        case tok_colon       : return 1;
        case tok_dollar      : return 1;
        case tok_comma       : return 1;
        case tok_dot         : return 1;
        case tok_plus        : return 1;
        case tok_minus       : return 1;
        case tok_star        : return 1;
        case tok_div         : return 1;
        case tok_xor         : return 1;
        case tok_or          : return 1;
        case tok_and         : return 1;
        case tok_cat         : return 1;
        case tok_assign      : return 1;
        case tok_lt          : return 1;
        case tok_gt          : return 1;

        case tok_lsh_ass     : return 3; // <<=
        case tok_rsh_ass     : return 3; // >>=

        case tok_spaceship   : return 3;// <=>

        case tok_kw_struct   : return 6;
        case tok_kw_union    : return 5;
        case tok_kw_enum     : return 4;
        case tok_kw_type     : return 4;
        case tok_kw_const    : return 5;
        case tok_kw_return   : return 6;
        case tok_kw_switch   : return 6;
        case tok_kw_while    : return 5;
        case tok_kw_typeof   : return 6;
        case tok_kw_inject   : return 6;
        case tok_kw_eject    : return 5;
        case tok_kw_assert   : return 6;
        case tok_kw_typedef  : return 7;
        case tok_kw_case     : return 4;
        case tok_kw_static   : return 6;
        case tok_kw_inline   : return 6;
        case tok_kw_else     : return 4;
        case tok_kw_break    : return 5;
        case tok_kw_continue : return 8;
        case tok_kw_until    : return 5;

    }
}

static inline uint32_t fastLog10(uint32_t val)
{
  uint32_t result;

  result = 0;
  if ( val >= 10 )
  {
    result = 1;
    if ( val >= 100 )
    {
      result = 2;
      if ( val >= 1000 )
      {
        result = 3;
        if ( val >= 10000 )
        {
          result = 4;
          if ( val >= 100000 )
          {
            result = 5;
            if ( val >= 1000000 )
            {
              result = 6;
              if ( val >= 10000000 )
              {
                result = 7;
                if ( val >= 100000000 )
                  result = 9 - (val < 1000000000);
              }
            }
          }
        }
      }
    }
  }
  return result;
}

uint32_t MetaCTokenLength(metac_token_t token)
{
    if (token.TokenType >= FIRST_STATIC_TOKEN(TOK_SELF))
    {
        return StaticMetaCTokenLength(token.TokenType);
    }
    else
    {
        if (token.TokenType == tok_unsignedNumber)
        {
            uint64_t v = token.ValueU64;
            return (uint32_t)(fastLog10(v) + 1);
        }
        else if (token.TokenType == tok_identifier)
        {
            return LENGTH_FROM_IDENTIFIER_KEY(token.Key);
        }
        else if (token.TokenType == tok_stringLiteral)
        {
            return LENGTH_FROM_STRING_KEY(token.Key);
        }
    }

    assert(0);
    return 0;
}

void InitMetaCLexer(metac_lexer_t* self)
{
    self->tokens_size = 0;
    self->tokens_capacity =
        (sizeof(self->inlineTokens) / sizeof(self->inlineTokens[0]));
    self->tokens = self->inlineTokens;
}

metac_lexer_state_t MetaCLexerStateFromString(uint32_t sourceId, const char* str)
{
    uint32_t length = strlen(str);
    return MetaCLexerStateFromBuffer(sourceId, str, length + 1);
}

metac_lexer_state_t MetaCLexerStateFromBuffer(uint32_t sourceId, const char* buffer, uint32_t bufferLength)
{
    assert(buffer[bufferLength] == '\0');

    metac_lexer_state_t result;

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

static inline bool IsIdentifierChar(char c)
{
    const char upper_c = (c & ~32);
    return (upper_c >= 'A' & upper_c <= 'Z') | c == '_';
}

static inline bool IsNumericChar(char c)
{
    return (((cast(unsigned) c) - '0') <= 9);
}

static inline metac_token_enum_t classify(char c)
{
    metac_token_enum_t result = tok_invalid;

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
    switch (c)
    {
        case 'n'  : return '\n';
        case 'v'  : return '\v';
        case 't'  : return '\t';
        case 'r'  : return '\r';
        case '"'  : return '"';
        case 'a'  : return '\a';
        case '\'': return '\'';
        case '\\': return '\\';
        case '0'  : return '\0';
    }
    return 'E';
}
void MetaCLexerMatchKeywordIdentifier(metac_token_t* tok)
{
#include "generated/metac_match_keyword.inl"
}

metac_token_t* MetaCLexerLexNextToken(metac_lexer_t* self,
                                      metac_lexer_state_t* state,
                                      const char* text, uint32_t len)
{
    assert(text[len] == '\0');
    metac_token_t* result = 0;

    metac_token_t token = {(metac_token_enum_t)0};
    token.SourceId = state->SourceId;

    assert(self->tokens_capacity > self->tokens_size);
    uint32_t eaten_chars = 0;
    char c = *text++;

    while (c && (c == ' ' || c == '\t' || c == '\n' || c == '\r'))
    {
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
        c = *text++;
    }
    if (c)
    {
        text -= 1;
    }

    token.Position += eaten_chars;
    if ((token.TokenType = MetaCLexFixedLengthToken(text)) == tok_invalid)
    {
        // const char* begin = text;
        if (c)
        {
            if (IsIdentifierChar(c))
            {
                uint32_t identifier_length = 0;
                uint32_t identifier_hash = ~0;
                const char* identifierBegin = text;

                while ((c = *text++) && (IsIdentifierChar(c) || IsNumericChar(c)))
                {
                    identifier_hash = crc32c_byte(identifier_hash, c);
                    identifier_length++;
                    eaten_chars++;
                }
                token.TokenType = tok_identifier;
                assert(identifier_length < 0xFFF);
                token.IdentifierKey =
                    IDENTIFIER_KEY(identifier_hash, identifier_length);
#ifdef IDENTIFIER_TABLE
                if(token.TokenType == tok_identifier)
                {
                    token->identifier_idx =
                        GetOrAddIdentfier(lexer->IdentifierTable, token.IdentifierKey, identifierBegin);
                }
#endif
#ifndef IDENTIFIER_TABLE
                token.Identifier = identifierBegin;
#endif
                MetaCLexerMatchKeywordIdentifier(&token);
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
                token.TokenType = tok_stringLiteral;
                token.String = text;
                c = *text++;
                // printf("c: %c\n", c);
                uint32_t string_length = 0;
                uint32_t string_hash = ~0;
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
                    string_hash = crc32c_byte(string_hash, c);
                    c = *text++;
                    eaten_chars++;
                }
                if (c != '"')
                {
                    ParseError(state, "Unterminted string literal");
                }
                eaten_chars++;
                assert(string_length < 0xFFFFF);
                token.Key = STRING_KEY(string_hash, string_length);
            }
        }
    }
    else
    {
        eaten_chars += StaticMetaCTokenLength(token.TokenType);
    }

    if (token.TokenType)
    {
        result = self->tokens + self->tokens_size++;
        *result = token;
    }
    else
    {
        static metac_token_t stop_token = {tok_eof};
        result = &stop_token;
    }
    state->Position += eaten_chars;
    return result;
}

#if TEST_LEXER
#include <string.h>

void test_lexer()
{
    const char* token_list[] =
    {
        "!",

        "(",
        ")",
        "{",
        "}",
        "[",
        "]",
         "/*",
        "*/",

        "//",

        "++",
        "--",

        ";",
        ":",
        "$",
        "[]",

        ",",
        "->",
        ".",
        "..",

        "+",
        "-",
        "*",
        "/",

        "^",
        "|",
        "&",
        "~",

        "<<",
        ">>",

        "||",
        "&&",

        "=",

        "+=",
        "-=",
        "*=",
        "/=",

        "^=",
        "|=",
        "&=",
        "~=",

        "<<=",
        ">>=",

        "==",
        "!=",

        "<",
        "<=",
        ">",
        ">=",
        "<=>",

        "struct",
        "union",
        "enum",

        "auto",
        "double",
        "float",
        "long",
        "short",
        "int",
        "char",
        "void",
        "type",

        "unsigned"
        "const",
        "volatile",
        "extern",

        "for",
        "sizeof",
        "return"
        "switch",
        "while",

        "typeof",
        "inject",
        "eject",
        "assert",
        "typedef",
        "case",
        "goto",
        "do",
        "static",
        "inline",
        "return",
        "if",
        "else",
        "break",
        "continue",
        "until",

        "\0",

        (const char*) 0
    };

    int idx = 0;

    for (metac_token_enum_t tok = FIRST_STATIC_TOKEN(TOK_SELF);
        idx < (sizeof(token_list) / sizeof(token_list[0]));
        (*(int*)&tok)++)
    {
        const char* word = token_list[idx++];
        if (!word)
        {
            assert(tok == tok_max);
            continue;
        }
        if (!memcmp(word, "first_", sizeof("first_") - 1))
        {
            continue;
        }
        metac_token_enum_t lexed;
        if ((tok >= FIRST_KEYWORD_TOKEN(TOK_SELF)) & tok <= LAST_KEYWORD_TOKEN(TOK_SELF))
        {
            metac_lexer_state_t state = {0};
            metac_token_t t1 = {tok_invalid};
            metac_lexer_t lexer;

            lexer.tokens = &t1;
            lexer.tokens_capacity = 1;
            lexer.tokens_size = 0;

            lexed = MetaCLexerLexNextToken(&lexer, &state, word, strlen(word))->TokenType;
        }
        else
        {
            lexed = MetaCLexFixedLengthToken(word);
        }
        assert(lexed == tok);
        assert(strlen(word) == StaticMetaCTokenLength(tok));
    }
}

int main(int argc, char* argv[])
{
    test_lexer();

    return 0;
}
#endif
