#ifdef IDENTIFIER_TABLE
#  include "metac_identifier_table.c"
#endif

#ifdef IDENTIFIER_TREE
#  include "metac_identifier_tree.c"
#endif

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

    case '?':
        return tok_question;

    case '#':
        return tok_hash;

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
            return tok_comment_end_multi;
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
            switch(_chrs[2])
            {
            default:
                return tok_dotdot;
            case '.':
                return tok_dotdotdot;
            }

        }

    case '/':
        switch (_chrs[1])
        {
        default :
            return tok_div;
        case '/':
            return tok_comment_begin_single;
        case '*':
            return tok_comment_begin_multi;
        case '=':
            return tok_div_ass;
        }

    case '%':
    {
        switch(_chrs[1])
        {
        default :
            return tok_rem;
        case '=':
            return tok_rem_ass;
        }
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

static uint32_t MetaCStaticTokenLength(metac_token_enum_t t)
{
    switch (t) {
        default              : return 2;
        case tok_eof         : return 0;

        case tok_bang        : return 1;
        case tok_question    : return 1;
        case tok_hash        : return 1;
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
        case tok_rem         : return 1;
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
        case tok_dotdotdot   : return 3;

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

        case tok_kw_auto     : return 4;
        case tok_kw_double   : return 6;
        case tok_kw_float    : return 5;
        case tok_kw_long     : return 4;
        case tok_kw_int      : return 3;
        case tok_kw_short    : return 5;
        case tok_kw_char     : return 4;
        case tok_kw_void     : return 4;
        case tok_kw_unsigned : return 8;
        case tok_kw_volatile : return 8;
        case tok_kw_extern   : return 6;
        case tok_kw_for      : return 3;
        case tok_kw_sizeof   : return 6;
        case tok_kw_goto     : return 4;

        case tok_kw_scope    : return 5;
        case tok_kw_yield    : return 5;

        case tok_kw___LINE__     : return 8;
        case tok_kw___FUNCTION__ : return 12;
        case tok_kw___METAC__    : return 9;
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
        return MetaCStaticTokenLength(token.TokenType);
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
        else if (token.TokenType == tok_comment_single)
        {
            return token.CommentLength + 3;
        }
        else if (token.TokenType == tok_comment_multi)
        {
            return token.CommentLength + 4;
        }
        else if (token.TokenType == tok_charLiteral)
        {
            return token.CharLiteralLength + 2;
        }
    }

    assert(0);
    return 0;
}



void MetaCLexerInit(metac_lexer_t* self)
{
    self->TokenSize = 0;
    self->TokenCapacity =
        (sizeof(self->inlineTokens) / sizeof(self->inlineTokens[0]));
    self->Tokens = self->inlineTokens;

    self->LocationStorage.LocationCapacity = self->TokenCapacity;
    self->LocationStorage.LocationSize = 0;
    self->LocationStorage.Locations = (metac_location_t*)malloc(
        self->LocationStorage.LocationCapacity
            * sizeof(metac_location_t)
    );

#ifdef ACCEL
    ACCEL_INIT(*self, Identifier);
    ACCEL_INIT(*self, String);
#endif
}
metac_lexer_state_t MetaCLexerStateFromString(uint32_t sourceId,
                                              const char* str)
{
    uint32_t length = strlen(str);
    return MetaCLexerStateFromBuffer(sourceId, str, length);
}

metac_lexer_state_t MetaCLexerStateFromBuffer(uint32_t sourceId,
                                              const char* buffer,
                                              uint32_t bufferLength)
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

static inline bool IsIdentifierChar(char c)
{
    const char upper_c = (c & ~32);
    return (((upper_c >= 'A') & (upper_c <= 'Z')) | c == '_');
}

static inline bool IsNumericChar(char c)
{
    return (((cast(unsigned) c) - '0') <= 9);
}

static inline bool IsHexLiteralChar(char c)
{
    return (IsNumericChar(c) | (((cast(unsigned) (c | 32)) - 'a') <= 6));
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
    else if (c == '\"')
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
        case 'b'  : return '\b';
        case 'f'  : return '\f';
        case '\''  : return '\'';
        case '?'  : return '?';
        case '\\': return '\\';
    }
    return 'E';
}
static void MetaCLexerMatchKeywordIdentifier(metac_token_t* tok,
                                             const char* identifier)
{
#include "generated/metac_match_keyword.inl"
}

bool static ParseOctal(const char** textP, uint32_t* eatenCharsP, uint64_t* valueP)
{
    bool result = true;

    uint64_t value = 0;
    const char* text = *textP;
    uint32_t eatenChars = *eatenCharsP;

    char c = *text++;

    if(c < '0' || c > '7')
    {
        result = false;
    }

    while(c && (c >= '0' && c <= '7'))
    {
        eatenChars++;
        value *= 8;
        value += c - '0';
        c = *text++;
    }

    *eatenCharsP = eatenChars;
    *textP = text - 1;
    *valueP = value;

    return result;
}



bool static ParseHex(const char** textP, uint32_t* eatenCharsP, uint64_t* valueP)
{
    bool result = true;

    uint64_t value = 0;
    const char* text = *textP;
    uint32_t eatenChars = *eatenCharsP;

    char c = *text++;

    if(!IsHexLiteralChar(c))
    {
        result = false;
    }

    while(IsHexLiteralChar(c))
    {
        eatenChars++;
        value *= 16;
        c |= 32;
        if (c <= '9')
        {
            value += (c - '0');
        }
        else
        {
            value += ((c - 'a') + 10);
        }
        c = *text++;
    }

    *eatenCharsP = eatenChars;
    *textP = text - 1;
    *valueP = value;
    return result;
}

void ParseErrorBreak(void)
{
    int k = 2;
}

bool IsValidEscapeChar(char c)
{
    // printf("Calling %s with '%c'\n", __FUNCTION__, c);
    return (c == 'n'  || c == '"' || c == 't')
        || (c == '\'' || c == 'r' || c == '`')
        || (c == '\\' || c == '\n'|| c == '`')
        || (c == 'x'  || c == 'v' || c == 'a')
        || (c == 'f'  || c == '?' || c == 'b')
        || (c >= '0' && c <= '7');
}

typedef uint32_t metac_location_ptr;

void MetaCLocationStorage_EndLoc(
        metac_location_storage_t* locationStorage,
        metac_location_ptr locationId,
        uint32_t line, uint16_t column)
{
    assert(locationId >= 4);
    assert((locationId - 4) < locationStorage->LocationSize);

    uint32_t idx = locationId - 4;
    metac_location_t *location =
        locationStorage->Locations + idx;

    assert((line - location->StartLine) < 0xFFFF);

    location->LineSpan = line - location->StartLine;
    assert(column < 0xFFFF);
    location->EndColumn = (uint16_t)column;
}

metac_location_ptr MetaCLocationStorage_StartLoc(
        metac_location_storage_t* locationStorage,
        uint32_t line, uint16_t column)
{
    assert(locationStorage->LocationSize <
        locationStorage->LocationCapacity);

    uint32_t result = locationStorage->LocationSize++;

    locationStorage->Locations[result].StartLine = line;
    locationStorage->Locations[result].StartColumn = column;

    return result + 4;
}


metac_token_t* MetaCLexerLexNextToken(metac_lexer_t* self,
                                      metac_lexer_state_t* state,
                                      const char* text, uint32_t len)

{
    static metac_token_t err_token = {tok_error};
    metac_token_t token = {tok_invalid};
    if (text[len] != '\0')
    {
        return &err_token;
    }
    metac_token_t* result = 0;


    assert(self->TokenCapacity > self->TokenSize);
    uint32_t eatenChars = 0;
    char c = *text++;
LcontinueLexnig:
    {
        uint32_t column = state->Column;

        while (c && c <= 32)
        {
            if (c == '\n')
            {
                state->Line++;
                column = 0;
            }
            if (c == '\r')
            {
                column = 0;
            }
            column++;
            eatenChars++;
            c = *text++;
        }
        state->Column = column;
    }
    if (c)
    {
        text -= 1;
    }

    state->Position += eatenChars;
    eatenChars = 0;
    token.Position = state->Position;
    token.LocationId =
        MetaCLocationStorage_StartLoc(&self->LocationStorage, state->Line, state->Column);

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
                    eatenChars++;
                }
                token.TokenType = tok_identifier;
                assert(identifier_length < 0xFFF);
                state->Column += eatenChars;
                token.IdentifierKey =
                    IDENTIFIER_KEY(identifier_hash, identifier_length);
                MetaCLexerMatchKeywordIdentifier(&token, identifierBegin);
                if(token.TokenType == tok_identifier)
                {
#ifndef ACCEL
                token.Identifier = identifierBegin;
#elif ACCEL == ACCEL_TABLE
                    token.IdentifierPtr =
                        GetOrAddIdentifier(&self->IdentifierTable, token.IdentifierKey, identifierBegin, identifier_length);
#elif ACCEL == ACCEL_TREE
                    token.IdentifierPtr =
                        GetOrAddIdentifier(&self->IdentifierTree, token.IdentifierKey, identifierBegin, identifier_length);
#else
#   error ("Unkown ACCELERATOR")
#endif
                }
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
                    c = *text;
                    eatenChars++;
                    if (c == 'x')
                    {
                        text++;
                        eatenChars++;
                        if (!ParseHex(&text, &eatenChars, &value))
                        {
                            ParseErrorF(state, "invalid hex literal %.*s", 4, text - 1);
                            result = &err_token;
                            goto Lreturn;
                        }
                        goto LParseNumberDone;
                    }
                    else if (!IsNumericChar(c))
                    {
                        value = 0;
                        goto LParseNumberDone;
                    }
                    else
                    {
                        if (!ParseOctal(&text, &eatenChars, &value))
                        {
                            ParseErrorF(state, "invalid octal literal %.*s", 4, text - 1);
                            result = &err_token;
                            goto Lreturn;
                        }
                        goto LParseNumberDone;
                    }
                }

                while (c && IsNumericChar((c = *text++)))
                {
                    eatenChars++;
                    value *= 10;
                    value += c - '0';
                }
                c |= 32;
                while (c == 'u' ||  c == 'l')
                {
                    eatenChars++;
                    c = (*text++ | 32);
                }
            LParseNumberDone:
                token.ValueU64 = value;
            }
            else if (c == '\'')
            {
                uint32_t charLiteralLength = 0;
                text++;
                token.TokenType = tok_charLiteral;
                uint32_t charHash = ~0u;
                c = *text++;
                eatenChars++;
                if (c == '\'')
                {
                    ParseError(state, "Empty character Literal");
                    result = &err_token;
                    goto Lreturn;
                }
                while(c && c != '\'')
                {
                    token.chars[charLiteralLength++] = c;
                    if (c == '\\')
                    {
                        c = *text++;
                        token.chars[charLiteralLength++] = c;
                        eatenChars++;
                        if (!IsValidEscapeChar(c))
                        {
                            ParseErrorF(state, "Invalid escape seqeunce '%.*s'", 4, (text - 2));
                        }
                    }
                    c = *text++;
                    eatenChars++;
                }
                if (*text++ != '\'')
                {
                    assert("Unterminated char literal");
                }
                state->Column += eatenChars++;
                assert(charLiteralLength < sizeof(token.chars));
                token.CharLiteralLength = charLiteralLength;
            }
            else if (c == '\"' || c == '`')
            {
                ++text;
                char matchTo = c;
                token.TokenType = tok_stringLiteral;
                const char* stringBegin = text;
                uint32_t stringHash = ~0u;
                c = *text++;

                uint32_t column;
                eatenChars++;
                uint32_t eatenCharsAtStringStart = eatenChars;

                while(c && c != matchTo)
                {
#ifdef INCREMENTAL_HASH
                    stringHash = crc32c_byte(stringHash, c);
#endif
                    eatenChars++;
                    column++;
                    if (c == '\\')
                    {
                        eatenChars++;
                        column++;
#ifdef INCREMENTAL_HASH
                        stringHash = crc32c_byte(stringHash, c);
#endif
                        c = *text++;
                        if (!IsValidEscapeChar(c))
                        {
                            state->Column = column;
                            ParseErrorF(state, "Invalid escape seqeunce '%.*s'", 4, (text - 2));
                        }
                        if (c == '\n')
                        {
                            state->Line++;
                            column = 0;
                        }

                     }
                     c = *text++;
                }

                if (c != matchTo)
                {
                    ParseErrorF(state, "Unterminated string literal '%.*s' \n", 10, text - eatenChars - 1);
                    result = &err_token;
                    goto Lreturn;
                }

                uint32_t stringLength = (eatenChars - eatenCharsAtStringStart);

                eatenChars++;
#ifndef INCREMENTAL_HASH
                stringHash = crc32c(~0, stringBegin, stringLength);
#endif
                assert(stringLength < 0xFFFFF);

                token.Key = STRING_KEY(stringHash, stringLength);
#if ACCEL == ACCEL_TABLE
                token.StringPtr = GetOrAddIdentifier(&self->StringTable, token.Key, stringBegin, stringLength);
#elif ACCEL == ACCEL_TREE
                token.StringPtr = GetOrAddIdentifier(&self->StringTree, token.Key, stringBegin, stringLength);
#else
                token.String = stringBegin;
#endif
            }
            //TODO special hack as long as we don't do proper preprocessing
            else if (c == '\\')
            {
                text++;
                c = *text++;
                if (c == '\n')
                {
                    c = *text++;
                    state->Line++;
                    goto LcontinueLexnig;
                }
                else
                {
                    printf("escaping '%c' in wild code '%.*s' \n", c, 8, text - 4);
                    assert(0); // this is not to escape a newline
                }
            }
        }
    }
    else if (token.TokenType == tok_comment_begin_single)
    {
        token.TokenType = tok_comment_single;
        eatenChars += 2;
        text += 2;
        char* newlinePtr = (char*)memchr(text, '\n', len - eatenChars);
        uint32_t commentLength = (newlinePtr - text);
        if (!newlinePtr)
        {
            commentLength = len - eatenChars - 1;
            c = '\0';
        }
        else
        {
        }
        eatenChars += commentLength + 1;
        token.CommentLength = commentLength;
        token.CommentBegin = text + 2;
        state->Column += commentLength + 1;
    }
    else if (token.TokenType == tok_comment_begin_multi)
    {
        token.TokenType = tok_comment_multi;
        // printf("Comment starts at line: %u:%u\n", state->Line, state->Column);
        uint32_t offset = 0;
        char* endPtr;
        char* lastNewline = 0;
        char* newlinePtr = (char*)memchr(text + 2, '\n', len - 2);
        char* slashPtr = (char*)memchr(text + 2, '/', len - 2);
        for(;;)
        {
            while (newlinePtr && newlinePtr < slashPtr)
            {
                lastNewline = newlinePtr;
                offset = (newlinePtr - text);
                state->Line++;
                newlinePtr = (char*)memchr(lastNewline + 1, '\n', len - (lastNewline - text));
            }
            if (!slashPtr) assert(0);
            offset = (slashPtr - text);
            if ((*(slashPtr - 1)) == '*')
            {
                endPtr = slashPtr + 1;
                break;
            }
            newlinePtr = (char*)memchr(slashPtr, '\n', len - 2 - offset);
            slashPtr = (char*)memchr(slashPtr + 1, '/', len - 2 - offset);
        }
        eatenChars = endPtr - text;

        if (lastNewline && lastNewline < endPtr)
        {
            state->Column = endPtr - lastNewline;
        }
        else
        {
            state->Column += eatenChars;
        }
        token.CommentLength = eatenChars - 4;
        token.CommentBegin = text + 2;
        //printf("Comment ends at line: %u:%u\n", state->Line, state->Column);
        //printf("Comment was: \"%.*s\"\n", eatenChars - 4,  text + 2);
    }
#if 0
    else if (token.TokenType == tok_hash)
    {
        uint32_t lines = 0;
        const char* newlinePtr = text;
        uint32_t offset = 0;

    LskiptoNewline
        for(;;)
        {
            newlinePtr = (char*)memchr(newlinePtr + 1, '\n', len - offset);
            offset = (newlinePtr - text);
            //printf("offset: %u\n", offset);
            if (!newlinePtr || ((*(newlinePtr - 1)) != '\\'))
            {

                break;
            }

            lines++;

        }
        ++offset;
        //printf("skipping %u chars\n", offset);
        //printf("Just Skipped '%.*s'\n", offset,  text);
        //printf("NextChars 8: '%.*s'\n", 8,  text + offset);
        eatenChars = offset + 1;
        state->Line += lines;
        state->Column = 1;
        goto LcontinueLexnig;
    }
#endif
    else
    {
        const uint32_t tokLen = MetaCStaticTokenLength(token.TokenType);
        eatenChars += tokLen;
        state->Column += tokLen;
    }

    if (token.TokenType)
    {
        result = self->Tokens + self->TokenSize++;
        *result = token;
    }
    else
    {
        static metac_token_t stop_token = {tok_eof};
        result = &stop_token;
    }
Lreturn:
    MetaCLocationStorage_EndLoc(&self->LocationStorage,
        token.LocationId, state->Line, state->Column);

    state->Position += eatenChars;
    return result;
}

#if TEST_LEXER
#include <string.h>

void test_lexer()
{
    const char* token_list[] =
    {
        "!",
        "?",
        "#",

        "(",
        ")",
        "{",
        "}",
        "[",
        "]",

        ";",
        ":",
        "$",

        ",",
        ".",

        "+",
        "-",
        "*",
        "/",
        "%",

        "^",
        "|",
        "&",
        "~",

        "<<",
        ">>",

        "||",
        "&&",

        "->",
        "..",

        "=",

        "+=",
        "-=",
        "*=",
        "/=",
        "%=",

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

        "...",

        "struct",
        "union",
        "enum",

        "auto",
        "type",
        "void",

        "char",
        "short",
        "int",
        "long",

        "float",
        "double",

        "unsigned",
        "const",
        "volatile",
        "extern",

        "for",
        "sizeof",
        "return",
        "switch",
        "while",
        "do",

        "typeof",
        "inject",
        "eject",
        "assert",
        "typedef",
        "case",
        "goto",
        "static",
        "inline",
        "if",
        "else",
        "break",
        "continue",
        "until",
        "yield",
        "scope",

        "__LINE__",
        "__FUNCTION__",
        "__METAC__",

         "/*",
        "*/",
        "//",

        "++",
        "--",
        "[]",

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
            assert(tok == tok_error);
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

            lexer.Tokens = &t1;
            lexer.TokenCapacity = 1;
            lexer.TokenSize = 0;

            lexed = MetaCLexerLexNextToken(&lexer, &state, word, strlen(word))->TokenType;
        }
        else
        {
            lexed = MetaCLexFixedLengthToken(word);
        }
        assert(lexed == tok);
        assert(strlen(word) == MetaCStaticTokenLength(tok));
    }
}

int main(int argc, char* argv[])
{
    test_lexer();

    return 0;
}
#endif
