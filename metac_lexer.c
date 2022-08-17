#include "metac_identifier_table.c"
#include "metac_lexer.h"
#include "compat.h"
#include <assert.h>
#include <string.h>
#include <stdio.h>

#include "crc32c.h"

static inline metac_token_enum_t MetaCLexFixedLengthToken(const char _chrs[3])
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
        switch(_chrs[1])
        {
        default:
            return tok_hash;
        case '#':
            return tok_hashhash;
        }

    case '@':
        return tok_at;

    case '!':
        switch (_chrs[1])
        {
        default:
            return tok_bang;
        case '=':
            return tok_not_equal;
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
            return tok_equals_equals;
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
//        case '=':
//            return tok_cat_ass;
        }

#ifdef LEX_KEYWORDS_IN_SWTICH
#  include "generated/metac_keyword_switch.inl"
#endif

    case '\n':
        return tok_newline;

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

static uint32_t MetaCStaticTokenLength(metac_token_enum_t token)
{
    assert(token != tok_uint
        && token != tok_identifier
        && token != tok_string
        && token != tok_comment_single
        && token != tok_comment_multi
        && token != tok_char
        && token != tok_char_uni);

    switch (token) {
        default              : return 2;
        case tok_eof         : return 0;

        case tok_bang        : return 1;
        case tok_question    : return 1;
        case tok_hash        : return 1;
        case tok_at          : return 1;
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
        case tok_newline     : return 1;

        case tok_lsh_ass     : return 3; // <<=
        case tok_rsh_ass     : return 3; // >>=

        case tok_spaceship   : return 3;// <=>
        case tok_dotdotdot   : return 3;

        case tok_kw_struct   : return 6;
        case tok_kw_union    : return 5;
        case tok_kw_enum     : return 4;
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
        case tok_kw_default  : return 7;
        case tok_kw_static   : return 6;
        case tok_kw_inline   : return 6;
        case tok_kw_else     : return 4;
        case tok_kw_break    : return 5;
        case tok_kw_continue : return 8;
        case tok_kw_until    : return 5;

        case tok_kw_auto     : return 4;
        case tok_kw_bool     : return 4;
        case tok_kw_double   : return 6;
        case tok_kw_float    : return 5;
        case tok_kw_long     : return 4;
        case tok_kw_int      : return 3;
        case tok_kw_short    : return 5;
        case tok_kw_char     : return 4;
        case tok_kw_void     : return 4;
        case tok_kw_signed   : return 6;
        case tok_kw_unsigned : return 8;
        case tok_kw_volatile : return 8;
        case tok_kw___shared : return 8;
        case tok_kw_extern   : return 6;
        case tok_kw_for      : return 3;
        case tok_kw_sizeof   : return 6;
        case tok_kw_size_t   : return 6;
        case tok_kw_goto     : return 4;

        case tok_kw_yield    : return 5;
        case tok_kw___attribute__ : return 13;
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
        if (token.TokenType == tok_uint)
        {
            return token.ValueLength;
        }
        else if (token.TokenType == tok_identifier)
        {
            return LENGTH_FROM_IDENTIFIER_KEY(token.Key);
        }
        else if (token.TokenType == tok_string)
        {
            return LENGTH_FROM_STRING_KEY(token.Key);
        }
        else if (token.TokenType == tok_comment_single)
        {
            return token.CommentLength + 2;
        }
        else if (token.TokenType == tok_comment_multi)
        {
            return token.CommentLength + 4;
        }
        else if (token.TokenType == tok_char)
        {
            return token.charLength + 2;
        }
        else if (token.TokenType == tok_char_uni)
        {
            return token.charLength + 4;
        }
    }

    assert(0);
    return 0;
}



void MetaCLexer_Init(metac_lexer_t* self)
{
    self->TokenCount = 0;
    self->TokenCapacity = ARRAY_SIZE(self->inlineTokens);
    self->Tokens = self->inlineTokens;

    self->LocationStorage.LocationCapacity = ARRAY_SIZE(self->inlineLocations);
    self->LocationStorage.LocationSize = 0;
    self->LocationStorage.Locations = self->inlineLocations;

    ACCEL_INIT(*self, Identifier, IDENTIFIER_LENGTH_SHIFT, 13);
    ACCEL_INIT(*self, String, STRING_LENGTH_SHIFT, 13);
}

void MetaCLexer_Free(metac_lexer_t* self)
{
    IdentifierTable_Free(&self->IdentifierTable);
    IdentifierTable_Free(&self->StringTable);

    if (self->LocationStorage.Locations != self->inlineLocations)
        free(self->LocationStorage.Locations);
    if (self->Tokens != self->inlineTokens)
        free(self->Tokens);

    static const metac_lexer_t zeroLexer = {0};
    *self = zeroLexer;
    self = 0;
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

#if 0
#   define UPPER_CHAR(C) \
        ((C) & ~32)

#   define IsIdentifierChar(C) \
        (((UPPER_CHAR(C) >= 'A') & (UPPER_CHAR(C) <= 'Z')) | c == '_')

#else
static inline bool IsIdentifierChar(char c)
{
    const char upper_c = (c & ~32);
    return (((upper_c >= 'A') & (upper_c <= 'Z')) | (c == '_'));
}
#endif

static inline bool IsNumericChar(char c)
{
    return (((cast(unsigned) c) - '0') <= 9);
}

static inline bool IsHexLiteralChar(char c)
{
  c |= 32;
  return (cast(unsigned)(c - '0') <= 9) | (cast(unsigned)(c - 'a') <= 6);
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
        result = tok_uint;
    }
    else if (c == '\"')
    {
        result = tok_string;
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

/// is it valid for c to follow a \ in character or string literal
bool IsValidEscapeChar(char c)
{
    // printf("Calling %s with '%c'\n", __FUNCTION__, c);
    return (c == 'n'  || c == '"' || c == 't')
        || (c == '\'' || c == 'r' || c == 'u')
        || (c == '\\' || c == '\n'|| c == '`')
        || (c == 'x'  || c == 'v' || c == 'a')
        || (c == 'f'  || c == '?' || c == 'b')
        || (c >= '0' && c <= '7')
        || (c == 'U');
}

typedef uint32_t metac_location_ptr;

void MetaCLocationStorage_Init(metac_location_t_array* self)
{
    self->LocationCapacity = 128;
    self->LocationSize = 0;
    self->Locations = cast(metac_location_t*)
        calloc(sizeof(metac_location_t), self->LocationCapacity);
}

metac_location_ptr MetaCLocationStorage_Store(metac_location_t_array* self,
                                              metac_location_t loc)
{
#ifndef TEST_LEXER
    if (self->LocationSize >= self->LocationCapacity)
    {
        _newMemRealloc((void**)&self->Locations, &self->LocationCapacity, sizeof(metac_location_t));
    }
#endif

    assert(self->LocationSize < self->LocationCapacity);

    uint32_t result = self->LocationSize++;
    self->Locations[result] = loc;

    return result + 4;
}

void MetaCLocationStorage_EndLoc(
        metac_location_t_array* self,
        metac_location_ptr locationId,
        uint32_t line, uint16_t column)
{
#ifndef NO_LOCATION_TRACKING
    assert(locationId >= 4);
    assert((locationId - 4) < self->LocationSize);

    uint32_t idx = locationId - 4;
    metac_location_t *location =
        self->Locations + idx;

    assert((line - location->StartLine) < 0xFFFF);

    location->LineSpan = line - location->StartLine;
    assert(column < 0xFFFF);
    location->EndColumn = (uint16_t)column;
#endif
}

void MetaCLocation_Expand(metac_location_t* self, metac_location_t endLoc)
{
    self->LineSpan = (endLoc.StartLine + endLoc.LineSpan) - self->StartLine;
    self->EndColumn = endLoc.EndColumn;
}

metac_location_ptr MetaCLocationStorage_StartLoc(
        metac_location_t_array* self,
        uint32_t line, uint16_t column)
{
#ifndef NO_LOCATION_TRACKING
#  ifndef TEST_LEXER
    if (self->LocationSize >= self->LocationCapacity)
    {
        _newMemRealloc((void**)&self->Locations, &self->LocationCapacity, sizeof(metac_location_t));
    }
#  endif
    assert(self->LocationSize < self->LocationCapacity);

    uint32_t result = self->LocationSize++;

    self->Locations[result].StartLine = line;
    self->Locations[result].StartColumn = column;

    return result + 4;
#else
    return 0;
#endif
}

metac_location_t MetaCLocationStorage_FromPair(metac_location_t_array *srcStorage,
                                               metac_location_ptr startLocIdx,
                                               metac_location_ptr endLocIdx)
{
    metac_location_t result;

    result = srcStorage->Locations[startLocIdx - 4];
    const metac_location_t endLoc = srcStorage->Locations[endLocIdx - 4];

    result.LineSpan = (endLoc.StartLine + endLoc.LineSpan) - result.StartLine;
    result.EndColumn = endLoc.EndColumn;

    return result;
}

metac_token_t* MetaCLexerLexNextToken(metac_lexer_t* self,
                                      metac_lexer_state_t* state,
                                      const char* text, uint32_t len)

{
    static metac_token_t stop_token = {tok_eof};
    static metac_token_t err_token = {tok_error};
    metac_token_t token = {tok_invalid};
    if (text[len] != '\0')
    {
        return &err_token;
    }
    metac_token_t* result = 0;

    if (self->TokenCount >= self->TokenCapacity)
    {
        uint32_t newCapa = 32;

        if (self->Tokens == self->inlineTokens)
        {
            metac_token_t* newTokens = cast(metac_token_t*)
                malloc(sizeof(metac_token_t) * newCapa);
            metac_location_t* newLocations = cast(metac_location_t*)
                malloc(sizeof(metac_location_t) * newCapa);

            memcpy(newTokens, self->Tokens, sizeof(metac_token_t) * ARRAY_SIZE(self->inlineTokens));
            memcpy(newLocations, self->LocationStorage.Locations,
                sizeof(metac_location_t) * ARRAY_SIZE(self->inlineLocations));
            self->Tokens = newTokens;
            self->LocationStorage.Locations = newLocations;

            self->TokenCapacity = newCapa;
            self->LocationStorage.LocationCapacity = newCapa;
        }
        else
        {
            newCapa = ALIGN4(cast(uint32_t)(self->TokenCapacity * 1.3f));
            self->Tokens = cast(metac_token_t*)
                realloc(self->Tokens, sizeof(metac_token_t) * newCapa);
            self->TokenCapacity = newCapa;
            self->LocationStorage.Locations = cast(metac_location_t*)
                realloc(self->LocationStorage.Locations,
                        sizeof(metac_location_t) * newCapa);
            self->LocationStorage.LocationCapacity = newCapa;
        }
        // printf("Not enough token storage\n");
    }
    uint32_t eatenChars = 0;
    char c = *text++;
LcontinueLexnig:
    {
        uint32_t column = state->Column;
        // ignore all the invisible ascii codes
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
    metac_location_t loc = self->LocationStorage.Locations[token.LocationId - 4];

    if (c && (token.TokenType = MetaCLexFixedLengthToken(text)) == tok_invalid)
    {
        // const char* begin = text;
        if (c)
        {
            if (IsIdentifierChar(c))
            {
                uint32_t identifierLength = 0;
                uint32_t identifierHash = ~0;
                const char* identifierBegin = text;

                while ((c = *text++) && (IsIdentifierChar(c) || IsNumericChar(c)))
                {
#ifdef INCREMENTAL_HASH
                    identifierHash = crc32c_byte(identifierHash, c);
#endif
                    identifierLength++;
                    eatenChars++;
                }
                token.TokenType = tok_identifier;
                assert(identifierLength < 0xFFF);
                state->Column += eatenChars;
#ifndef INCREMENTAL_HASH
                identifierHash = crc32c_nozero(~0, identifierBegin, identifierLength);
#endif
                token.IdentifierKey =
                    IDENTIFIER_KEY(identifierHash, identifierLength);

                // You can take out keyword matching but it doesn't cost much anywys
                MetaCLexerMatchKeywordIdentifier(&token, identifierBegin);

                if(token.TokenType == tok_identifier)
                {
                    token.IdentifierPtr =
                        GetOrAddIdentifier(&self->IdentifierTable, token.IdentifierKey, identifierBegin);
                }
            }
            else if (IsNumericChar(c))
            {
                token.TokenType = tok_uint;
                bool isHex;
                uint64_t value;
                uint32_t initialPos = eatenChars;
//             LparseDigits:
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
                            ParseErrorF(loc, "invalid hex literal %.*s", 4, text - 1);
                            result = &err_token;
                            goto Lreturn;
                        }
                        c = *text++;
                        //printf("eaten_chars: %u -- C: %c\n", eatenChars, c);
                        goto LParseNumberDone;
                    }
                    else if (c >= '0' && c <= '7') // if an octal number follows
                    {
                        if (!ParseOctal(&text, &eatenChars, &value))
                        {
                            ParseErrorF(loc, "invalid octal literal %.*s", 4, text - 1);
                            result = &err_token;
                            goto Lreturn;
                        }
                        c = *text++;
                        goto LParseNumberDone;
                    }
                    else // nither x nor octal number follows 0
                    {
                        // nothing to do here value was already intialized to 0
                    }
                }

                while (c && IsNumericChar((c = *text++)))
                {
                    eatenChars++;
                    value *= 10;
                    value += c - '0';
                }
            LParseNumberDone:
                c |= 32;
                while (c == 'u' ||  c == 'l')
                {
                    eatenChars++;
                    c = (*text++ | 32);
                }
                token.ValueU64 = value;
                token.ValueLength = eatenChars - initialPos;
                state->Column += eatenChars;
            }
            else if (c == '\'')
            {
                uint32_t charLength = 0;
                text++;
                token.TokenType = tok_char;
                uint32_t charHash = ~0u;
                c = *text++;
                eatenChars++;
                if (c == '\'')
                {
                    ParseError(loc, "Empty character Literal");
                    result = &err_token;
                    goto Lreturn;
                }
                while(c && c != '\'')
                {
                    token.chars[charLength++] = c;
                    if (charLength > 8)
                    {
                        ParseError(loc, "Char literal too long.");
                        token.TokenType = tok_error;
                        goto Lreturn;
                    }

                    if (c == '\\')
                    {
                        c = *text++;
                        token.chars[charLength++] = c;
                        eatenChars++;
                        if (!IsValidEscapeChar(c))
                        {
                            ParseErrorF(loc, "Invalid escape seqeunce '%.*s'", 4, (text - 2));
                        }
                        if (c == 'U')
                        {
                            // start eating the chars after the /U
                            // since there might be up to eight
                            token.TokenType = tok_char_uni;
                            charLength = 0;
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
                token.charLength = charLength;
            }
            else if (c == '\"' || c == '`')
            {
                ++text;
                char matchTo = c;
                token.TokenType = tok_string;
                const char* stringBegin = text;
                uint32_t stringHash = ~0u;
                c = *text++;

                uint32_t column = state->Column;
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
                            ParseErrorF(loc, "Invalid escape seqeunce '%.*s'", 4, (text - 2));
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
                    ParseErrorF(loc, "Unterminated string literal '%.*s' \n", 10, text - eatenChars - 1);
                    result = &err_token;
                    goto Lreturn;
                }

                uint32_t stringLength = (eatenChars - eatenCharsAtStringStart);

                eatenChars++;
#ifndef INCREMENTAL_HASH
                stringHash = crc32c_nozero(~0, stringBegin, stringLength);
#endif
                assert(stringLength < 0xFFFFF);
                state->Column = column;
                token.Key = STRING_KEY(stringHash, stringLength);
                token.StringPtr = GetOrAddIdentifier(&self->StringTable, token.Key, stringBegin);
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
                    ParseErrorF(loc, "escaping '%c' in wild code '%.*s' \n", c, 8, text - 4);
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
            commentLength = len - eatenChars;
            c = '\0';
        }
        eatenChars += commentLength + !!newlinePtr;
        token.CommentLength = commentLength;
        token.CommentBegin = text;
        state->Column += commentLength + 1;
        state->Line += 1;
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
#if 0 // this is only active for as long as the parser can't do deal
      // with preprocessor stuff
    else if (token.TokenType == tok_hash)
    {
        uint32_t lines = 0;
        const char* newlinePtr = text + 1;
        uint32_t offset = 0;

    LskiptoNewline:
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
        //printf("skipping %u chars\n", offset);
        //printf("Just Skipped '%.*s'\n", offset,  text);
        //printf("NextChars 8: '%.*s'\n", 8,  text + offset);
        state->Line += lines;
        state->Column = 1;
        text += offset;
        state->Position += offset;
        if (state->Size > state->Position)
        {
            c = *text++;
        }
        else
        {
            c = '\0';
            token.TokenType = tok_eof;
        }
        goto LcontinueLexnig;
    }
#endif
    else
    {
        const uint32_t tokLen = MetaCStaticTokenLength(token.TokenType);
        eatenChars += tokLen;
        state->Column += tokLen;
    }

    MetaCLocationStorage_EndLoc(&self->LocationStorage,
        token.LocationId, state->Line, state->Column);
Lreturn:
    if (token.TokenType)
    {
        result = self->Tokens + self->TokenCount++;
        *result = token;
    }
    else
    {
        result = &stop_token;
    }

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
        "@",

        "(",
        ")",
        "{",
        "}",
        "[",
        "]",

        ";",
        ":",
        "$",
        "~",

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
//        "~=",

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

        "typedef",
        "auto",
//        "type",
        "void",

        "bool",
        "char",
        "short",
        "int",
        "long",

        "size_t",

        "float",
        "double",

        "signed",
        "unsigned",
        "const",
        "volatile",
        "__shared",
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
        "case",
        "default",
        "goto",
        "static",
        "inline",
        "if",
        "else",
        "break",
        "continue",
        "until",
        "yield",
        "__attribute__",


         "/*",
        "*/",
        "//",

        "++",
        "--",
        "[]",
        "##",
        "\n",

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
        if ( ((tok >= FIRST_KEYWORD_TOKEN(TOK_SELF)) & (tok <= LAST_KEYWORD_TOKEN(TOK_SELF))) )
        {
            metac_lexer_state_t state = {0};
            metac_token_t t1 = {tok_invalid};
            metac_lexer_t lexer;

            lexer.Tokens = &t1;
            lexer.TokenCapacity = 1;
            lexer.TokenCount = 0;

            metac_location_t l1;
            lexer.LocationStorage.Locations = &l1;
            lexer.LocationStorage.LocationCapacity = 1;
            lexer.LocationStorage.LocationSize = 0;

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
