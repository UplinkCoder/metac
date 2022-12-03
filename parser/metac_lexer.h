#ifndef _METAC_LEXER_H_
#define _METAC_LEXER_H_
#include "../os/compat.h"
#include "../os/bsf.h"

#include "metac_identifier_table.h"

typedef uint32_t block_idx_t;
typedef uint16_t crc32c_lower16_t;

typedef struct metac_lexer_state_t
{
    char const* Text;

    uint32_t Column;
    uint32_t Line;
    uint32_t Position;
    uint32_t Size;

    block_idx_t OuterBlock;
    uint16_t SourceId;
} metac_lexer_state_t;

#define TOK_SELF(TOK) \
    TOK

#define FIRST_KEYWORD_TOKEN(M) \
    M(tok_kw_struct)

#define LAST_KEYWORD_TOKEN(M) \
    M(tok_kw___attribute__)

#define FIRST_BINARY_TOKEN(M) \
    M(tok_comma)

#define LAST_BINARY_TOKEN(M) \
    M(tok_spaceship)

#define FOREACH_BINARY_TOKEN(M) \
    FIRST_BINARY_TOKEN(M) \
    M(tok_dot) \
    \
    M(tok_plus) \
    M(tok_minus) \
    M(tok_star) \
    M(tok_div) \
    M(tok_rem) \
    M(tok_xor) \
    M(tok_or) \
    M(tok_and) \
    M(tok_lsh) \
    M(tok_rsh) \
    \
    M(tok_oror) \
    M(tok_andand) \
    \
    M(tok_arrow) \
    M(tok_dotdot) \
    \
    M(tok_assign) \
    \
    M(tok_add_ass) \
    M(tok_sub_ass) \
    M(tok_mul_ass) \
    M(tok_div_ass) \
    M(tok_rem_ass) \
    M(tok_xor_ass) \
    M(tok_or_ass) \
    M(tok_and_ass) \
    M(tok_lsh_ass) \
    M(tok_rsh_ass) \
    \
    M(tok_equals_equals) \
    M(tok_not_equal) \
    M(tok_lt) \
    M(tok_le) \
    M(tok_gt) \
    M(tok_ge) \
    \
    LAST_BINARY_TOKEN(M)

#define FOREACH_KEYWORD_TOKEN(M) \
    FIRST_KEYWORD_TOKEN(M) \
    \
    M(tok_kw_union) \
    M(tok_kw_enum) \
    M(tok_kw_typedef) \
    \
    M(tok_kw_auto) \
    M(tok_kw_void) \
    M(tok_kw_bool) \
    M(tok_kw_char) \
    M(tok_kw_short) \
    M(tok_kw_int) \
    M(tok_kw_long) \
    M(tok_kw_size_t) \
    M(tok_kw_float) \
    M(tok_kw_double) \
    \
    M(tok_kw_signed) \
    M(tok_kw_unsigned) \
    M(tok_kw_const) \
    M(tok_kw_volatile) \
    M(tok_kw__shared) \
    M(tok_kw_extern) \
    \
    M(tok_kw_for) \
    M(tok_kw_sizeof) \
    M(tok_kw_return) \
    M(tok_kw_switch) \
    M(tok_kw_while) \
    M(tok_kw_do) \
    \
    M(tok_kw_typeof) \
    M(tok_kw_inject) \
    M(tok_kw_eject) \
    M(tok_kw_assert) \
    \
    M(tok_kw_case) \
    M(tok_kw_default) \
    M(tok_kw_goto) \
    M(tok_kw_static) \
    M(tok_kw_inline) \
    M(tok_kw_if) \
    M(tok_kw_else) \
    M(tok_kw_break) \
    M(tok_kw_continue) \
    M(tok_kw_until) \
    \
    M(tok_kw_yield) \
    \
    M(tok_kw__scope) \
    \
    LAST_KEYWORD_TOKEN(M)

#define FIRST_STATIC_TOKEN(M) \
    M(tok_bang)

#define LAST_STATIC_TOKEN(M) \
    M(tok_eof)

#define FOREACH_STATIC_TOKEN(M) \
    FIRST_STATIC_TOKEN(M) \
    \
    M(tok_question) \
    \
    M(tok_hash) \
    M(tok_at) \
    \
    M(tok_lParen) \
    M(tok_rParen) \
    M(tok_lBrace) \
    M(tok_rBrace) \
    M(tok_lBracket) \
    M(tok_rBracket) \
    \
    M(tok_semicolon) \
    M(tok_colon) \
    M(tok_dollar) \
    M(tok_tilde) \
    \
    FOREACH_BINARY_TOKEN(M) \
    \
    M(tok_dotdotdot) \
    \
    FOREACH_KEYWORD_TOKEN(M) \
    \
    M(tok_comment_begin_multi) \
    M(tok_comment_end_multi) \
    M(tok_comment_begin_single) \
    \
    M(tok_plusplus) \
    M(tok_minusminus) \
    M(tok_full_slice) \
    M(tok_hashhash) \
    \
    M(tok_newline) \
    \
    LAST_STATIC_TOKEN(M)

#define FIRST_TOKEN(M) \
    M(tok_invalid)

#define LAST_TOKEN(M) \
    M(tok_error)

#define FOREACH_TOKEN(M) \
    FIRST_TOKEN(M) \
    \
    M(tok_identifier) \
    M(tok_uint) \
    M(tok_float) \
    M(tok_double) \
    M(tok_string) \
    M(tok_char) \
    M(tok_char_uni) \
    M(tok_comment_single) \
    M(tok_comment_multi) \
    \
    M(tok_macro_parameter) \
    \
    FOREACH_STATIC_TOKEN(M) \
    \
    LAST_TOKEN(M)


#define WITH_COMMA(TOK) \
    TOK,

typedef enum metac_token_enum_t
{
    FOREACH_TOKEN(WITH_COMMA)
    tok_max
} metac_token_enum_t;

#undef WITH_COMMA

typedef struct metac_token_t {
    metac_token_enum_t TokenType;

    uint32_t Position;
    uint32_t LocationId;

    union { // switch(TokenType)
        uint32_t Key;
        // case tok_identfier :
        struct {
            uint32_t IdentifierKey;
            metac_identifier_ptr_t IdentifierPtr;
        };
        // case tok_string :
        struct {
            uint32_t StringKey;
            metac_identifier_ptr_t StringPtr;
        };
        // case tok_comment_begin, tok_comment_begin_single :
        struct {
            uint32_t CommentLength;
            const char* CommentBegin;
        };
        // case tok_char :
        struct {
            uint32_t charLength;
            char chars[8];
        };
        struct {
            union {
                uint64_t ValueU64;
                int64_t ValueI64;
                uint32_t ValueU32;
                int32_t ValueI32;
                float ValueF23;
                double ValueF52;
            };
            uint32_t ValueLength;
        };
        // case tok_macro_parameter:
        uint32_t MacroParameterIndex;
    };
} metac_token_t;

#define FOREACH_PARSE_NUMBER_FLAG(M) \
    M(parse_number_flag_none, 0) \
    M(parse_number_flag_hex, 1 << 0) \
    M(parse_number_flag_float, 1 << 1)

#define DEF_MEMBER(NAME, VALUE) \
    NAME = VALUE,

typedef enum parse_number_flag_t
{
    FOREACH_PARSE_NUMBER_FLAG(DEF_MEMBER)
} parse_number_flag_t;

#undef DEF_MEMBER

static const char* ParseNumberFlag_toChars(parse_number_flag_t flag)
{
    const char* result = 0;

    switch(flag)
    {
#define CASE(FLAG, VALUE) \
    case FLAG: result = #FLAG; break;
        FOREACH_PARSE_NUMBER_FLAG(CASE)
#undef CASE
    }
    return result;
}

typedef struct metac_token_buffer_t
{
    metac_token_t* Ptr;
    uint32_t Length;
    uint32_t Capacity;
} metac_token_buffer_t;

uint32_t MetaCTokenLength(metac_token_t token);


typedef struct metac_location_t
{
    uint32_t StartLine;

    uint16_t LineSpan;
    uint16_t StartColumn;
    uint16_t EndColumn;
    uint16_t SourceId;
} metac_location_t;


typedef struct metac_location_t_array
{
    metac_location_t* Locations;

    uint32_t LocationSize;
    uint32_t LocationCapacity;
} metac_location_t_array;

typedef uint32_t metac_location_ptr;

typedef struct metac_lexer_t {
    // inject array_methods(tokens, token_size, token_capacity);

    metac_token_t* Tokens;
    uint32_t TokenCount;
    uint32_t TokenCapacity;

    metac_location_t_array LocationStorage;

    metac_token_t       inlineTokens[16];
    metac_location_t inlineLocations[16];

    metac_identifier_table_t IdentifierTable;
    metac_identifier_table_t StringTable;

    metac_alloc_t* Allocator;
#if !defined(NO_PREPROCESSOR)
    bool InDefine;
#endif
} metac_lexer_t;


const char* MetaCTokenEnum_toChars(metac_token_enum_t tok);

#define ParseErrorF(LOC, MSG, ...) \
    fprintf(stderr, "ParseError[%s:%u]: {%u:%u}"  MSG  "\n", __FILE__, __LINE__, (LOC.StartLine), (LOC.StartColumn), __VA_ARGS__); \
    ParseErrorBreak();
#define ParseError(LOC, MSG) \
    fprintf(stderr, "ParseError[%s:%u]: {%u:%u}"  MSG  "\n", __FILE__, __LINE__, (LOC.StartLine), (LOC.StartColumn)); \
    ParseErrorBreak();

void MetaCLexer_Init(metac_lexer_t* self, metac_alloc_t* Allocator);

void MetaCLocation_Expand(metac_location_t* self, metac_location_t endLoc);
void MetaCLocationStorage_Init(metac_location_t_array* self);

metac_location_ptr MetaCLocationStorage_StartLoc(
        metac_location_t_array* self,
        uint32_t line, uint16_t column);

void MetaCLocationStorage_EndLoc(
        metac_location_t_array* self,
        metac_location_ptr locationId,
        uint32_t line, uint16_t column);

metac_location_t MetaCLocationStorage_FromPair(metac_location_t_array *srcStorage,
                                               metac_location_ptr startLocIdx,
                                               metac_location_ptr endLocIdx);

metac_location_ptr MetaCLocationStorage_Store(metac_location_t_array* self,
                                              metac_location_t loc);

metac_lexer_state_t MetaCLexerStateFromString(uint32_t sourceId, const char* str);
metac_lexer_state_t MetaCLexerStateFromBuffer(uint32_t sourceId, const char* buffer, uint32_t bufferLength);
metac_token_t* MetaCLexerLexNextToken(metac_lexer_t* self, metac_lexer_state_t* state,
                                      const char* text, uint32_t len);
void MetaCLexer_Free(metac_lexer_t* self);
#endif // _METAC_LEXER_H_
