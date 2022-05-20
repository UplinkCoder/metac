#ifndef _METAC_LEXER_H_
#define _METAC_LEXER_H_
#include "compat.h"

#define ACCEL_TABLE 1
#define ACCEL_TREE 2

#ifndef ACCEL
#else
# if ACCEL == ACCEL_TABLE
#  include "metac_identifier_table.h"
#  define IDENTIFIER_TABLE
#  define MEMBER_SUFFIX(X) X ## Table
#  define MEMBER_INFIX(P, S) P ## Table # S
#  define ACCELERATOR "Table"
#  define ACCEL_SUFFIX Table
#  define ACCEL_INIT(A, B) IdentifierTableInit(&((A).B ## Table))
# else
#  error "Unknown ACCEL value"
# endif
#endif

//#  define MEMBER_INIT(X) IdentifierTreeInit(## X ## ->IdentifierTree)


#define IDENTIFIER_PTR(TABLE, TOKEN) \
    IdentifierPtrToCharPtr(TABLE, (TOKEN).IdentifierPtr)

#define STRING_PTR(ACCEL_, PTR) \
    IdentifierPtrToCharPtr(MEMBER_SUFFIX(ACCEL_), (PTR))

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
    M(tok_kw___METAC__)

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
    M(tok_cat) \
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
    M(tok_cat_ass) \
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
    M(tok_kw___shared) \
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
    M(tok_kw_scope) \
    \
    M(tok_kw___LINE__) \
    M(tok_kw___FUNCTION__) \
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
    M(tok_string) \
    M(tok_char) \
    M(tok_char_uni) \
    M(tok_comment_single) \
    M(tok_comment_multi) \
    \
    FOREACH_STATIC_TOKEN(M) \
    \
    LAST_TOKEN(M)

#define WITH_COMMA(TOK) \
    TOK,

typedef enum metac_token_enum_t
{
    FOREACH_TOKEN(WITH_COMMA)
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
#ifdef ACCEL
            metac_identifier_ptr_t IdentifierPtr;
#else
            const char* Identifier;
#endif
        };
        // case tok_string :
        struct {
            uint32_t StringKey;
#ifdef ACCEL
            metac_identifier_ptr_t StringPtr;
#else
            const char* String;
#endif
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
        uint64_t ValueU64;
        int64_t ValueI64;
        uint32_t ValueU32;
        int32_t ValueI32;
        float ValueF23;
        double ValueF52;
    };
} metac_token_t;


uint32_t MetaCTokenLength(metac_token_t token);

/*
meta_var[2] c_array_helpers(meta_var ptr)
{
    meta_var size = (meta_var) {
        .name = concat(ptr.name, "_size");
        .type = uint32_t;
    };

    meta_var capacity = {
        .name = concat(ptr.name, "_capacity");
        .type = uint32_t;
    };

    return [size, capacity];
}
*/
/*
eject array_helpers_and_methods(meta_var ptr)
{
    meta_var[2] size_capa = c_array_helpers(ptr);
    eject size_capa[1];
    eject size_capa[2];
    eject array_methods(ptr, size_capa[1], size_capa[2]);
}


eject array_methods(meta_var ptr, meta_var size, meta_var capacity)
{
    assert(size.name.length == ptr.name.length + "_size".length);
    assert(capacity.name.length == ptr.name.length + "_capacity".length);
    assert(size.name[0 .. ptr.length] == ptr.name);
    assert(capacity.name[0 .. ptr.length] == ptr.name);

    assert(ptr.isMember())
    ptr.AddMethod("length",
        AddAnonymousFunction(i32, [],
            [ Return(size) ]
        );
    );
    auto catAssignMethod = ptr.AddMethod(void, "~=", [ptr.type.ElementType().ConstOf()]);
    catAssignMethod.SetBody([
        (fragment){
            assert ( $size < $capacity );
            *($ptr + $size) = $(catAssignMethod.args[0]);
         }
    ])

    );
}
*/

typedef struct metac_location_t
{
    uint32_t StartLine;

    uint16_t LineSpan;

    uint16_t StartColumn;
    uint16_t EndColumn;

    uint16_t SourceId;
} metac_location_t;


typedef struct metac_location_storage_t
{
    metac_location_t* Locations;

    uint32_t LocationSize;
    uint32_t LocationCapacity;

} metac_location_storage_t;

typedef struct metac_lexer_t {
    // inject array_methods(tokens, token_size, token_capacity);

    metac_token_t* Tokens;
    uint32_t TokenSize;
    uint32_t TokenCapacity;

    metac_location_storage_t LocationStorage;

    metac_token_t       inlineTokens[64];
    metac_location_t inlineLocations[64];
#if ACCEL == ACCEL_TABLE
    metac_identifier_table_t IdentifierTable;
    metac_identifier_table_t StringTable;
#elif ACCEL == ACCEL_TREE
    metac_identifier_tree_t IdentifierTree;
    metac_identifier_tree_t StringTree;
#endif
} metac_lexer_t;


#include <stdio.h>

const char* MetaCTokenEnum_toChars(metac_token_enum_t tok);

#define ParseErrorF(STATE, MSG, ...) \
    fprintf(stderr, "ParseError[%s:%u]: {%u:%u}"  MSG  "\n", __FILE__, __LINE__, (STATE ? STATE->Line : 0), (STATE ? STATE->Column : 0), __VA_ARGS__); \
    ParseErrorBreak();

#define ParseError(STATE, MSG) \
    fprintf(stderr, "ParseError[%s:%u]: {%u:%u}"  MSG  "\n", __FILE__, __LINE__, (STATE ? STATE->Line : 0), (STATE ? STATE->Column : 0)); \
    ParseErrorBreak();

void MetaCLexerInit(metac_lexer_t* self);
metac_lexer_state_t MetaCLexerStateFromString(uint32_t sourceId, const char* str);
metac_lexer_state_t MetaCLexerStateFromBuffer(uint32_t sourceId, const char* buffer, uint32_t bufferLength);
metac_token_t* MetaCLexerLexNextToken(metac_lexer_t* self, metac_lexer_state_t* state,
                                      const char* text, uint32_t len);
#endif // _METAC_LEXER_H_
