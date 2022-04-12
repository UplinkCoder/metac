#ifndef _METAC_LEXER_H_
#define _METAC_LEXER_H_
#include "compat.h"

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
    M(tok_kw_typedef)

#define FIRST_BINARY_TOKEN(M) \
    M(tok_comma)

#define LAST_BINARY_TOKEN(M) \
    M(tok_spaceship)

#define FOREACH_BINARY_TOKEN(M) \
    FIRST_BINARY_TOKEN(M) \
    M(tok_arrow) \
    M(tok_dot) \
    M(tok_dotdot) \
    \
    M(tok_plus) \
    M(tok_plusplus) \
    M(tok_add_ass) \
    \
    M(tok_minus) \
    M(tok_minusminus) \
    M(tok_sub_ass) \
    \
    M(tok_div) \
    M(tok_div_ass) \
    \
    M(tok_xor) \
    M(tok_xor_ass) \
    \
    M(tok_or) \
    M(tok_oror) \
    M(tok_or_ass) \
    \
    M(tok_and) \
    M(tok_andand) \
    M(tok_and_ass) \
    \
    M(tok_star) \
    M(tok_mul_ass) \
    \
    M(tok_cat) \
    M(tok_cat_ass) \
    \
    M(tok_assign) \
    M(tok_equalsEquals) \
    M(tok_notEqual) \
    \
    M(tok_lessThan) \
    M(tok_lessEqual) \
    \
    M(tok_lsh) \
    M(tok_lsh_ass) \
    \
    M(tok_greaterThan) \
    M(tok_greaterEqual) \
    \
    M(tok_rsh) \
    M(tok_rsh_ass) \
    \
    LAST_BINARY_TOKEN(M)

#define FOREACH_KEYWORD_TOKEN(M) \
    FIRST_KEYWORD_TOKEN(M) \
    M(tok_kw_union) \
    M(tok_kw_type) \
    M(tok_kw_enum) \
    M(tok_kw_inject) \
    M(tok_kw_eject) \
    M(tok_kw_assert) \
    LAST_KEYWORD_TOKEN(M)

#define FOREACH_TOKEN(M) \
    M(tok_invalid) \
    \
    M(tok_identifier) \
    M(tok_unsignedNumber) \
    M(tok_stringLiteral) \
    \
    M(tok_first_static) \
    \
    M(tok_lParen) \
    M(tok_rParen) \
    M(tok_lBrace) \
    M(tok_rBrace) \
    M(tok_lBracket) \
    M(tok_rBracket) \
    \
    M(tok_comment_begin) \
    M(tok_comment_end) \
    \
    M(tok_comment_single) \
    M(tok_bang) \
    M(tok_semicolon) \
    M(tok_colon) \
    M(tok_dollar) \
    M(tok_full_slice) \
    \
    M(tok_first_binary) \
    \
    FOREACH_BINARY_TOKEN(M) \
    \
    M(tok_first_keyword) \
    \
    FOREACH_KEYWORD_TOKEN(M) \
    \
    M(tok_eof) \
    M(tok_max)

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
    uint32_t SourceId;
    union { // switch(TokenType)
        // case tok_identfier :
        struct {
            union
            {
                struct {
                    crc32c_lower16_t Crc32CLw16;
                    uint16_t Length;
                } ;
                uint32_t IdentifierKey;
            };
            const char* Identifier;
        };
        // case tok_string :
        struct {
            union
            {
                struct {
                    crc32c_lower16_t Crc32CLw16_;
                    uint16_t Length_;
                } ;
                uint32_t StringKey;
            };
            const char* String;
        };

        uint64_t ValueU64;
        int64_t ValueI64;
        uint32_t ValueU32;
        int32_t ValueI32;
        float ValueF23;
        double ValueF52;
    };

    /// 0 means file scope
    /// otherwise it's the token_idx of the enclosing block
    uint32_t outer_scope;
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

typedef struct metac_lexer_t {
    // inject array_methods(tokens, token_size, token_capacity);

    metac_token_t* tokens;
    uint32_t tokens_size;
    uint32_t tokens_capacity;

    metac_token_t inlineTokens[2048];

#ifdef IDENTIFIER_TABLE
    string_table_t IdentifierTable;
#endif
#ifdef STRING_LITERAL_TABLE
    string_table_t StringLiteralTable;
#endif
} metac_lexer_t;


void InitMetaCLexer(metac_lexer_t* self);
metac_lexer_state_t MetaCLexerStateFromString(uint32_t sourceId, const char* str);
metac_lexer_state_t MetaCLexerStateFromBuffer(uint32_t sourceId, const char* buffer, uint32_t bufferLength);
void MetaCLexerMatchKeywordIdentifier(metac_token_t*);
metac_token_t* MetaCLexerLexNextToken(metac_lexer_t* self, metac_lexer_state_t* state,
                                      const char* text, uint32_t len);
#endif // _METAC_LEXER_H_
