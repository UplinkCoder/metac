#ifndef _METAL_LEXER_H_
#define _METAL_LEXER_H_
#include "compat.h"

typedef uint32_t block_idx_t;
typedef uint16_t crc32c_lower16_t;

typedef struct metal_lexer_state_t
{
    char const* Text;

    uint32_t Column;
    uint32_t Line;
    uint32_t Position;
    uint32_t Size;

    block_idx_t OuterBlock;
    uint16_t SourceId;

} metal_lexer_state_t;

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
    M(tok_comment_single) \
    M(tok_bang) \
    M(tok_addr) \
    M(tok_semicolon) \
    M(tok_colon) \
    M(tok_dollar) \
    M(tok_full_slice) \
    \
    M(tok_first_binary) \
    \
    M(tok_comma) \
    M(tok_arrow) \
    M(tok_dot) \
    M(tok_dotdot) \
    \
    M(tok_minus) \
    M(tok_plus) \
    M(tok_div) \
    M(tok_star) \
    \
    M(tok_cat) \
    M(tok_cat_ass) \
    \
    M(tok_assign) \
    M(tok_equalsEquals) \
    M(tok_notEqual) \
    M(tok_lessThan) \
    M(tok_lessEqual) \
    M(tok_greaterThan) \
    M(tok_greaterEqual) \
    M(tok_spaceship) \
    \
    M(tok_first_keyword) \
    \
    M(tok_kw_struct) \
    M(tok_kw_union) \
    M(tok_kw_type) \
    M(tok_kw_enum) \
    M(tok_kw_inject) \
    M(tok_kw_eject) \
    M(tok_kw_assert) \
    M(tok_kw_typedef) \
    \
    M(tok_eof) \
    M(tok_max)

#define WITH_COMMA(TOK) \
    TOK,

typedef enum metal_token_enum_t
{
    FOREACH_TOKEN(WITH_COMMA)
} metal_token_enum_t;

#undef WITH_COMMA

typedef struct metal_token_t {
    metal_token_enum_t TokenType;

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
} metal_token_t;


uint32_t MetalTokenLength(metal_token_t token);

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

typedef struct metal_lexer_t {
    // inject array_methods(tokens, token_size, token_capacity);

    metal_token_t* tokens;
    uint32_t tokens_size;
    uint32_t tokens_capacity;

    metal_token_t inlineTokens[2048];

} metal_lexer_t;


void InitMetalLexer(metal_lexer_t* self);
metal_lexer_state_t MetalLexerStateFromString(uint32_t sourceId, const char* str);
metal_lexer_state_t MetalLexerStateFromBuffer(uint32_t sourceId, const char* buffer, uint32_t bufferLength);
metal_token_t* MetalLexerLexNextToken(metal_lexer_t* self, metal_lexer_state_t* state,
                                      const char* text, uint32_t len);
#endif // _METAL_LEXER_H_
