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

typedef enum metal_token_enum_t {
    tok_invalid, // "invalid"

    tok_identifier, // "identifier"
    tok_unsignedNumber, // "unsigned number"
    // tok_signedNumber, // "signed number"
    tok_stringLiteral, // string_Literal

    tok_lParen, // "("
    tok_rParen, // ")"
    tok_lBrace, // "{"
    tok_rBrace, // "}"
    tok_lBracket, // "["
    tok_rBracket, // "]"

    tok_comment_begin, // "/*"
    tok_comment_end, // "*/"
    tok_comment_single, // "//"
    tok_bang, // "!"
    tok_addr, // "&"
    tok_semicolon,// ";"
    tok_colon, // ":"
    tok_dollar, // "$"
    tok_full_slice, // "[]"
    
    tok_first_binary,
    // binary ops
    tok_comma = tok_first_binary, // ","
    tok_dot, // "."
    tok_dotdot, // ".."
    
    tok_minus, // "-",
    tok_plus, // "+"
    tok_div, // "/"
    tok_star,// "*"
    
    tok_cat, // "~"
    tok_cat_ass, // "~="

    tok_assign, // "="
    tok_equalsEquals, // "=="
    tok_notEqual, // "!="
    tok_lessThan, //  "<"
    tok_lessEqual, // "<="
    
    tok_greaterThan, // ">"
    tok_greaterEqual, // ">="

    tok_spaceShip, // "<=>"
    // end binary ops

    tok_kw_struct, // "struct"
    tok_kw_union, // "union"
    tok_kw_type,  // "type"
    tok_kw_enum, // "enum"
    tok_kw_inject, // "inject"
    tok_kw_eject,// "eject"
    tok_kw_assert, // "assert"
    tok_kw_typedef, // "typedef"

    tok_eof, // "EOF"
    tok_max
} metal_token_enum_t;

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
    assert(capacity.name.length == capacity.name.length + "_capacity".length);
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
