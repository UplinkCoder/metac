#include "compat.h"

typedef uint32_t block_idx_t;
typedef uint16_t crc32c_lower16_t;

typedef struct metal_lexer_state_t
{
    char const* text;
    const char* ptr;

    uint32_t col;
    uint32_t line;
    uint32_t pos;
    uint32_t size;

    block_idx_t outer_block;

} metal_lexer_state_t;

typedef enum metal_token_enum_t {
    tok_invalid, // "invalid"

    tok_identifier, // "identifier"
    tok_unsignedNumber, // "unsigned number"
    tok_signedNumber, // "signed number"
    tok_lParen, // "("
    tok_rParen, // ")"
    tok_lBrace, // "{"
    tok_rBrace, // "}"
    tok_lBracket, // "["
    tok_rBracket, // "]"

    tok_comma, // ","
    tok_dot, // "."
    tok_dotdot, // ".."
    tok_comment_begin, // "/*" */
    tok_comment_end, // "*/"
    tok_comment_single, // "//"
    tok_quote, // "\""
    tok_bang, // "!"
    tok_minus, // "-",
    tok_plus, // "+"
    tok_div, // "/"
    tok_star,// "*"
    tok_addr, // "&"

    tok_cat, // "~"
    tok_cat_ass, // "~="

    tok_semicolon,// ";"
    tok_colon,
    tok_dollar, // "$"
    tok_dollar_paren, // "$("
    tok_assign, // "="
    tok_equalsequals, // "=="
    tok_lessThan, //  "<"
    tok_lessEqual, // "<="
    tok_full_slice, // "[]"

    tok_greaterThan, // ">"
    tok_greaterEqual, // ">="

    tok_spaceShip, // "<=>"

    tok_kw_struct, // "struct"
    tok_kw_union, // "union"
    tok_kw_type,  // "type"
    tok_kw_enum, // "enum"
    tok_kw_inject, // "inject"
    tok_kw_eject,// "eject"
    tok_kw_assert, // "assert"
    tok_kw_typedef, // "typedef"

    tok_max
} metal_token_enum_t;

typedef struct metal_token_t {
    metal_token_enum_t TokenType;

    uint32_t pos;
    uint32_t source_id;
    union {
        struct {
            union
            {
                struct {
                    crc32c_lower16_t crc32_lw_16;
                    uint16_t length;
                } ;
                uint32_t identifier_key;
            };
            char* identfier;
        };
        uint64_t u64_value;
        int64_t i64_value;
        uint32_t u32_value;
        int32_t i32_value;
        float f23_value;
        double f52_value;
    };

    /// 0 means file scope
    /// otherwise it's the token_idx of the enclosing block
    uint32_t outer_scope;
} metal_token_t;



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

/*
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
