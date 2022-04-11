#include "compat.h"
#include "metal_lexer.h"

#define FOREACH_BINARY_EXP(M, ...) \
    M(exp_bin_invalid, _VA_ARGS_) \
    \
    M(exp_arrow, _VA_ARGS_) \
    M(exp_dot, _VA_ARGS_) \
    M(exp_dotdot, _VA_ARGS_) \
    \
    M(exp_add, _VA_ARGS_) \
    M(exp_sub, _VA_ARGS_) \
    M(exp_mul, _VA_ARGS_) \
    M(exp_div, _VA_ARGS_) \
    M(exp_cat, _VA_ARGS_) \
    M(exp_catass, _VA_ARGS_) \
    M(exp_assign, _VA_ARGS_) \
    \
    M(exp_eq, _VA_ARGS_) \
    M(exp_neq, _VA_ARGS_) \
    M(exp_lt, _VA_ARGS_) \
    M(exp_le, _VA_ARGS_) \
    M(exp_gt, _VA_ARGS_) \
    M(exp_ge, _VA_ARGS_) \
    M(exp_spaceship, _VA_ARGS_) \
    \
    M(exp_bin_max, _VA_ARGS_)

#define FOREACH_EXP(M, ...) \
    M(exp_invalid, _VA_ARGS_) \
    \
    M(exp_identifier,  _VA_ARGS_) \
    M(exp_string,  _VA_ARGS_) \
    M(exp_signed_integer, _VA_ARGS_) \
    M(exp_inject, _VA_ARGS_) \
    M(exp_eject,  _VA_ARGS_) \
    M(exp_assert, _VA_ARGS_) \
    M(exp_outer,  _VA_ARGS_) \
    M(exp_addr,  _VA_ARGS_) \
    M(exp_ptr,  _VA_ARGS_) \
    M(exp_paren,  _VA_ARGS_) \
    \
    FOREACH_BINARY_EXP(M, _VA_ARGS_) \
    \
    M(exp_full_slice, _VA_ARGS_) \
    M(exp_slice, _VA_ARGS_) \
    M(exp_call, _VA_ARGS_) \
    \
    M(exp_ptr_or_mul, _VA_ARGS_) \
    M(exp_max, _VA_ARGS_)


#define WITH_COMMA(S, ...) \
    S,

typedef enum metal_expression_type_t
{
    FOREACH_EXP(WITH_COMMA)
} metal_expression_type_t;

#undef WITH_COMMA

typedef struct metal_expression_t
{
    metal_expression_type_t Type;
    uint32_t LocationIdx;

    uint32_t Hash;

    union // switch(type)
    {
        // invalid case exp_max, exp_invalid :

        // case exp_add, exp_sub, exp_mul, exp_div, exp_cat, exp_catAss, exp_assign,
        // exp_lt, exp_gt, exp_le, exp_ge, exp_spaceShip :
        struct {
            struct metal_expression_t* _E1;
            struct metal_expression_t* E2;
        };
        // case  exp_inject, exp_eject, exp_assert, exp_outerParen, exp_outer :
        struct {
            struct metal_expression_t* E1;
        };
        // case identifier_exp :
        struct {
            union
            {
                struct {
                    crc32c_lower16_t Crc32CLw16_;
                    uint16_t Length_;
                } ;
                uint32_t IdentifierKey;
            };
            const char* Identifier;
        };
        // case exp_string :
        struct {
            union
            {
                struct {
                    crc32c_lower16_t Crc32CLw16;
                    uint16_t Length;
                } ;
                uint32_t StringKey;
            };
            const char* String;
        };

        // case exp_signed_integer :
        int64_t ValueI64;
        // case exp_unsigned_integer :
        uint64_t ValueU64;
    };
} metal_expression_t;

typedef enum metal_statement_type_t
{
    stmt_invalid = exp_max + 1,

    stmt_expression,

    stmt_max
} metal_statement_type_t;


typedef struct metal_parser_t
{
    metal_lexer_t* Lexer;
    metal_lexer_state_t* lexer_state;
    
    uint32_t CurrentTokenIndex;
} metal_parser_t;

void MetalParserInitFromLexer(metal_parser_t* self, metal_lexer_t* lexer);
metal_expression_t* ParseExpression(metal_parser_t* self, metal_expression_t* prev);
metal_expression_t* ParseExpressionFromString(const char* exp);
