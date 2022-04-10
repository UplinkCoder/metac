#include "compat.h"
#include "metal_lexer.h"

typedef enum metal_expression_type_t
{
    exp_invalid,

    exp_add,
    exp_sub,
    exp_mul,
    exp_div,
    exp_cat,
    exp_catAss,
    exp_assign,
    exp_lt,
    exp_gt,
    exp_le,
    exp_ge,
    exp_spaceShip,

    exp_string,
    exp_signed_integer,
    exp_inject,
    exp_eject,
    exp_assert,
    exp_outerParen,
    exp_outer,

    exp_max
} metal_expression_type_t;

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
    metal_lexer_t* lexer;

    uint32_t currentTokenIndex;
} metal_parser_t;

void MetalParserInitFromLexer(metal_parser_t* self, metal_lexer_t* lexer);
metal_expression_t* parseExpression(metal_parser_t* self, metal_expression_t* prev);
metal_expression_t* parseExpressionFromString(const char* exp);
