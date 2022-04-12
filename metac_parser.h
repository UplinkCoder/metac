#include "compat.h"
#include "metac_lexer.h"

/*    M(exp_bin_invalid) \*/

#define FIRST_BINARY_EXP(M) \
    M(exp_comma)

#define LAST_BINARY_EXP(M) \
    M(exp_spaceship)

#define FOREACH_BINARY_EXP(M) \
    \
    FIRST_BINARY_EXP(M) \
    M(exp_arrow) \
    M(exp_dot) \
    M(exp_dotdot) \
    \
    M(exp_add) \
    M(exp_sub) \
    M(exp_mul) \
    M(exp_div) \
    M(exp_xor) \
    M(exp_or) \
    M(exp_and) \
    M(exp_cat) \
    M(exp_lsh) \
    M(exp_rsh) \
    \
    M(exp_oror) \
    M(exp_andand) \
    \
    M(exp_assign) \
    \
    M(exp_add_ass) \
    M(exp_sub_ass) \
    M(exp_mul_ass) \
    M(exp_div_ass) \
    M(exp_xor_ass) \
    M(exp_or_ass) \
    M(exp_and_ass) \
    M(exp_cat_ass) \
    M(exp_lsh_ass) \
    M(exp_rsh_ass) \
    \
    M(exp_eq) \
    M(exp_neq) \
    M(exp_lt) \
    M(exp_le) \
    M(exp_gt) \
    M(exp_ge) \
    LAST_BINARY_EXP(M)

#define FOREACH_EXP(M) \
    M(exp_invalid) \
    \
    M(exp_identifier) \
    M(exp_string) \
    M(exp_signed_integer) \
    M(exp_typeof) \
    M(exp_inject) \
    M(exp_eject) \
    M(exp_assert) \
    M(exp_outer) \
    M(exp_addr) \
    M(exp_ptr) \
    M(exp_paren) \
    \
    FOREACH_BINARY_EXP(M) \
    \
    M(exp_full_slice) \
    M(exp_slice) \
    M(exp_call) \
    \
    M(exp_addr_or_and) \
    M(exp_ptr_or_mul) \
    \
    M(exp_max)


#define WITH_COMMA(S) \
    S,

typedef enum metac_expression_type_t
{
    FOREACH_EXP(WITH_COMMA)
} metac_expression_type_t;

#undef WITH_COMMA

typedef struct metac_expression_t
{
    metac_expression_type_t Type;
    uint32_t LocationIdx;

    uint32_t Hash;

    union // switch(type)
    {
        // invalid case exp_max, exp_invalid :

        // case exp_add, exp_sub, exp_mul, exp_div, exp_cat, exp_catAss, exp_assign,
        // exp_lt, exp_gt, exp_le, exp_ge, exp_spaceShip :
        struct {
            struct metac_expression_t* _E1;
            struct metac_expression_t* E2;
        };
        // case  exp_inject, exp_eject, exp_assert, exp_outerParen, exp_outer :
        struct {
            struct metac_expression_t* E1;
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
} metac_expression_t;

typedef enum metac_statement_type_t
{
    stmt_invalid = exp_max + 1,

    stmt_expression,

    stmt_max
} metac_statement_type_t;

typedef struct metac_parser_reorder_state_t
{
    metac_expression_t* operandStack[1024];
    metac_expression_type_t operatorStack[1024];
    uint32_t nOperands;
    uint32_t nOperators;
    uint32_t Depth;
} metac_parser_reorder_state_t;

typedef struct metac_parser_t
{
    metac_lexer_t* Lexer;
    metac_lexer_state_t* lexer_state;

    uint32_t CurrentTokenIndex;
    metac_parser_reorder_state_t* ExpressionReorderState;

} metac_parser_t;

void MetaCParserInitFromLexer(metac_parser_t* self, metac_lexer_t* lexer);
metac_expression_t* ParseExpression(metac_parser_t* self, metac_expression_t* prev);
metac_expression_t* ParseExpressionFromString(const char* exp);