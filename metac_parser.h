#ifndef _METAC_PARSER_H_
#define _METAC_PARSER_H_

#ifndef ACCEL
#  error "You must compile the parser with ACCEL set"
#  error "Known values are ACCEL_TABLE and ACCEL_TREE"
#else
#  if ACCEL == ACCEL_TABLE
#    include "metac_identifier_table.h"
#  elif ACCEL == ACCEL_TREE
#    include "metac_identifier_tree.h"
#  else
#    error "Unknow ACCEL value " #ACCEL
#  endif

#include "compat.h"
#include "metac_lexer.h"
#ifdef IDENTIFIER_TABLE
#  include "metac_identifier_table.h"
#endif

#ifdef IDENTIFIER_TREE
#  include "metac_identifier_tree.h"
#endif
/*    M(exp_bin_invalid) \*/

#define FIRST_BINARY_EXP(M) \
    M(exp_comma)

#define LAST_BINARY_EXP(M) \
    M(exp_spaceship)

#define FIRST_DECL_TYPE(M) \
    M(decl_type)

#define LAST_DECL_TYPE(M) \
    M(decl_typedef)

#define FOREACH_DECL_KIND(M) \
    M(decl_variable) \
    M(decl_field) \
    FIRST_DECL_TYPE(M) \
    \
    M(decl_struct) \
    M(decl_union) \
    M(decl_enum) \
    M(decl_functiontype) \
    LAST_DECL_TYPE(M) \
    \
    M(decl_function)

#define FOREACH_BINARY_EXP(M) \
    FIRST_BINARY_EXP(M) \
    FOREACH_BINARY_EXP_(M) \
    LAST_BINARY_EXP(M)

#define FOREACH_BINARY_EXP_(M) \
    M(exp_dot) \
    \
    M(exp_add) \
    M(exp_sub) \
    M(exp_mul) \
    M(exp_div) \
    M(exp_rem) \
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
    M(exp_arrow) \
    M(exp_dotdot) \
    \
    M(exp_assign) \
    \
    M(exp_add_ass) \
    M(exp_sub_ass) \
    M(exp_mul_ass) \
    M(exp_div_ass) \
    M(exp_rem_ass) \
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
    M(exp_ge)

#define FOREACH_EXP(M) \
    M(exp_invalid) \
    \
    M(exp_identifier) \
    M(exp_string) \
    M(exp_char) \
    M(exp_signed_integer) \
    M(exp_increment) \
    M(exp_decrement) \
    M(exp_post_increment) \
    M(exp_post_decrement) \
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


#define FOREACH_STMT_KIND(M) \
    M(stmt_min) \
    \
    FOREACH_STMT_KIND_(M) \
    \
    M(stmt_max)

#define FOREACH_STMT_KIND_(M) \
    M(stmt_block) \
    M(stmt_if) \
    M(stmt_switch) \
    M(stmt_while) \
    M(stmt_do_while) \
    M(stmt_label) \
    M(stmt_case) \
    M(stmt_break) \
    M(stmt_yield) \
    M(stmt_scope) \
    M(stmt_continue) \
    M(stmt_goto) \
    \
    M(stmt_exp) \
    M(stmt_decl)


#define FOREACH_NODE_KIND(M) \
    FOREACH_EXP(M) \
    FOREACH_STMT_KIND(M) \
    FOREACH_DECL_KIND(M) \
    M(node_max)

#define DEFINE_NODE_MEMBERS(MEMB) \
    node_ ## MEMB,

#if 1
typedef enum metac_node_kind_t
{
    FOREACH_NODE_KIND(DEFINE_NODE_MEMBERS)
} metac_node_kind_t;
#endif

#undef DEFINE_NODE_MEMBERS

#define DEFINE_MEMBERS(MEMBER) \
    MEMBER,

typedef enum scope_kind_t
{
    scope_exit
} scope_kind_t;

typedef enum metac_declaration_kind_t
{
    FOREACH_DECL_KIND(DEFINE_MEMBERS)
} metac_declaration_kind_t;

typedef enum metac_expression_kind_t
{
    FOREACH_EXP(DEFINE_MEMBERS)
} metac_expression_kind_t;


#define BIN_MEMBERS(MEMB) \
    bin_ ## MEMB,

typedef enum metac_binary_expression_kind_t
{
    bin_exp_invalid = (FIRST_BINARY_EXP(TOK_SELF) - 1),

    FOREACH_BINARY_EXP(BIN_MEMBERS)
} metac_binary_expression_kind_t;

#define EXPRESSION_HEADER \
    metac_expression_kind_t Kind; \
    uint32_t LocationIdx; \
    uint32_t Hash; \
    uint32_t Serial;

typedef struct metac_expression_t
{
    EXPRESSION_HEADER

    union // switch(Kind)
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
            uint32_t IdentifierKey;
#ifdef ACCEL
            metac_identifier_ptr_t IdentifierPtr;
#else
            const char* Identifier;
#endif
        };
        // case exp_string :
        struct {
            uint32_t StringKey;

#ifdef ACCEL
            metac_identifier_ptr_t StringPtr;
#else
            const char* String;
#endif
        };
        // case exp_char:
        struct {
            uint32_t CharKey;
            char Chars[8];
        };

        // case exp_signed_integer :
        int64_t ValueI64;
        // case exp_unsigned_integer :
        uint64_t ValueU64;
    };
} metac_expression_t;

typedef enum metac_statement_kind_t
{
    stmt_min = exp_max + 1,

    FOREACH_STMT_KIND_(DEFINE_MEMBERS)

    stmt_max
} metac_statement_kind_t;

#define STATEMENT_HEADER \
    metac_statement_kind_t StmtKind; \
    uint32_t LocationIdx; \
    uint32_t Hash; \
    uint32_t Serial; \
    struct metac_statement_t* Next;

typedef struct statement_header_t
{
    STATEMENT_HEADER
} statement_header_t;

typedef struct stmt_block_t
{
    STATEMENT_HEADER
} stmt_block_t;

typedef struct stmt_break_t
{
    STATEMENT_HEADER
} stmt_break_t;

typedef struct stmt_continue_t
{
    STATEMENT_HEADER
} stmt_continue_t;

typedef struct stmt_yield_t
{
    STATEMENT_HEADER

    metac_expression_t* E1;
} stmt_yield_t;

typedef struct stmt_scope_t
{
    STATEMENT_HEADER

    scope_kind_t ScopeKind;
    struct metac_statement_t* Stmt;
} stmt_scope_t;

typedef struct stmt_defer_t
{
    STATEMENT_HEADER

    struct metac_statement_t* Stmt;
} stmt_defer_t;

typedef struct stmt_while_t
{
    STATEMENT_HEADER

    metac_expression_t* E1;
} stmt_while_t;

typedef struct stmt_case_t
{
    STATEMENT_HEADER

    metac_expression_t* E1;
} stmt_case_t;

typedef struct stmt_goto_t
{
    STATEMENT_HEADER

    metac_identifier_ptr_t Label;
} stmt_goto_t;

typedef struct stmt_exp_t
{
    STATEMENT_HEADER

    metac_expression_t* Expression;
} stmt_exp_t;

typedef struct stmt_decl_t
{
    STATEMENT_HEADER

    struct metac_declaration_t* Declaration;
} stmt_decl_t;

typedef struct stmt_if_t
{
    STATEMENT_HEADER

    metac_expression_t* IfCond;
    struct metac_statement_t* IfBody;
    struct metac_statement_t* ElseBody;
} stmt_if_t;

typedef struct stmt_label_t
{
    STATEMENT_HEADER

    metac_identifier_ptr_t Label;
} stmt_label_t;

typedef struct stmt_switch_t
{
    STATEMENT_HEADER

    metac_identifier_ptr_t Label;
} stmt_switch_t;

typedef struct stmt_do_while_t
{
    STATEMENT_HEADER

    metac_expression_t* E1;
} stmt_do_while_t;

typedef struct metac_statement_t
{
    union // switch(Kind)
    {
        struct {
            STATEMENT_HEADER
        };

        // invalid case stmt_max, stmt_invalid :
        // case stmt_if :
        stmt_if_t stmt_if;
        // case stmt_exp :
        stmt_exp_t stmt_exp;
        // case stmt_block :
        stmt_block_t stmt_block;
        // case stmt_label :
        stmt_label_t stmt_label;
        // case stmt_goto :
        stmt_goto_t stmt_goto;
        // case stmt_yield :
        stmt_yield_t stmt_yield;
    };
} metac_statement_t;

#define DECLARATION_HEADER \
    metac_declaration_kind_t DeclKind; \
    uint32_t LocationIdx; \
    uint32_t Hash; \
    uint32_t Serial;

typedef enum metac_type_kind_t
{
    type_invalid,

    type_struct,
    type_union,
    type_enum,

    type_auto,
    type_type,
    type_void,

    type_char,
    type_short,
    type_int,
    type_long,

    type_float,
    type_double,

    type_identifier,
    type_max
} metac_type_kind_t;

typedef enum metac_type_modifiers
{
    typemod_none,

    typemod_const = (1 << 0),
    typemod_unsigned = (1 << 1),

} metac_type_modifiers;

#define TYPE_HEADER \
    metac_type_kind_t TypeKind; \
    metac_type_modifiers TypeModifiers;


typedef struct decl_type_t
{
    DECLARATION_HEADER

    TYPE_HEADER

    metac_identifier_ptr_t Identifier;
} decl_type_t;

typedef struct decl_field_t
{
    DECLARATION_HEADER

    decl_type_t* Type;

    metac_identifier_ptr_t Identifier;

    struct decl_field_t* Next;
} decl_field_t;

typedef struct decl_variable_t
{
    DECLARATION_HEADER

    decl_type_t* Type;

    metac_identifier_ptr_t Identifier;

} decl_variable_t;

typedef struct decl_struct_t
{
    DECLARATION_HEADER

    TYPE_HEADER

    metac_identifier_ptr_t Identifier;

    struct decl_field_t* Fields;

    uint32_t FieldCount;
} decl_struct_t;

typedef struct decl_typedef_t
{
    DECLARATION_HEADER

    struct metac_declaration_t* Type;

    metac_identifier_ptr_t Identifier;
} decl_typedef_t;

typedef struct metac_declaration_t
{
    union {
        struct {
            DECLARATION_HEADER
        };

        decl_typedef_t decl_typedef;
        decl_type_t decl_type;
        decl_struct_t decl_struct;
    };

} metac_declaration_t;



typedef struct metac_parser_reorder_state_t
{
    metac_expression_t* operandStack[1024];
    metac_expression_kind_t operatorStack[1024];

    uint32_t nOperands;
    uint32_t nOperators;
    uint32_t Depth;
} metac_parser_reorder_state_t;

typedef struct metac_define_t
{
    uint32_t IdentifierKey;
    metac_identifier_ptr_t IdentifierPtr;

    /// at which tokden positon the define is
    uint32_t TokenPosition;
    uint32_t SourceId;
    ///UINT32_MAX means variadic
    uint32_t NumberOfParameters;
} metac_define_t;

typedef struct metac_parser_t
{
    metac_lexer_t* Lexer;
    metac_lexer_state_t* LexerState;

    uint32_t CurrentTokenIndex;
    metac_parser_reorder_state_t* ExpressionReorderState;
#if ACCEL == ACCEL_TABLE
    metac_identifier_table_t IdentifierTable;
    metac_identifier_table_t StringTable;
#elif ACCEL == ACCEL_TREE
    metac_identifier_tree_t IdentifierTree;
    metac_identifier_tree_t StringTree;
#endif
    metac_define_t* Defines;
    uint32_t DefineCount;
    uint32_t DefineCapacity;

    metac_define_t inlineDefines[8];
} metac_parser_t;

extern metac_parser_t g_lineParser;

void MetaCParserInitFromLexer(metac_parser_t* self, metac_lexer_t* lexer);
metac_expression_t* MetaCParserParseExpression(metac_parser_t* self, metac_expression_t* prev);
metac_expression_t* MetaCParserParseExpressionFromString(const char* exp);

metac_declaration_t* MetaCParserParseDeclaration(metac_parser_t* self, metac_declaration_t* parent);

const char* PrintExpression(metac_parser_t* self, metac_expression_t* exp);
#undef DEFINE_MEMBERS
#endif
#endif // ifndef ACCEL
