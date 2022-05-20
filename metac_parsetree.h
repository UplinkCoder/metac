#ifndef _METAC_PARSETREE_H_
#define _METAC_PARSETREE_H_

#include "compat.h"
#include "metac_identifier_table.h"
#include "metac_type_table.h"

#define FIRST_BINARY_EXP(M) \
    M(exp_comma)

#define LAST_BINARY_EXP(M) \
    M(exp_spaceship)

#define FIRST_DECL_TYPE(M) \
    M(decl_type)

#define LAST_DECL_TYPE(M) \
    M(decl_typedef)

#define FOREACH_DECL_KIND_(M) \
    M(decl_min) \
    \
    FOREACH_DECL_KIND(M) \
    \
    M(decl_max)

#define FOREACH_DECL_KIND(M) \
    M(decl_variable) \
    M(decl_field) \
    M(decl_parameter) \
    M(decl_enum_member) \
    \
    FIRST_DECL_TYPE(M) \
    M(decl_type_struct) \
    M(decl_type_union) \
    M(decl_type_enum) \
    M(decl_type_array) \
    M(decl_type_ptr) \
    M(decl_type_functiontype) \
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
    M(exp_sizeof) \
    M(exp_inject) \
    M(exp_eject) \
    M(exp_assert) \
    M(exp_outer) \
    M(exp_unary_dot) \
    M(exp_addr) \
    M(exp_ptr) \
    M(exp_not) \
    M(exp_compl) \
    M(exp_umin) \
    M(exp_paren) \
    M(exp_cast) \
    \
    FOREACH_BINARY_EXP(M) \
    \
    M(exp_full_slice) \
    M(exp_slice) \
    M(exp_index) \
    M(exp_call) \
    M(exp_argument) \
    \
    M(exp_addr_or_and) \
    M(exp_ptr_or_mul) \
    \
    M(exp_dot_compiler) \
    M(exp_dot_context) \
    M(exp_dot_target) \
    \
    M(exp_max)


#define FOREACH_STMT_KIND_(M) \
    M(stmt_min) \
    \
    FOREACH_STMT_KIND(M) \
    \
    M(stmt_max)

#define FOREACH_STMT_KIND(M) \
    M(stmt_block) \
    M(stmt_if) \
    M(stmt_switch) \
    M(stmt_while) \
    M(stmt_for) \
    M(stmt_do_while) \
    M(stmt_label) \
    M(stmt_case) \
    M(stmt_break) \
    M(stmt_yield) \
    M(stmt_scope) \
    M(stmt_continue) \
    M(stmt_goto) \
    M(stmt_return) \
    \
    M(stmt_exp) \
    M(stmt_decl)


#define FOREACH_NODE_KIND(M) \
    FOREACH_EXP(M) \
    FOREACH_STMT_KIND_(M) \
    FOREACH_DECL_KIND_(M) \
    M(max)

#define DEFINE_NODE_MEMBERS(MEMB) \
    node_ ## MEMB,

#pragma pack(push, 1)


#if 1
typedef enum metac_node_kind_t
{
    FOREACH_NODE_KIND(DEFINE_NODE_MEMBERS)
} metac_node_kind_t;

typedef struct metac_node_header_t
{
    metac_node_kind_t Kind;
    uint32_t LocationIdx;
    uint32_t Hash;
    uint32_t Serial;
} metac_node_header_t;
#endif

#undef DEFINE_NODE_MEMBERS

#define DEFINE_MEMBERS(MEMBER) \
    MEMBER,

typedef enum scope_kind_t
{
    scope_exit
} scope_kind_t;

#define EXP_SELF(EXP) \
    EXP

#define BIN_MEMBERS(MEMB) \
    bin_ ## MEMB,

typedef enum metac_expression_kind_t
{
    FOREACH_EXP(DEFINE_MEMBERS)
} metac_expression_kind_t;

typedef enum metac_statement_kind_t
{
    stmt_min = exp_max + 1,

    FOREACH_STMT_KIND(DEFINE_MEMBERS)

    stmt_max
} metac_statement_kind_t;

typedef enum metac_declaration_kind_t
{
    decl_min = stmt_max + 1,

    FOREACH_DECL_KIND(DEFINE_MEMBERS)

    decl_max
} metac_declaration_kind_t;

typedef enum metac_binary_expression_kind_t
{
    bin_exp_invalid = (FIRST_BINARY_EXP(EXP_SELF) - 1),

    FOREACH_BINARY_EXP(BIN_MEMBERS)
} metac_binary_expression_kind_t;

#define EXPRESSION_HEADER \
    metac_expression_kind_t Kind; \
    uint32_t LocationIdx; \
    uint32_t Hash; \
    uint32_t Serial;

typedef struct metac_expression_header_t
{
    EXPRESSION_HEADER
} metac_expression_header_t;

typedef struct exp_argument_t
{
    struct metac_expression_t* Expression;
    struct exp_argument_t* Next;
} exp_argument_t;

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
        // case exp_sizeof:
        struct {
            union {
                // struct metac_expression_t* SizeofExp;
                struct decl_type_t* SizeofType;
            };
        };
        // case exp_cast:
        struct {
            struct metac_expression_t* CastExp;
            struct decl_type_t* CastType;
        };

        // case exp_argument:
        exp_argument_t* arguments;
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

    struct metac_statement_t* Body;
    uint32_t StatementCount;
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

    metac_expression_t* Expression;
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

typedef struct stmt_for_t
{
    STATEMENT_HEADER

    metac_expression_t* ForInit;
    metac_expression_t* ForCond;
    metac_expression_t* ForPostLoop;

    struct metac_statement_t* ForBody;
} stmt_for_t;

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

    struct metac_expression_t* IfCond;
    struct metac_statement_t* IfBody;
    struct metac_statement_t* ElseBody;
} stmt_if_t;

typedef struct stmt_label_t
{
    STATEMENT_HEADER

    metac_identifier_ptr_t Label;
} stmt_label_t;

typedef struct stmt_return_t
{
    STATEMENT_HEADER

    metac_expression_t* Expression;
} stmt_return_t;

typedef struct stmt_switch_t
{
    STATEMENT_HEADER

    metac_expression_t* Expression;
} stmt_switch_t;

typedef struct stmt_do_while_t
{
    STATEMENT_HEADER

    metac_expression_t* Expression;
    struct metac_statement_t* Body;
} stmt_do_while_t;

#define MEMBER(KIND) \
    KIND##_t KIND;
typedef struct metac_statement_t
{
    union // switch(Kind)
    {
        struct {
            STATEMENT_HEADER
        };

        FOREACH_STMT_KIND(MEMBER);
    };
} metac_statement_t;

#define DECLARATION_HEADER \
    metac_declaration_kind_t DeclKind; \
    uint32_t LocationIdx; \
    uint32_t Hash; \
    uint32_t Serial;

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

    // only set if TypeKind == type_identifier
    metac_identifier_ptr_t TypeIdentifier;
} decl_type_t;

typedef struct decl_variable_t
{
    DECLARATION_HEADER

    decl_type_t* VarType;

    metac_identifier_ptr_t VarIdentifier;

    metac_expression_t* VarInitExpression;

} decl_variable_t;


typedef struct decl_field_t
{
    DECLARATION_HEADER

    decl_variable_t* Field;

    struct decl_field_t* Next;
} decl_field_t;


typedef struct decl_parameter_t
{
    DECLARATION_HEADER

    decl_variable_t* Parameter;

    struct decl_parameter_t* Next;
} decl_parameter_t;

typedef struct decl_parameter_list_t
{
    uint32_t ParameterCount;
    decl_parameter_t* List;
} decl_parameter_list_t;

typedef struct decl_function_t
{
    DECLARATION_HEADER

    decl_type_t* ReturnType;

    decl_parameter_t* Parameters;

    uint32_t ParameterCount;
    metac_identifier_ptr_t Identifier;

    stmt_block_t* FunctionBody;
} decl_function_t;

typedef struct decl_type_ptr_t
{
    DECLARATION_HEADER

    TYPE_HEADER

    decl_type_t* ElementType;
} decl_type_ptr_t;

typedef struct decl_enum_member_t
{
    DECLARATION_HEADER

    metac_identifier_ptr_t Name;

    metac_expression_t* Value;

    struct decl_enum_member_t* Next;
} decl_enum_member_t;

typedef struct decl_type_enum_t
{
    DECLARATION_HEADER

    TYPE_HEADER

    decl_enum_member_t* Members;

} decl_type_enum_t;

typedef struct decl_type_functiontype_t
{
    DECLARATION_HEADER

    TYPE_HEADER

    decl_type_t* ReturnType;

    /// this may or may not include identifiers
    decl_parameter_t* Parameters;

    uint32_t ParameterCount;
} decl_type_functiontype_t;

typedef struct decl_type_array_t
{
    DECLARATION_HEADER

    TYPE_HEADER

    decl_type_t* ElementType;

    metac_expression_t* Dim;
} decl_type_array_t;


typedef struct decl_type_struct_t
{
    DECLARATION_HEADER

    TYPE_HEADER

    metac_identifier_ptr_t Identifier;

    metac_identifier_ptr_t BaseIdentifier;

    struct decl_field_t* Fields;

    uint32_t FieldCount;
} decl_type_struct_t;

/// this is not used, it only exits
/// for tooling purposes
typedef struct decl_type_union_t
{
    DECLARATION_HEADER

    TYPE_HEADER

    metac_identifier_ptr_t Identifier;

    metac_identifier_ptr_t BaseIdentifier;

    struct decl_field_t* Fields;

    uint32_t FieldCount;
} decl_type_union_t;

typedef struct decl_typedef_t
{
    DECLARATION_HEADER

    decl_type_t* Type;

    metac_identifier_ptr_t Identifier;
} decl_typedef_t;

typedef struct metac_declaration_t
{
    union {
        struct {
            DECLARATION_HEADER
        };

        FOREACH_DECL_KIND(MEMBER)
    };
#undef MEMBER
} metac_declaration_t;
#pragma pack(pop)

#endif
