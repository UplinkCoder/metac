#include "../os/compat.h"

#define FIRST_BINARY_EXP(M) \
    M(expr_comma)

#define LAST_BINARY_EXP(M) \
    M(expr_spaceship)

#define FIRST_DECL_TYPE(M) \
    M(decl_type)

#define LAST_DECL_TYPE(M) \
    M(decl_type_typedef)

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
    M(decl_type_typeof) \
    M(decl_type_tuple) \
    LAST_DECL_TYPE(M) \
    \
    M(decl_function) \
    \
    M(decl_label) \
    M(decl_comment)

#define FOREACH_BINARY_EXP(M) \
    FIRST_BINARY_EXP(M) \
    FOREACH_BINARY_EXP_(M) \
    LAST_BINARY_EXP(M)

#define FOREACH_CMP_EXP(M) \
    M(expr_eq) \
    M(expr_neq) \
    M(expr_lt) \
    M(expr_le) \
    M(expr_gt) \
    M(expr_ge)

#define FOREACH_BIN_ARITH_EXP(M) \
    M(expr_add) \
    M(expr_sub) \
    M(expr_mul) \
    M(expr_div) \
    M(expr_rem) \
    M(expr_xor) \
    M(expr_or) \
    M(expr_and) \
    M(expr_lsh) \
    M(expr_rsh)

#define FOREACH_BIN_ARITH_ASSIGN_EXP(M) \
    M(expr_add_ass) \
    M(expr_sub_ass) \
    M(expr_mul_ass) \
    M(expr_div_ass) \
    M(expr_rem_ass) \
    M(expr_xor_ass) \
    M(expr_or_ass) \
    M(expr_and_ass) \
    M(expr_lsh_ass) \
    M(expr_rsh_ass)

#define FOREACH_BIN_LOGIC_EXP(M) \
    M(expr_oror) \
    M(expr_andand)

#define FOREACH_BINARY_EXP_(M) \
    M(expr_dot) \
    \
    FOREACH_BIN_ARITH_EXP(M) \
    \
    FOREACH_BIN_LOGIC_EXP(M) \
    \
    M(expr_arrow) \
    M(expr_dotdot) \
    \
    M(expr_assign) \
    \
    FOREACH_BIN_ARITH_ASSIGN_EXP(M) \
    \
    FOREACH_CMP_EXP(M)

#define FOREACH_LITERAL_EXP(M) \
    M(expr_string) \
    M(expr_char) \
    M(expr_signed_integer) \
    M(expr_float)

#define FOREACH_UNARY_EXP(M) \
    M(expr_increment) \
    M(expr_decrement) \
    M(expr_post_increment) \
    M(expr_post_decrement) \
    M(expr_typeof) \
    M(expr_sizeof) \
    M(expr_inject) \
    M(expr_eject) \
    M(expr_assert) \
    M(expr_outer) \
    M(expr_stringize) \
    M(expr_unary_dot) \
    M(expr_addr) \
    M(expr_deref) \
    M(expr_not) \
    M(expr_compl) \
    M(expr_umin)


#define FOREACH_EXP(M) \
    M(expr_invalid) \
    \
    M(expr_identifier) \
    \
    FOREACH_LITERAL_EXP(M) \
    \
    FOREACH_UNARY_EXP(M) \
    \
    M(expr_run) \
    \
    M(expr_paren) \
    M(expr_tuple) \
    M(expr_ternary) \
    M(expr_cast) \
    \
    FOREACH_BINARY_EXP(M) \
    \
    M(expr_full_slice) \
    M(expr_slice) \
    M(expr_index) \
    M(expr_call) \
    M(expr_argument) \
    M(expr_type) \
    \
    M(expr_template_instance) \
    \
    M(expr_variable) \
    M(expr_field) \
    M(expr_function) \
    \
    M(expr_unknown_value) \
    \
    M(expr_addr_or_and) \
    M(expr_deref_or_mul) \
    \
    M(expr_dot_compiler) \
    M(expr_dot_context) \
    M(expr_dot_target) \
    \
    M(expr_max)


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
    M(stmt_casebody) \
    M(stmt_break) \
    M(stmt_yield) \
    M(stmt_scope) \
    M(stmt_continue) \
    M(stmt_goto) \
    M(stmt_return) \
    \
    M(stmt_empty) \
    \
    M(stmt_exp) \
    M(stmt_decl) \
    M(stmt_comment)


#define FOREACH_NODE_KIND(M) \
    FOREACH_EXP(M) \
    FOREACH_STMT_KIND(M) \
    FOREACH_DECL_KIND(M)

/// Omits hash and serial
#define METAC_COPY_HEADER(SRCP, DSTP) do { \
    (DSTP)->Kind = (SRCP)->Kind; \
    (DSTP)->LocationIdx = (SRCP)->LocationIdx; \
} while (0);


#ifndef _METAC_NODE_H_
#define _METAC_NODE_H_

#define DEFINE_NODE_MEMBERS(MEMB) \
    node_ ## MEMB = MEMB,

#define DEFINE_MEMBERS(MEMBER) \
    MEMBER,

typedef enum scope_kind_t
{
    scope_exit
} scope_kind_t;

#define EXPR_SELF(EXP) \
    EXP

#define BIN_MEMBERS(MEMB) \
    bin_ ## MEMB,

typedef enum metac_expr_kind_t
{
    FOREACH_EXP(DEFINE_MEMBERS)
} metac_expr_kind_t;

typedef enum metac_stmt_kind_t
{
    stmt_min = expr_max + 1,

    FOREACH_STMT_KIND(DEFINE_MEMBERS)

    stmt_max
} metac_stmt_kind_t;

typedef enum metac_decl_kind_t
{
    decl_min = stmt_max + 1,

    FOREACH_DECL_KIND(DEFINE_MEMBERS)

    decl_max
} metac_decl_kind_t;

typedef enum metac_node_kind_t
{
    FOREACH_NODE_KIND(DEFINE_NODE_MEMBERS)
    node_max
} metac_node_kind_t;

#undef DEFINE_NODE_MEMBERS

typedef enum metac_binary_expr_kind_t
{
    bin_exp_invalid = (FIRST_BINARY_EXP(EXPR_SELF) - 1),

    FOREACH_BINARY_EXP(BIN_MEMBERS)
} metac_binary_expr_kind_t;

typedef struct metac_node_header_t
{
    metac_node_kind_t Kind;
    uint32_t LocationIdx;
    uint32_t Hash;
    uint32_t Serial;
} metac_node_header_t;

typedef metac_node_header_t* metac_node_t;
#define METAC_NODE(N) \
    (*(metac_node_t*)(&N))
#endif
