#include "../os/compat.h"

#define FIRST_BINARY_EXP(M) \
    M(exp_comma)

#define LAST_BINARY_EXP(M) \
    M(exp_spaceship)

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
    M(exp_eq) \
    M(exp_neq) \
    M(exp_lt) \
    M(exp_le) \
    M(exp_gt) \
    M(exp_ge)

#define FOREACH_BIN_ARITH_EXP(M) \
    M(exp_add) \
    M(exp_sub) \
    M(exp_mul) \
    M(exp_div) \
    M(exp_rem) \
    M(exp_xor) \
    M(exp_or) \
    M(exp_and) \
    M(exp_lsh) \
    M(exp_rsh)

#define FOREACH_BIN_ARITH_ASSIGN_EXP(M) \
    M(exp_add_ass) \
    M(exp_sub_ass) \
    M(exp_mul_ass) \
    M(exp_div_ass) \
    M(exp_rem_ass) \
    M(exp_xor_ass) \
    M(exp_or_ass) \
    M(exp_and_ass) \
    M(exp_lsh_ass) \
    M(exp_rsh_ass)

#define FOREACH_BIN_LOGIC_EXP(M) \
    M(exp_oror) \
    M(exp_andand)

#define FOREACH_BINARY_EXP_(M) \
    M(exp_dot) \
    \
    FOREACH_BIN_ARITH_EXP(M) \
    \
    FOREACH_BIN_LOGIC_EXP(M) \
    \
    M(exp_arrow) \
    M(exp_dotdot) \
    \
    M(exp_assign) \
    \
    FOREACH_BIN_ARITH_ASSIGN_EXP(M) \
    \
    FOREACH_CMP_EXP(M)

#define FOREACH_EXP(M) \
    M(exp_invalid) \
    \
    M(exp_identifier) \
    M(exp_string) \
    M(exp_char) \
    M(exp_signed_integer) \
    M(exp_float) \
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
    M(exp_stringize) \
    M(exp_unary_dot) \
    M(exp_addr) \
    M(exp_deref) \
    M(exp_not) \
    M(exp_compl) \
    M(exp_umin) \
    M(exp_paren) \
    M(exp_tuple) \
    M(exp_ternary) \
    M(exp_cast) \
    \
    FOREACH_BINARY_EXP(M) \
    \
    M(exp_full_slice) \
    M(exp_slice) \
    M(exp_index) \
    M(exp_call) \
    M(exp_argument) \
    M(exp_type) \
    \
    M(exp_template_instance) \
    \
    M(exp_variable) \
    M(exp_field) \
    M(exp_function) \
    \
    M(exp_unknown_value) \
    \
    M(exp_addr_or_and) \
    M(exp_deref_or_mul) \
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

#define EXP_SELF(EXP) \
    EXP

#define BIN_MEMBERS(MEMB) \
    bin_ ## MEMB,

typedef enum metac_expr_kind_t
{
    FOREACH_EXP(DEFINE_MEMBERS)
} metac_expr_kind_t;

typedef enum metac_stmt_kind_t
{
    stmt_min = exp_max + 1,

    FOREACH_STMT_KIND(DEFINE_MEMBERS)

    stmt_max
} metac_stmt_kind_t;

typedef enum metac_declaration_kind_t
{
    decl_min = stmt_max + 1,

    FOREACH_DECL_KIND(DEFINE_MEMBERS)

    decl_max
} metac_declaration_kind_t;

typedef enum metac_node_kind_t
{
    FOREACH_NODE_KIND(DEFINE_NODE_MEMBERS)
    node_max
} metac_node_kind_t;

#undef DEFINE_NODE_MEMBERS

typedef enum metac_binary_expr_kind_t
{
    bin_exp_invalid = (FIRST_BINARY_EXP(EXP_SELF) - 1),

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
