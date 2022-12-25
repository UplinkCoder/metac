#include "metac_node.h"

static bool IsUnaryExp(metac_expr_kind_t kind)
{
    switch(kind)
    {
        case expr_umin:
        case expr_not:
        case expr_compl:
        case expr_decrement:
        case expr_increment:
        case expr_post_decrement:
        case expr_post_increment:
        case expr_paren:
        case expr_cast:
        case expr_deref:
        case expr_addr:
        case expr_sizeof:
        case expr_typeof:
            return true;
        default:
            return false;
    }
}

static inline bool MetaCNode_IsExpr(metac_node_t node)
{
    return (node->Kind > node_expr_invalid && node->Kind < node_expr_max);
}

static inline bool MetaCNode_IsStmt(metac_node_t node)
{
    return (node->Kind > stmt_min && node->Kind < stmt_max);
}

static inline bool MetaCNode_IsDecl(metac_node_t node)
{
    return (node->Kind > decl_min && node->Kind < decl_max);
}
