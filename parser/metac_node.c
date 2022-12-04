#include "metac_node.h"

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
