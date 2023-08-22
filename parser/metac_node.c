#include "metac_node.h"

static inline bool MetaCNode_IsExpr(metac_node_t node)
{
    return (node->Kind > (metac_node_kind_t)node_expr_invalid
            && node->Kind < (metac_node_kind_t)node_expr_max);
}

static inline bool MetaCNode_IsStmt(metac_node_t node)
{
    return (node->Kind > (metac_node_kind_t)stmt_min
            && node->Kind < (metac_node_kind_t)stmt_max);
}

static inline bool MetaCNode_IsDecl(metac_node_t node)
{
    return (node->Kind > (metac_node_kind_t)decl_min
            && node->Kind < (metac_node_kind_t)decl_max);
}
