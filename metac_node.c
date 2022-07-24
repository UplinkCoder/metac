#ifndef _METAC_NODE_C_
#define _METAC_NODE_C_

#include "metac_node.h"
static inline bool MetaCNode_IsExpression(metac_node_t node)
{
    return (node->Kind > node_exp_invalid && node->Kind < node_exp_max);
}

static inline bool MetaCNode_IsStatement(metac_node_t node)
{
    return (node->Kind > stmt_min && node->Kind < stmt_max);
}

static inline bool MetaCNode_IsDeclaration(metac_node_t node)
{
    return (node->Kind > decl_min && node->Kind < decl_max);
}

#endif