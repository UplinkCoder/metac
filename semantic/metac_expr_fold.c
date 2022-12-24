#include "../semantic/metac_semantic.h"
#include "../semantic/metac_expr_fold.h"

#define CASE_(EXP) \
    case EXP:

bool IsConstant(metac_sema_state_t* sema, metac_sema_expr_t* expr)
{
    switch(expr->Kind)
    {
        default:
            return false;

        FOREACH_LITERAL_EXP(CASE_)
            return true;

        FOREACH_UNARY_EXP(CASE_)
            return IsConstant(sema, expr->E1);

        FOREACH_BINARY_EXP(CASE_)
            return (IsConstant(sema, expr->E1) && IsConstant(sema, expr->E2)); 
    }
}

#undef CASE_

typedef struct replacement_pair_t
{
    metac_node_t Parent;
    metac_node_t Child;
    metac_node_t ReplacementChild;
} replacement_pair_t;

typedef struct replacement_array_t
{
   ARENA_ARRAY(replacement_pair_t, ReplacementPairs)
} replacement_array_t;

typedef struct constant_fold_ctx_t
{
    replacement_array_t Map;
    metac_node_t* root;
} constant_fold_ctx_t;

void ConstantFold_Walker(metac_node_t node, void* constantFoldCtx)
{
    constant_fold_ctx_t* ctx = (constant_fold_ctx_t*) constantFoldCtx;
}

metac_sema_expr_t* ConstantFold_SubExps(metac_sema_expr_t* expr)
{
    return 0;
    // switch()
}