#include "../semantic/metac_semantic.h"
#include "../semantic/metac_expr_fold.h"
#include "../semantic/metac_expr_semantic.h"

#define CASE_(EXP) \
    case EXP:

bool IsLiteral(metac_expr_kind_t kind)
{
    switch(kind)
    {
        default: return false;

        FOREACH_LITERAL_EXP(CASE_)
            return true;
    }
}

bool IsConstant(metac_sema_expr_t* expr)
{
    switch(expr->Kind)
    {
        default:
            return false;

        FOREACH_LITERAL_EXP(CASE_)
            return true;

        FOREACH_UNARY_EXP(CASE_)
            return IsConstant(expr->E1);

        FOREACH_BINARY_EXP(CASE_)
            return (IsConstant(expr->E1) && IsConstant(expr->E2));
    }
}

#undef CASE_

#if 0

static inline metac_sema_expr_t** FindInParentExpr(metac_sema_expr_t* parent,
                                                   metac_sema_expr_t* child)
{
    metac_sema_expr_t** result = 0;

    if (!parent || METAC_NODE(parent) == emptyNode)
    {
        // Nothing to do here
    }
    else
    {
        if ((IsUnaryExp(parent->Kind) | IsBinaryExp(parent->Kind) | (parent->Kind == expr_ternary)))
        {
            if (parent->E1 == child)
            {
                result = &parent->E1;
            }
        }

        if (((IsBinaryExp(parent->Kind)) | (parent->Kind == expr_ternary)))
        {
            if (parent->E2 == child)
            {
                result = &parent->E2;
            }
        }

        if (parent->Kind == expr_ternary)
        {
            if (parent->Econd == child)
            {
                result = &parent->Econd;
            }
        }
    }
    return result;
}

void ConstantFold_ReplaceInParent(metac_node_t parent,
                                  metac_sema_expr_t* child,
                                  metac_sema_expr_t replacement)
{
    metac_sema_expr_t** childP = FindInParentExpr((metac_sema_expr_t*)parent, child);
    if (childP)
    {
        **childP = replacement;
    }
}

void ConstantFold_AddReplacement(constant_fold_ctx_t* ctx,
                                 metac_sema_expr_t* e,
                                 metac_sema_expr_t replacement)
{
    assert(e->Hash != replacement.Hash);
    // uint32_t pair_hash = CRC32C_VALUE(e->Hash, replacement->Hash);
    replacement_pair_t p = { ctx->Parent, e, replacement };

    ARENA_ARRAY_ADD(ctx->Array.ReplacementPairs, p);
    //ConstantFold_ReplaceInParent(ctx->Parent, e, replacement);
}
#define PRINT_REPLACEMENTS 1

void ConstantFold_ApplyReplacements(constant_fold_ctx_t* ctx)
{
    uint32_t i;
    const replacement_pair_t* pairs = ctx->Array.ReplacementPairs;
    const uint32_t count = ctx->Array.ReplacementPairsCount;
#ifdef PRINT_REPLACEMENTS
    metac_printer_t printer;
    if (count)
    {
        // MetaCPrinter_Init(&printer, ctx->Sema->ParserIdentifierTable, ctx->Sema->ParserStringTable, ctx->Sema->DebugAlloc);
    }
#endif
    for(i = 0; i < count; i++)
    {
        const replacement_pair_t p = pairs[i];
#ifdef PRINT_REPLACEMENTS
        const char* orig = MetaCPrinter_PrintSemaNode(&printer, ctx->Sema, p.Child);
        const char* replaced = MetaCPrinter_PrintSemaNode(&printer, ctx->Sema, ((metac_node_t)&p.ReplacementChild));
        printf("%s -> %s\n", orig, replaced);
#endif
        ConstantFold_ReplaceInParent(p.Parent, p.Child, p.ReplacementChild);
    }

    if (count)
    {
        MetaCPrinter_Free(&printer);
    }
}

static inline int ConstantFold_WalkerFn(metac_node_t node, void* ctx)
{
    constant_fold_ctx_t* context =
        (constant_fold_ctx_t*) ctx;
    assert(crc32c_nozero(~0, __FUNCTION__, strlen(__FUNCTION__))
        == context->FunctionKey);

    bool stopWalking = 0;
    if (IsExprNode(node->Kind))
    {
        metac_sema_expr_t* e = (metac_sema_expr_t*) node;
        if (!IsLiteral(e->Kind) && IsConstant(e))
        {
            metac_sema_expr_t replacement =
                EvaluateExpr(context->Sema, e, &context->Heap);

            if (replacement.Hash != e->Hash)
            {
                metac_expr_t* exp = (metac_expr_t*) &replacement;
                ConstantFold_AddReplacement(context, e, replacement);
            }
            stopWalking = 1;
        }
        else
        {
            context->Parent = cast(metac_node_t)e;
        }
    }

    return stopWalking;
}

constant_fold_result_t ConstantFold_SubExps(metac_sema_state_t* sema, metac_sema_expr_t* expr)
{
    constant_fold_ctx_t ctx = { crc32c(~0, "ConstantFold_WalkerFn", strlen("ConstantFold_WalkerFn")) };
    metac_alloc_t foldAlloc = {0};
    constant_fold_result_t result;

    Allocator_Init(&foldAlloc, &sema->TempAlloc);

    ctx.Sema = sema;

    ARENA_ARRAY_INIT(replacement_pair_t, ctx.Array.ReplacementPairs, &foldAlloc);

    MetaCSemaTree_Walk(expr, sema, ConstantFold_WalkerFn, &ctx);

    if (ctx.Array.ReplacementPairsCount != 0)
    {
        printf("found %d viable constant replacements\n", ctx.Array.ReplacementPairsCount);
        ConstantFold_ApplyReplacements(&ctx);
    }

    result.Ctx = ctx;
    result.FoldAlloc = foldAlloc;

    return result;

}
#endif