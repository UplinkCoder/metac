typedef struct replacement_pair_t
{
    metac_node_t Parent;
    metac_sema_expr_t* Child;
    metac_sema_expr_t ReplacementChild;
} replacement_pair_t;

typedef struct replacement_array_t
{
   ARENA_ARRAY(replacement_pair_t, ReplacementPairs)
} replacement_array_t;

typedef struct constant_fold_ctx_t
{
    uint32_t FunctionKey;
    replacement_array_t Array;
    metac_node_t Parent;
    metac_sema_state_t* Sema;
    BCHeap Heap;
} constant_fold_ctx_t;


typedef struct constant_fold_result_t
{
    metac_alloc_t FoldAlloc;
    constant_fold_ctx_t Ctx;
}  constant_fold_result_t;


bool IsConstant(metac_sema_state_t* sema, metac_sema_expr_t* e);

void ConstantFold_ReplaceInParent (metac_node_t parent, metac_sema_expr_t* child, metac_sema_expr_t replacement);

void ConstantFold_AddReplacement (constant_fold_ctx_t* ctx, metac_sema_expr_t* e, metac_sema_expr_t replacement);

void ConstantFold_ApplyReplacements (constant_fold_ctx_t* ctx);

constant_fold_result_t ConstantFold_SubExps(metac_sema_state_t* sema, metac_sema_expr_t* expr);
