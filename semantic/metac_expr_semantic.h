#ifndef NO_FIBERS

#ifndef _METAC_EXPR_SEMANTIC_H_
#define _METAC_EXPR_SEMANTIC_H_
#include "../os/metac_task.h"

void MetaCSemantic_PushResultType(metac_sema_state_t* self, metac_type_index_t castType);

typedef struct MetaCSemantic_doExprSemantic_task_context_t
{
    metac_sema_state_t* Sema;
    metac_expr_t* Expr;
    metac_sema_expr_t* Result;
} MetaCSemantic_doExprSemantic_task_context_t;

void MetaCSemantic_doExprSemantic_Task(task_t* task);
#endif // _METAC_EXPR_SEMANTIC_H_
#endif // !NO_FIBERS

#define MetaCSemantic_doExprSemantic(SELF, NODE, RESULT) \
    MetaCSemantic_doExprSemantic_(SELF, ((metac_expr_t*)(NODE)), RESULT, \
                                  __FILE__, __LINE__)

static bool IsAggregateType(metac_type_index_kind_t typeKind);


metac_sema_expr_t* MetaCSemantic_doExprSemantic_(metac_sema_state_t* self,
                                                       metac_expr_t* expr,
                                                       metac_sema_expr_t* result,
                                                       const char* callFun,
                                                       uint32_t callLine);

void MetaCSemantic_PushExpr(metac_sema_state_t* self, metac_sema_expr_t* expr);
void MetaCSemantic_PopExpr(metac_sema_state_t* self,  metac_sema_expr_t* expr);

bool MetaCSemantic_CanHaveAddress(metac_sema_state_t* self, metac_sema_expr_t* expr);

typedef struct BCHeap BCHeap;

metac_sema_expr_t EvaluateExpr(metac_sema_state_t* sema,
                               metac_sema_expr_t* e,
                               BCHeap* heap);
