#ifndef NO_FIBERS

#include "metac_task.h"
typedef struct MetaCSemantic_doExprSemantic_task_context_t
{
    metac_semantic_state_t* Sema;
    metac_expression_t* Expr;
    metac_sema_expression_t* Result;
} MetaCSemantic_doExprSemantic_task_context_t;

void MetaCSemantic_doExprSemantic_Task(task_t* task);
#endif

#define MetaCSemantic_doExprSemantic(SELF, NODE, RESULT) \
    MetaCSemantic_doExprSemantic_(SELF, ((metac_expression_t*)(NODE)), RESULT, \
                                  __FILE__, __LINE__)

metac_sema_expression_t* MetaCSemantic_doExprSemantic_(metac_semantic_state_t* self,
                                                       metac_expression_t* expr,
                                                       metac_sema_expression_t* result,
                                                       const char* callFun,
                                                       uint32_t callLine);

void MetaCSemantic_PushExpr(metac_semantic_state_t* self, metac_sema_expression_t* expr);
void MetaCSemantic_PopExpr(metac_semantic_state_t* self,  metac_sema_expression_t* expr);

bool MetaCSemantic_CanHaveAddress(metac_semantic_state_t* self, metac_expression_t* expr);
