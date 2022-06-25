typedef struct MetaCSemantic_doExprSemantic_task_context_t
{
    metac_semantic_state_t* Sema;
    metac_expression_t* Expr;
    metac_sema_expression_t* Result;
} MetaCSemantic_doExprSemantic_task_context_t;

#define MetaCSemantic_doExprSemantic(SELF, NODE) \
    MetaCSemantic_doExprSemantic_(SELF, ((metac_expression_t*)(NODE)), \
                                  __FILE__, __LINE__)

metac_sema_expression_t* MetaCSemantic_doExprSemantic_(metac_semantic_state_t* self,
                                                       metac_expression_t* expr,
                                                       const char* callFile,
                                                       uint32_t callLine);

void MetaCSemantic_PushExpr(metac_semantic_state_t* self, metac_sema_expression_t* expr);
void MetaCSemantic_PopExpr(metac_semantic_state_t* self,  metac_sema_expression_t* expr);
