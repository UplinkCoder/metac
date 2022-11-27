#include "../os/compat.h"
#include "metac_parsetree.h"

metac_expr_t* AllocNewExpr(metac_expr_kind_t kind);

#define ASSERT_VALID_DECL(DECL_P) \
            assert(((metac_decl_t*)(DECL_P)) >= _newDecl_mem  \
                && ((metac_decl_t*)(DECL_P)) <= (_newDecl_mem + _newDecl_size))

#define AllocNewDecl(KIND, RESULT_PTR) \
    (KIND ## _t*) AllocNewDecl_(KIND, sizeof(KIND ##_t), ((void**)(RESULT_PTR)), __LINE__)

metac_decl_t* AllocNewDecl_(metac_decl_kind_t kind, uint32_t nodeSize, void** result_ptr, uint32_t line);


#define AllocNewStmt(KIND, RESULT_PTR) \
    (KIND ## _t*) AllocNewStmt_(KIND, sizeof(KIND ##_t), ((void**)(RESULT_PTR)))

metac_stmt_t* AllocNewStmt_(metac_stmt_kind_t kind, uint32_t nodeSize, void** result_ptr);
