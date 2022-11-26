#include "../os/compat.h"
#include "metac_parsetree.h"

metac_expr_t* AllocNewExpression(metac_expr_kind_t kind);

#define ASSERT_VALID_DECL(DECL_P) \
            assert(((metac_declaration_t*)(DECL_P)) >= _newDecl_mem  \
                && ((metac_declaration_t*)(DECL_P)) <= (_newDecl_mem + _newDecl_size))

#define AllocNewDeclaration(KIND, RESULT_PTR) \
    (KIND ## _t*) AllocNewDeclaration_(KIND, sizeof(KIND ##_t), ((void**)(RESULT_PTR)), __LINE__)

metac_declaration_t* AllocNewDeclaration_(metac_declaration_kind_t kind, uint32_t nodeSize, void** result_ptr, uint32_t line);


#define AllocNewStatement(KIND, RESULT_PTR) \
    (KIND ## _t*) AllocNewStatement_(KIND, sizeof(KIND ##_t), ((void**)(RESULT_PTR)))

metac_stmt_t* AllocNewStatement_(metac_stmt_kind_t kind, uint32_t nodeSize, void** result_ptr);
