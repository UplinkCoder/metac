#ifndef _METAC_ALLOC_NODE_H_
#define _METAC_ALLOC_NODE_H_

#include "compat.h"
#include "metac_parsetree.h"
#include "metac_sematree.h"


#define EXPR_PTR_V(IDX) \
    ((uint32_t) (IDX) | NodeKind_Expression << 29)

#define STMT_PTR_V(IDX) \
    ((uint32_t) (IDX) | NodeKind_Statement << 29)

#define DECL_PTR_V(IDX) \
    ((uint32_t) ((IDX) | NodeKind_Declaration << 29))

metac_expression_ptr_t AllocNewExpression(metac_expression_kind_t kind);
uint32_t ExpressionIndex(metac_expression_t* ptr);
metac_expression_t* ToExpressionPtr(metac_expression_ptr_t exp);
#define ASSERT_VALID_DECL(DECL_P) \
            assert(((metac_declaration_t*)(DECL_P)) >= _newDecl_mem  \
                && ((metac_declaration_t*)(DECL_P)) <= (_newDecl_mem + _newDecl_size))

#define AllocNewDeclaration(KIND, RESULT_PTR) \
    (KIND ## _t*) AllocNewDeclaration_(KIND, sizeof(KIND ##_t), ((void**)(RESULT_PTR)), __LINE__)

metac_declaration_t* AllocNewDeclaration_(metac_declaration_kind_t kind, uint32_t nodeSize, void** result_ptr, uint32_t line);


#define AllocNewStatement(KIND, RESULT_PTR) \
    (KIND ## _t*) AllocNewStatement_(KIND, sizeof(KIND ##_t), ((void**)(RESULT_PTR)))

metac_statement_t* AllocNewStatement_(metac_statement_kind_t kind, uint32_t nodeSize, void** result_ptr);
#endif