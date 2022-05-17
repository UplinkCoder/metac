#include "compat.h"
#include "metac_parsetree.h"
#include "metac_sematree.h"

metac_expression_t* AllocNewExpression(metac_expression_kind_t kind);

#define ASSERT_VALID_DECL(DECL_P) \
            assert(((metac_declaration_t*)(DECL_P)) >= _newDecl_mem  \
                && ((metac_declaration_t*)(DECL_P)) <= (_newDecl_mem + _newDecl_size))

#define AllocNewDeclaration(KIND, RESULT_PTR) \
    (KIND ## _t*) AllocNewDeclaration_(KIND, sizeof(KIND ##_t), ((void**)(RESULT_PTR)), __LINE__)

metac_declaration_t* AllocNewDeclaration_(metac_declaration_kind_t kind, size_t nodeSize, void** result_ptr, uint32_t line);


#define AllocNewStatement(KIND, RESULT_PTR) \
    (KIND ## _t*) AllocNewStatement_(KIND, sizeof(KIND ##_t), ((void**)(RESULT_PTR)))

metac_statement_t* AllocNewStatement_(metac_statement_kind_t kind, size_t nodeSize, void** result_ptr);
// ------------------------------------------- sema ---------------------------------------------------

metac_sema_expression_t* AllocNewSemaExpression(metac_expression_t* expr);

sema_decl_function_t* AllocNewSemaFunction(decl_function_t* func);

sema_decl_variable_t* AllocFunctionParameters(sema_decl_function_t* func,
                                              uint32_t parameterCount);

sema_type_aggregate_t* AllocNewAggregate(metac_type_kind_t kind);

metac_type_aggregate_field_t* AllocAggregateFields(sema_type_aggregate_t* aggregate,
                                                   metac_type_kind_t kind,
                                                   uint32_t fieldCount);

#define AllocNewSemaStatement(KIND, RESULT_PTR) \
    (sema_ ## KIND ## _t*) AllocNewSemaStatement_(KIND, sizeof(sema_ ## KIND ##_t), ((void**)(RESULT_PTR)))

metac_sema_statement_t* AllocNewSemaStatement_(metac_statement_kind_t kind,
                                               size_t nodeSize, void** result_ptr);

#define StatementIndex(STMT) StatementIndex_((metac_sema_statement_t*)STMT)

uint32_t StatementIndex_(metac_sema_statement_t* stmt);
uint32_t FunctionIndex(sema_decl_function_t* func);