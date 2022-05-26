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

sema_decl_variable_t* AllocNewSemaVariable(decl_variable_t *decl, metac_sema_declaration_t ** result_ptr);

sema_decl_variable_t* AllocFunctionParameters(sema_decl_function_t* func,
                                              uint32_t parameterCount);

sema_decl_type_t* AllocNewSemaType(metac_type_index_t typeIndex);

metac_type_aggregate_t* AllocNewAggregate(metac_type_kind_t kind, decl_type_struct_t* decl);

metac_type_aggregate_field_t* AllocAggregateFields(metac_type_aggregate_t* aggregate,
                                                   metac_type_kind_t kind,
                                                   uint32_t fieldCount);
#define AllocNewSemaStatement(KIND, RESULT_PTR) \
    (sema_ ## KIND ## _t*) AllocNewSemaStatement_(KIND, sizeof(sema_ ## KIND ##_t), ((void**)(RESULT_PTR)))

metac_sema_statement_t* AllocNewSemaStatement_(metac_statement_kind_t kind,
                                               size_t nodeSize, void** result_ptr);

sema_stmt_block_t* AllocNewSemaBlockStatement(sema_stmt_block_t* Parent, uint32_t statementCount,
                                              void** result_ptr);

metac_scope_t* AllocNewScope(metac_scope_t* parent, metac_scope_parent_t owner);

#define StatementIndex(STMT) StatementIndex_((metac_sema_statement_t*)STMT)

uint32_t FunctionIndex(sema_decl_function_t* func);
uint32_t StructIndex(metac_type_aggregate_t* struct_);
uint32_t StatementIndex_(metac_sema_statement_t* stmt);
uint32_t BlockStatementIndex(sema_stmt_block_t* blockstmt);
uint32_t UnionIndex(metac_type_aggregate_t* union_);

sema_decl_function_t* FunctionPtr(uint32_t index);
metac_type_aggregate_t* StructPtr(uint32_t index);
metac_sema_statement_t* StatementPtr(uint32_t index);
sema_stmt_block_t* BlockStatementPtr(uint32_t index);
metac_type_aggregate_t* UnionPtr(uint32_t index);