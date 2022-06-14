#ifndef _METAC_SEMANTIC_H_
#define _METAC_SEMANTIC_H_

#include "metac_semantic_lru.h"
#include "metac_parsetree.h"
#include "metac_parser.h"
#include "metac_type_table.h"
#include "metac_sematree.h"
#include "metac_scope.h"
//TODO get rid of exp_eval after testing
#include "repl/exp_eval.h"

#ifndef AT
#  define AT(...)
#endif

#define FOREACH_SEMA_STATE_ARRAY(SELF, M) \
    M(SELF, metac_sema_expression_t, Expressions) \
    M(SELF, sema_decl_variable_t, Variables) \
    M(SELF, sema_decl_function_t, Functions) \
    M(SELF, metac_scope_t, Scopes) \
    M(SELF, sema_stmt_block_t, BlockStatements) \
    M(SELF, metac_sema_statement_t, Statements)

#define DECLARE_ARRAY(UNUSED, TYPE_NAME, VAR) \
    TYPE_NAME* VAR; \
    uint32_t VAR##_size; \
    uint32_t VAR##_capacity;

#define INIT_ARRAY(SELF, TYPE_NAME, VAR) \
   SELF->VAR = (TYPE_NAME*)0; \
   SELF->VAR##_size = 0; \
   SELF->VAR##_capacity = 0;

void _newMemRealloc(void** memP, uint32_t* capacityP, const uint32_t elementSize);
//static uint32_t _nodeCounter = 1;

#ifndef ATOMIC
#define INC(v) \
    (v++)
#else
#define INC(v)
    (__builtin_atomic_fetch_add(&v, __ATOMIC_RELEASE))
#endif

/// TODO: lock during realloc
#define REALLOC_BOILERPLATE(VAR) \
if (VAR ## _capacity <= VAR ## _size) \
    { \
        _newMemRealloc( \
            ((void**)&VAR), \
            &VAR## _capacity, \
            sizeof(*VAR) \
        ); \
    }

/// TODO: lock during realloc
#define REALLOC_N_BOILERPLATE(VAR, N) \
if (VAR ## _capacity <= (VAR ## _size + (N))) \
    { \
        _newMemRealloc( \
            ((void**)&VAR), \
            &VAR## _capacity, \
            sizeof(*VAR) \
        ); \
    }

typedef struct metac_sema_decl_state_t
{
    sema_decl_variable_t* CurrentVariables;
    uint32_t CurrentVariableSize;
    uint32_t CurrentVariableCapacity;

    // uint32_t CurrentOffset;
} metac_sema_decl_state_t;

#define DECLARE_TYPE_TABLE(TYPE_NAME, MEMBER_NAME, INDEX_KIND) \
    METAC_TYPE_TABLE_T(TYPE_NAME) MEMBER_NAME;

#define FOREACH_TYPE_TABLE(M) \
    M(enum,         EnumTypeTable,             enum) \
    M(array,        ArrayTypeTable,           array) \
    M(aggregate,    StructTypeTable,         struct) \
    M(ptr,          PtrTypeTable,               ptr) \
    M(aggregate,    UnionTypeTable,           union) \
    M(typedef,      TypedefTypeTable,       typedef) \
    M(functiontype, FunctionTypeTable, functiontype) \
    M(tuple,        TupleTypeTable,           tuple)

typedef struct metac_semantic_state_t
{
    bool initialized;
    metac_identifier_table_t SemanticIdentifierTable;
    metac_identifier_table_t* ParserIdentifierTable;
    metac_identifier_table_t* ParserStringTable;

    // has state such as the current stack offset and the like
    // to layout variables
    AT(transient) metac_sema_decl_state_t* CurrentDeclarationState;

    AT(transient) uint32_t TemporaryScopeDepth;

    AT(transient) metac_scope_t* CurrentScope;

    AT(transient) metac_semantic_lru_t LRU;

    // metac_type_table_t* TypeTable;
    FOREACH_TYPE_TABLE(DECLARE_TYPE_TABLE)

    FOREACH_SEMA_STATE_ARRAY(NULL, DECLARE_ARRAY)

    AT(transient) metac_sema_expression_t* ExpressionStack;
    AT(transient) uint32_t ExpressionStackSize;
    AT(transient) uint32_t ExpressionStackCapacity;

    metac_type_aggregate_t* CompilerInterface;

    AT(valid_if, CurrentScope->Parent.Owner.Kind == scope_parent_function)
    uint32_t FrameOffset;
} metac_semantic_state_t;

#define SemanticError(STATE, MSG, ...) \
    fprintf(stderr, "SemanticError[%s:%u]: "  MSG  "\n", __FILE__, __LINE__, __VA_ARGS__)

void RegisterType(metac_semantic_state_t* state, decl_type_t* type);
const char* TypeToChars(metac_semantic_state_t* self, metac_type_index_t typeIndex);
void MetaCSemantic_Init(metac_semantic_state_t* self,
                        metac_parser_t* parser,
                        metac_type_aggregate_t* compilerStruct);

void MetaCSemantic_PushExpr(metac_semantic_state_t* self, metac_sema_expression_t* expr);
void MetaCSemantic_PopExpr(metac_semantic_state_t* self,  metac_sema_expression_t* expr);

#define MetaCSemantic_doExprSemantic(SELF, NODE) \
    MetaCSemantic_doExprSemantic_(SELF, ((metac_expression_t*)(NODE)), \
                                  __FUNCTION__, __LINE__)

metac_sema_expression_t* MetaCSemantic_doExprSemantic_(metac_semantic_state_t* self,
                                                       metac_expression_t* expr,
                                                       const char* callFun,
                                                       uint32_t callLine);

#define MetaCSemantic_doStatementSemantic(SELF, NODE) \
    MetaCSemantic_doStatementSemantic_(SELF, ((metac_statement_t*)(NODE)), \
                                       __FUNCTION__, __LINE__)

metac_sema_statement_t* MetaCSemantic_doStatementSemantic_(metac_semantic_state_t* self,
                                                           metac_statement_t* stmt,
                                                           const char* callFun,
                                                           uint32_t callLine);

#define MetaCSemantic_doDeclSemantic(SELF, NODE) \
    MetaCSemantic_doDeclSemantic_(SELF, ((metac_declaration_t*)(NODE)), \
                                  __FUNCTION__, __LINE__)

metac_sema_declaration_t* MetaCSemantic_doDeclSemantic_(metac_semantic_state_t* self,
                                                        metac_declaration_t* decl,
                                                        const char* callFun,
                                                        uint32_t callLine);

#define MetaCSemantic_doTypeSemantic(SELF, NODE) \
    MetaCSemantic_doTypeSemantic_(SELF, ((decl_type_t*)(NODE)), \
                                  __FUNCTION__, __LINE__)

metac_type_index_t MetaCSemantic_doTypeSemantic_(metac_semantic_state_t* self,
                                                decl_type_t* type,
                                                const char* callFun, uint32_t callLine);

/// Returns _emptyNode to signifiy it could not be found
/// a valid node otherwise
metac_node_header_t* MetaCSemantic_LookupIdentifier(metac_semantic_state_t* self,
                                                    metac_identifier_ptr_t identifierPtr);
#define Expression_IsEqual(A, B) \
    (A == B ? true : Expression_IsEqual_(A, B))
bool Expression_IsEqual_(metac_sema_expression_t* a,
                         metac_sema_expression_t* b);


metac_sema_expression_t* AllocNewSemaExpression(metac_semantic_state_t* self, metac_expression_t* expr);

sema_decl_function_t* AllocNewSemaFunction(metac_semantic_state_t* self,decl_function_t* func);

sema_decl_variable_t* AllocNewSemaVariable(metac_semantic_state_t* self, decl_variable_t *decl, metac_sema_declaration_t ** result_ptr);

sema_decl_variable_t* AllocFunctionParameters(metac_semantic_state_t* self, sema_decl_function_t* func,
                                              uint32_t parameterCount);

sema_decl_type_t* AllocNewSemaType(metac_semantic_state_t* self, metac_type_index_t typeIndex);

#define AllocNewAggregate(SELF, KIND) \
    (AllocNewAggregate_(SELF, KIND, __LINE__, __FILE__))
metac_type_aggregate_t* AllocNewAggregate_(metac_semantic_state_t* self, metac_declaration_kind_t kind, uint32_t line, const char* file);

metac_type_aggregate_field_t* AllocAggregateFields(metac_semantic_state_t* self,
                                                   metac_type_aggregate_t* aggregate,
                                                   metac_declaration_kind_t kind,
                                                   uint32_t fieldCount);
#define AllocNewSemaStatement(SELF, KIND, RESULT_PTR) \
    (sema_ ## KIND ## _t*) AllocNewSemaStatement_(SELF, KIND, sizeof(sema_ ## KIND ##_t), ((void**)(RESULT_PTR)))

metac_sema_statement_t* AllocNewSemaStatement_(metac_semantic_state_t* self,
                                               metac_statement_kind_t kind,
                                               size_t nodeSize, void** result_ptr);

sema_stmt_block_t* AllocNewSemaBlockStatement(metac_semantic_state_t* self,
                                              sema_stmt_block_t* Parent, uint32_t statementCount,
                                              void** result_ptr);

metac_scope_t* AllocNewScope(metac_semantic_state_t* self, metac_scope_t* parent, metac_scope_parent_t owner);
metac_type_array_t* AllocNewSemaArrayType(metac_semantic_state_t* self, metac_type_index_t elementTypeIndex, uint32_t dim);

void MetaCSemantic_Handoff(metac_semantic_state_t* self, metac_sema_declaration_t** declP,
                           metac_semantic_state_t* newOwner);

#define StatementIndex(SEMA, STMT) StatementIndex_(SEMA, (metac_sema_statement_t*)STMT)

uint32_t FunctionIndex(metac_semantic_state_t* self, sema_decl_function_t* func);
uint32_t StructIndex(metac_semantic_state_t* self, metac_type_aggregate_t* struct_);
uint32_t StatementIndex_(metac_semantic_state_t* self, metac_sema_statement_t* stmt);
uint32_t BlockStatementIndex(metac_semantic_state_t* self, sema_stmt_block_t* blockstmt);
uint32_t UnionIndex(metac_semantic_state_t* self, metac_type_aggregate_t* union_);
uint32_t TypedefIndex(metac_semantic_state_t* self, metac_type_typedef_t* typedef_);
uint32_t PtrTypeIndex(metac_semantic_state_t* self, metac_type_ptr_t* ptr);
uint32_t ArrayTypeIndex(metac_semantic_state_t* self, metac_type_array_t* array);
uint32_t FunctiontypeIndex(metac_semantic_state_t* self, metac_type_functiontype_t* functiontype);

sema_decl_function_t* FunctionPtr(metac_semantic_state_t* self, uint32_t index);
metac_type_aggregate_t* StructPtr(metac_semantic_state_t* self, uint32_t index);
metac_sema_statement_t* StatementPtr(metac_semantic_state_t* self, uint32_t index);
sema_stmt_block_t* BlockStatementPtr(metac_semantic_state_t* self, uint32_t index);
metac_type_aggregate_t* UnionPtr(metac_semantic_state_t* self, uint32_t index);
metac_type_typedef_t* TypedefPtr(metac_semantic_state_t* self, uint32_t index);
metac_type_ptr_t* PtrTypePtr(metac_semantic_state_t* self, uint32_t index);
metac_type_array_t* ArrayTypePtr(metac_semantic_state_t* self, uint32_t index);
metac_type_functiontype_t* FunctiontypePtr(metac_semantic_state_t* self, uint32_t index);

metac_scope_t* MetaCScope_PushNewScope(metac_semantic_state_t* sema,
                                       metac_scope_t* parent,
                                       metac_scope_parent_t owner);

#endif
