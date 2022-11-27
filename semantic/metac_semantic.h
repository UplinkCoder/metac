#ifndef _METAC_SEMANTIC_H_
#define _METAC_SEMANTIC_H_

#include "../os/metac_alloc.h"
#include "metac_semantic_lru.h"
#include "../parser/metac_parsetree.h"
#include "../parser/metac_parser.h"
#include "metac_type_table.h"
#include "metac_sematree.h"
#include "metac_scope.h"

#include "../os/metac_coro.h"
#include "../os/metac_atomic.h"
#include "../3rd_party/rwlock.h"

#ifndef AT
#  define AT(...)
#endif

#define FOREACH_SEMA_STATE_ARRAY(SELF, M) \
    M(SELF, metac_sema_expr_t, Expressions) \
    M(SELF, sema_decl_variable_t, Variables) \
    M(SELF, sema_decl_function_t, Functions) \
    M(SELF, metac_scope_t, Scopes) \
    M(SELF, sema_stmt_block_t, BlockStatements) \
    M(SELF, metac_sema_stmt_t, Statements)

#define DECLARE_ARENA_STATE_ARRAY(UNUSED, TYPE_NAME, VAR) \
    metac_alloc_t VAR##Allocator; \
    ARENA_ARRAY(TYPE_NAME, VAR)

#define INIT_ARENA_STATE_ARRAY(SELF, TYPE_NAME, VAR) \
    SELF->VAR = (TYPE_NAME*) 0; \
    Allocator_Init(&SELF->VAR##Allocator, &(SELF->Allocator)); \
    ARENA_ARRAY_INIT(TYPE_NAME, SELF->VAR, &(SELF->VAR##Allocator));

metac_noinline void _newMemRealloc(void** memP, uint32_t* capacityP, const uint32_t elementSize);
//static uint32_t _nodeCounter = 1;

typedef struct metac_semantic_waiter_t
{
    uint32_t FuncHash;
    uint32_t NodeHash;
#ifndef NO_FIBERS
	aco_t* Continuation;
#endif
} metac_semantic_waiter_t;

typedef struct metac_semantic_waitlist_t
{
    RWLock WaiterLock;
    metac_semantic_waiter_t* Waiters;
    uint32_t WaiterCount;
    uint32_t WaiterCapacity;
} metac_semantic_waitlist_t;

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

#define FOREACH_ON_RESOLVE_FAIL(M) \
    M(OnResolveFail_Invalid)       \
    M(OnResolveFail_Yield)         \
    M(OnResolveFail_ReturnNull)    \
    M(OnResolveFail_ReturnError)

#define DEFINE_MEMBER(MEMBER) MEMBER,

typedef enum metac_semantic_on_resolve_fail_t
{
    FOREACH_ON_RESOLVE_FAIL(DEFINE_MEMBER)
    OnResolveFail_Max
} metac_semantic_on_resolve_fail_t;

#undef DEFINE_MEMBER

typedef struct metac_switch_state_t
{
    //ARENA_ARRAY(sema_stmt_case_t*, PendingCases)

    metac_sema_expr_t* SwitchExp;
} metac_switch_state_t;

typedef struct metac_semantic_state_t
{
    bool initialized;
    metac_alloc_t Allocator;
    metac_alloc_t TempAlloc;

    metac_identifier_table_t SemanticIdentifierTable;
    metac_identifier_table_t* ParserIdentifierTable;
    metac_identifier_table_t* ParserStringTable;

    metac_semantic_waitlist_t Waiters;

    AT(per_function) uint32_t nLocals;

    // has state such as the current stack offset and the like
    // to layout variables
    AT(transient) metac_sema_decl_state_t* CurrentDeclState;

    AT(transient) uint32_t TemporaryScopeDepth;

    AT(TaskLocal) AT(transient) metac_scope_t* CurrentScope;

    AT(TaskLocal) AT(transient) metac_semantic_lru_t LRU;

    // metac_type_table_t* TypeTable;
    FOREACH_TYPE_TABLE(DECLARE_TYPE_TABLE)

    FOREACH_SEMA_STATE_ARRAY(NULL, DECLARE_ARENA_STATE_ARRAY)

    AT(TaskLocal) AT(transient) metac_scope_t* MountParent;

    AT(transient) metac_sema_expr_t* ExpressionStack;
    AT(transient) uint32_t ExpressionStackSize;
    AT(transient) uint32_t ExpressionStackCapacity;

    AT(transient) sema_stmt_switch_t** SwitchStack;
    AT(transient) uint32_t SwitchStackSize;
    AT(transient) uint32_t SwitchStackCapacity;

    ARENA_ARRAY(metac_scope_t*, DeclStatementScope)

    metac_type_aggregate_t* CompilerInterface;
    sema_decl_variable_t CompilerVariable;

    ARENA_ARRAY(metac_sema_decl_t*, Globals)
    ARENA_ARRAY(metac_semantic_on_resolve_fail_t, OnResolveFailStack)
} metac_semantic_state_t;

bool IsUnresolved(metac_node_t node);

/// Sets the behavior for name-resolve failing
void MetaCSemantic_PushOnResolveFail(metac_semantic_state_t* self,
                                     metac_semantic_on_resolve_fail_t onFail);

// Resets the behavior for the case of a name-resolve failing
void MetaCSemantic_PopOnResolveFail(metac_semantic_state_t* self);


#include "metac_type_semantic.h"
#include "metac_expr_semantic.h"

#define SemanticError(LOC, MSG, ...) \
    fprintf(stderr, "SemanticError[%s:%u]: "  MSG  "\n", __FILE__, __LINE__, __VA_ARGS__)

void RegisterType(metac_semantic_state_t* state, decl_type_t* type);
const char* TypeToChars(metac_semantic_state_t* self, metac_type_index_t typeIndex);
void MetaCSemantic_Init(metac_semantic_state_t* self,
                        metac_parser_t* parser,
                        metac_type_aggregate_t* compilerStruct);

#define MetaCSemantic_doIndexSemantic(SELF, EXPR) \
    MetaCSemantic_doIndexSemantic_(SELF, EXPR, \
                                   __FILE__, __LINE__)

metac_sema_expr_t* MetaCSemantic_doIndexSemantic_(metac_semantic_state_t* self,
                                                        metac_expr_t* expr,
                                                        const char* callFile,
                                                        uint32_t callLine);
#define MetaCSemantic_doStatementSemantic(SELF, NODE) \
    MetaCSemantic_doStatementSemantic_(SELF, ((metac_stmt_t*)(NODE)), \
                                       __FILE__, __LINE__)

metac_sema_stmt_t* MetaCSemantic_doStatementSemantic_(metac_semantic_state_t* self,
                                                           metac_stmt_t* stmt,
                                                           const char* callFile,
                                                           uint32_t callLine);

#define MetaCSemantic_doDeclSemantic(SELF, NODE) \
    MetaCSemantic_doDeclSemantic_(SELF, ((metac_decl_t*)(NODE)), \
                                  __FILE__, __LINE__)

metac_sema_decl_t* MetaCSemantic_doDeclSemantic_(metac_semantic_state_t* self,
                                                        metac_decl_t* decl,
                                                        const char* callFile,
                                                        uint32_t callLine);

#define MetaCSemantic_doTypeSemantic(SELF, NODE) \
    MetaCSemantic_doTypeSemantic_(SELF, ((decl_type_t*)(NODE)), \
                                  __FILE__, __LINE__)

metac_type_index_t MetaCSemantic_doTypeSemantic_(metac_semantic_state_t* self,
                                                decl_type_t* type,
                                                const char* callFile, uint32_t callLine);

/// Returns _emptyNode to signifiy it could not be found
/// a valid node otherwise
metac_node_header_t* MetaCSemantic_LookupIdentifier(metac_semantic_state_t* self,
                                                    metac_identifier_ptr_t identifierPtr);
#ifndef Expression_IsEqual
#define Expression_IsEqual(A, B) \
    ((A == B) ? true : Expression_IsEqual_( \
    ((const metac_sema_expr_t*)(A)), ((const metac_sema_expr_t*)(B)))
#endif

bool Expression_IsEqual_(const metac_sema_expr_t* a,
                         const metac_sema_expr_t* b);


metac_sema_expr_t* AllocNewSemaExpression(metac_semantic_state_t* self, metac_expr_t* expr);

sema_decl_function_t* AllocNewSemaFunction(metac_semantic_state_t* self,decl_function_t* func);

sema_decl_variable_t* AllocNewSemaVariable(metac_semantic_state_t* self, decl_variable_t *decl, metac_sema_decl_t ** result_ptr);

sema_decl_variable_t* AllocFunctionParameters(metac_semantic_state_t* self, sema_decl_function_t* func,
                                              uint32_t parameterCount);

sema_decl_type_t* AllocNewSemaType(metac_semantic_state_t* self, metac_type_index_t typeIndex);

#define AllocNewAggregate(SELF, KIND) \
    (AllocNewAggregate_(SELF, KIND, __LINE__, __FILE__))
metac_type_aggregate_t* AllocNewAggregate_(metac_semantic_state_t* self, metac_decl_kind_t kind, uint32_t line, const char* file);

metac_type_aggregate_field_t* AllocAggregateFields(metac_semantic_state_t* self,
                                                   metac_type_aggregate_t* aggregate,
                                                   metac_decl_kind_t kind,
                                                   uint32_t fieldCount);
#define AllocNewSemaStatement(SELF, KIND, RESULT_PTR) \
    (sema_ ## KIND ## _t*) AllocNewSemaStatement_(SELF, KIND, sizeof(sema_ ## KIND ##_t), ((void**)(RESULT_PTR)))

metac_sema_stmt_t* AllocNewSemaStatement_(metac_semantic_state_t* self,
                                               metac_stmt_kind_t kind,
                                               size_t nodeSize, void** result_ptr);

sema_stmt_block_t* AllocNewSemaBlockStatement(metac_semantic_state_t* self,
                                              sema_stmt_block_t* Parent, uint32_t statementCount,
                                              void** result_ptr);

sema_stmt_casebody_t* AllocNewSemaCasebodyStatement(metac_semantic_state_t* self,
                                                    uint32_t statementCount,
                                                    void** result_ptr);

metac_scope_t* AllocNewScope(metac_semantic_state_t* self, metac_scope_t* parent, metac_scope_owner_t owner);
metac_type_array_t* AllocNewSemaArrayType(metac_semantic_state_t* self, metac_type_index_t elementTypeIndex, uint32_t dim);

void MetaCSemantic_Handoff(metac_semantic_state_t* self, metac_sema_decl_t** declP,
                           metac_semantic_state_t* newOwner);

metac_type_t NodeFromTypeIndex(metac_semantic_state_t* sema,
                               metac_type_index_t typeIndex);


uint32_t FunctionIndex(metac_semantic_state_t* self, sema_decl_function_t* func);
uint32_t StructIndex(metac_semantic_state_t* self, metac_type_aggregate_t* struct_);
#define StatementIndex(SEMA, STMT) StatementIndex_(SEMA, (metac_sema_stmt_t*)STMT)
uint32_t StatementIndex_(metac_semantic_state_t* self, metac_sema_stmt_t* stmt);
uint32_t BlockStatementIndex(metac_semantic_state_t* self, sema_stmt_block_t* blockstmt);
uint32_t UnionIndex(metac_semantic_state_t* self, metac_type_aggregate_t* union_);
uint32_t TypedefIndex(metac_semantic_state_t* self, metac_type_typedef_t* typedef_);
uint32_t PtrTypeIndex(metac_semantic_state_t* self, metac_type_ptr_t* ptr);
uint32_t ArrayTypeIndex(metac_semantic_state_t* self, metac_type_array_t* array);
uint32_t EnumIndex(metac_semantic_state_t* self, metac_type_enum_t* enum_);
uint32_t FunctiontypeIndex(metac_semantic_state_t* self, metac_type_functiontype_t* functiontype);
uint32_t TupleTypeIndex(metac_semantic_state_t* self, metac_type_tuple_t* typeType);

sema_decl_function_t* FunctionPtr(metac_semantic_state_t* self, uint32_t index);
metac_type_aggregate_t* StructPtr(metac_semantic_state_t* self, uint32_t index);
metac_sema_stmt_t* StatementPtr(metac_semantic_state_t* self, uint32_t index);
sema_stmt_block_t* BlockStatementPtr(metac_semantic_state_t* self, uint32_t index);
metac_type_aggregate_t* UnionPtr(metac_semantic_state_t* self, uint32_t index);
metac_type_typedef_t* TypedefPtr(metac_semantic_state_t* self, uint32_t index);
metac_type_ptr_t* PtrTypePtr(metac_semantic_state_t* self, uint32_t index);
metac_type_array_t* ArrayTypePtr(metac_semantic_state_t* self, uint32_t index);
metac_type_enum_t* EnumTypePtr(metac_semantic_state_t* self, uint32_t index);
metac_type_functiontype_t* FunctiontypePtr(metac_semantic_state_t* self, uint32_t index);
metac_type_tuple_t* TupleTypePtr(metac_semantic_state_t* self, uint32_t index);

metac_scope_t* MetaCScope_PushNewScope(metac_semantic_state_t* sema,
                                       metac_scope_t* parent,
                                       metac_scope_owner_t owner);

metac_scope_t* MetaCSemantic_MountScope(metac_semantic_state_t* self,
                                        metac_scope_t* scope_);
metac_scope_t* MetaCSemantic_UnmountScope(metac_semantic_state_t* self);


scope_insert_error_t MetaCSemantic_RegisterInScope(metac_semantic_state_t* self,
                                                   metac_identifier_ptr_t idPtr,
                                                   metac_node_t node);
#define MetaCSemantic_PushTemporaryScope(SELF, TMPSCOPE) \
    MetaCSemantic_PushTemporaryScope_(SELF, TMPSCOPE, __LINE__, __FILE__)

metac_scope_t* MetaCSemantic_PushTemporaryScope_(metac_semantic_state_t* self,
                                                 metac_scope_t* tmpScope,
                                                 uint32_t line,
                                                 const char* file);

#define MetaCSemantic_PopTemporaryScope(SELF) \
    MetaCSemantic_PopTemporaryScope_(SELF, __LINE__, __FILE__)

void MetaCSemantic_PopTemporaryScope_(metac_semantic_state_t* self,
//                                      metac_scope_t* tmpScope,
                                      uint32_t line,
                                      const char* file);
void MetaCSemantic_ConstantFold(metac_semantic_state_t* self, metac_sema_expr_t* exp);

#endif
