#include "metac_semantic.h"
#include <assert.h>
#include "metac_alloc_node.h"
#include "metac_target_info.h"
#include "metac_default_target_info.h"
#include "bsf.h"
#include <stdlib.h>
#include "crc32c.h"
#define AT(...)

#if defined(SIMD)
#  include "metac_simd.h"
#endif


#ifndef NO_FIBERS
#  include "metac_task.h"
#  include "metac_coro.c"
#endif

#include "metac_type_semantic.c"
#include "metac_expr_semantic.c"

#ifndef NO_FIBERS
const char* MetaCExpressionKind_toChars(metac_expression_kind_t);
extern void* CurrentFiber(void);
extern task_t* CurrentTask(void);
#endif

bool IsExpressionNode(metac_node_kind_t);

bool Expression_IsEqual_(const metac_sema_expression_t* a,
                         const metac_sema_expression_t* b)
{
    bool result = true;
    if (a == b)
        assert(0);
        // pointer equality comparision should have been done
        // before calling this
    if (a->Kind == b->Kind)
    {
        switch(a->Kind)
        {
            case exp_signed_integer:
               result = a->ValueI64 == b->ValueI64;
            break;

            case exp_argument:
            {
                if (a->ArgumentList->ArgumentCount
                    == b->ArgumentList->ArgumentCount)
                {
                    const uint32_t ArgumentCount =
                        a->ArgumentList->ArgumentCount;
                    const metac_sema_expression_t* ExpsA
                            = a->ArgumentList->Arguments;
                    const metac_sema_expression_t* ExpsB
                            = b->ArgumentList->Arguments;

                    for(uint32_t i = 0;
                        i < ArgumentCount;
                        i++)
                    {
                        result &= Expression_IsEqual(ExpsA + i, ExpsB + i);
                    }
                }
                else
                {
                    result = false;
                }
            } break;

            default: assert(0); // Not handled right now
        }
    }
    else
        result = false;
Lret:
    return result;
}


// In case we do a stand alone compile of semantic we need this
#ifndef _METAC_PARSER_C_
    static uint32_t _nodeCounter = 64;
#endif

#ifndef _emptyPointer
#  define _emptyPointer (void*)0x1
#  define emptyNode (metac_node_t) _emptyPointer
#endif

typedef struct handoff_walker_context_t
{
    uint32_t FunctionKey;

    const metac_semantic_state_t* Origin;
    metac_semantic_state_t* NewOwner;
    metac_sema_declaration_t* decl;
    metac_node_t result;
} handoff_walker_context_t;

#define CRC32C_VALUE(HASH, VAL) \
    (crc32c_nozero(HASH, &(VAL), sizeof(VAL)))


#ifndef ATOMIC
#define POST_ADD(v, b) \
    (v += b, v - b)
#else
#define POST_ADD(v, b)
    (__sync_fetch_and_add(&v, b))
#endif

metac_sema_expression_t* AllocNewSemaExpression(metac_semantic_state_t* self, metac_expression_t* expr)
{
    metac_sema_expression_t* result = 0;

    REALLOC_BOILERPLATE(self->Expressions)

    {
        result = self->Expressions + INC(self->Expressions_size);
        (*(metac_expression_header_t*) result) = (*(metac_expression_header_t*) expr);

        result->TypeIndex.v = 0;
        result->Serial = INC(_nodeCounter);
    }

    if (expr->Kind == exp_tuple)
    {
        const uint32_t tupleExpCount = expr->TupleExpressionCount;
        REALLOC_N_BOILERPLATE(self->Expressions, tupleExpCount);

        uint32_t allocPos = POST_ADD(self->Expressions_size, tupleExpCount);
        metac_sema_expression_t* elements =
            self->Expressions + allocPos;
        exp_tuple_t* expList = expr->TupleExpressionList;

        metac_expression_t* elemExpr;
        for(uint32_t i = 0;
            i < tupleExpCount;
            i++)
        {
            elemExpr = expList->Expression;
            metac_sema_expression_t* semaElem = elements + i;
            semaElem->Serial = INC(_nodeCounter);

            (*(metac_expression_header_t*) semaElem) = (*(metac_expression_header_t*) elemExpr);

            memcpy(
                ((char*)semaElem) + sizeof(metac_sema_expression_header_t),
                ((char*)elemExpr) + sizeof(metac_expression_header_t),
                sizeof(metac_expression_t) - sizeof(metac_expression_header_t)
            );

            expList = expList->Next;

        }
        result->TupleExpressions = elements;
    }
    else
    {
        memcpy(
            ((char*)result) + sizeof(metac_sema_expression_header_t),
            ((char*)expr) + sizeof(metac_expression_header_t),
            sizeof(metac_expression_t) - sizeof(metac_expression_header_t)
        );
    }

    return result;
}
// ---------------------------------------------- sema -----------------------------

uint32_t StructIndex(metac_semantic_state_t* self, metac_type_aggregate_t* struct_)
{
    uint32_t result = (struct_ - self->StructTypeTable.Slots);
    return result;
}

uint32_t UnionIndex(metac_semantic_state_t* self, metac_type_aggregate_t* union_)
{
    uint32_t result = (union_ - self->UnionTypeTable.Slots);
    return result;
}


uint32_t FunctionIndex(metac_semantic_state_t* self, sema_decl_function_t* func)
{
    uint32_t result = (func - self->Functions);
    return result;
}

uint32_t StatementIndex_(metac_semantic_state_t* self, metac_sema_statement_t* stmt)
{
    uint32_t result = (stmt - self->Statements);
    return result;
}

uint32_t TypedefIndex(metac_semantic_state_t* self, metac_type_typedef_t* typedef_)
{
    uint32_t result = (typedef_ - self->TypedefTypeTable.Slots);
    return result;
}

uint32_t ArrayTypeIndex(metac_semantic_state_t* self, metac_type_array_t* array)
{
    uint32_t result = (array - self->ArrayTypeTable.Slots);
    return result;
}

uint32_t PtrTypeIndex(metac_semantic_state_t* self, metac_type_ptr_t* ptr)
{
    uint32_t result = (ptr - self->PtrTypeTable.Slots);
    return result;
}

uint32_t FunctiontypeIndex(metac_semantic_state_t* self, metac_type_functiontype_t* functiontype)
{
    uint32_t result = (functiontype - self->FunctionTypeTable.Slots);
    return result;
}

uint32_t EnumIndex(metac_semantic_state_t* self, metac_type_enum_t* enumtype)
{
    uint32_t result = (enumtype - self->EnumTypeTable.Slots);
    return result;
}


uint32_t TupleTypeIndex(metac_semantic_state_t* self, metac_type_tuple_t* tupletype)
{
    uint32_t result = (tupletype - self->TupleTypeTable.Slots);
    return result;
}

metac_type_aggregate_t* StructPtr(metac_semantic_state_t* self, uint32_t index)
{
    metac_type_aggregate_t* result = (self->StructTypeTable.Slots + index);
    return result;
}

metac_type_aggregate_t* UnionPtr(metac_semantic_state_t* self, uint32_t index)
{
    metac_type_aggregate_t* result = (self->UnionTypeTable.Slots + index);
    return result;
}

sema_decl_function_t* FunctionPtr(metac_semantic_state_t* self, uint32_t index)
{
    sema_decl_function_t* result = (self->Functions + index);
    return result;
}

metac_sema_statement_t* StatementPtr(metac_semantic_state_t* self, uint32_t index)
{
    metac_sema_statement_t* result = (self->Statements + index);
    return result;
}

metac_type_typedef_t* TypedefPtr(metac_semantic_state_t* self, uint32_t index)
{
    metac_type_typedef_t* result = (self->TypedefTypeTable.Slots + index);
    return result;
}

metac_type_ptr_t* PtrTypePtr(metac_semantic_state_t* self, uint32_t index)
{
    metac_type_ptr_t* result = (self->PtrTypeTable.Slots + index);
    return result;
}

metac_type_array_t* ArrayTypePtr(metac_semantic_state_t* self, uint32_t index)
{
    metac_type_array_t* result = (self->ArrayTypeTable.Slots + index);
    return result;
}

metac_type_functiontype_t* FunctiontypePtr(metac_semantic_state_t* self, uint32_t index)
{
    metac_type_functiontype_t* result = (self->FunctionTypeTable.Slots + index);
    return result;
}

metac_type_enum_t* EnumTypePtr(metac_semantic_state_t* self, uint32_t index)
{
    metac_type_enum_t* result = (self->EnumTypeTable.Slots + index);
    return result;
}


metac_type_tuple_t* TupleTypePtr(metac_semantic_state_t* self, uint32_t index)
{
    metac_type_tuple_t* result = (self->TupleTypeTable.Slots + index);
    return result;
}

metac_scope_t* MetaCScope_PushNewScope(metac_semantic_state_t* sema,
                                       metac_scope_t *parent,
                                       metac_scope_parent_t scopeOwner)
{
    metac_scope_t* result = AllocNewScope(sema, parent, scopeOwner);

    MetaCScopeTable_Init(&result->ScopeTable);

    return result;
}


metac_scope_t* AllocNewScope(metac_semantic_state_t* self,
                             metac_scope_t* parent, metac_scope_parent_t owner)
{
    metac_scope_t* result;

    REALLOC_BOILERPLATE(self->Scopes)

    {
        result = self->Scopes + INC(self->Scopes_size);

        result->Serial = INC(_nodeCounter);
        result->Owner = owner;
        result->Parent = parent;
    }

    return result;
}


sema_decl_function_t* AllocNewSemaFunction(metac_semantic_state_t* self,
                                           decl_function_t* func)
{
    sema_decl_function_t* result = 0;

    REALLOC_BOILERPLATE(self->Functions)

    {
        result = self->Functions + INC(self->Functions_size);
        (*(metac_node_header_t*) result) = (*(metac_node_header_t*) func);

        result->Serial = INC(_nodeCounter);
        result->TypeIndex.v = 0;
    }

    return result;
}

sema_decl_variable_t* AllocNewSemaVariable(metac_semantic_state_t* self, decl_variable_t* decl, metac_sema_declaration_t** result_ptr)
{
    sema_decl_variable_t* result = 0;
    REALLOC_BOILERPLATE(self->Variables)

    result = self->Variables + INC(self->Variables_size);
    (*result_ptr) = (metac_sema_declaration_t*)result;

    result->DeclKind = decl_variable;
    result->Serial = INC(_nodeCounter);
    decl->LocationIdx = result->LocationIdx;


    return result;
}

sema_decl_variable_t* AllocFunctionParameters(metac_semantic_state_t* self,
                                              sema_decl_function_t* func,
                                              uint32_t parameterCount)
{
    sema_decl_variable_t* result = 0;

    REALLOC_N_BOILERPLATE(self->Variables, parameterCount)

    {
        result = self->Variables + POST_ADD(self->Variables_size, parameterCount);
        for(uint32_t i = 0;
            i < parameterCount;
            i++)
        {
            (result + i)->DeclKind = decl_parameter;
            (result + i)->Serial = INC(_nodeCounter);
        }

    }

    return result;
}
#if 0
metac_type_aggregate_field_t* AllocAggregateFields(metac_semantic_state_t* self,
                                                   metac_type_aggregate_t* aggregate,
                                                   metac_declaration_kind_t kind,
                                                   uint32_t fieldCount)
{
    uint32_t aggregateIndex = 0;
    metac_type_aggregate_field_t* result = 0;
    switch(kind)
    {
        case decl_type_struct:
        {
            REALLOC_BOILERPLATE(_newSemaStructFields)
            result = _newSemaStructFields_mem +
                POST_ADD(_newSemaStructFields_size, fieldCount);
            aggregateIndex = aggregate - _newSemaStructs_mem;
        } break;
        case decl_type_union:
        {
            REALLOC_BOILERPLATE(_newSemaUnionFields)
            result = _newSemaUnionFields_mem +
                POST_ADD(_newSemaUnionFields_size, fieldCount);
            aggregateIndex = aggregate - _newSemaUnions_mem;
        } break;

        case type_class:
        {
            assert(0);
        } break;

    }

    {
        for(uint32_t i = 0;
            i < fieldCount;
            i++)
        {
            (result + i)->Header.Serial = INC(_nodeCounter);
            (result + i)->AggregateIndex = aggregateIndex;
        }

    }

    aggregate->Fields = result;

    return result;
}
#endif

metac_sema_statement_t* AllocNewSemaStatement_(metac_semantic_state_t* self,
                                               metac_statement_kind_t kind,
                                               size_t nodeSize, void** result_ptr)
{
    metac_sema_statement_t* result = 0;

    REALLOC_BOILERPLATE(self->Statements)

    {
        result = self->Statements + INC(self->Statements_size);
        // result->Parent = 0;

        result->Serial = INC(_nodeCounter);
        // result->TypeIndex.v = 0;
    }

    *result_ptr = result;

    return result;
}

sema_stmt_block_t* AllocNewSemaBlockStatement(metac_semantic_state_t* self,
                                              sema_stmt_block_t* Parent, uint32_t statementCount,
                                              void** result_ptr)
{
    sema_stmt_block_t* result = 0;

    REALLOC_BOILERPLATE(self->BlockStatements)

    {
        uint32_t pointersSize = statementCount * sizeof(sema_stmt_block_t*);
        uint32_t sizeInBlockStatements =
            (pointersSize + sizeof(*self->BlockStatements)) /
            sizeof(*self->BlockStatements);

        result = self->BlockStatements + POST_ADD(self->BlockStatements_size,
                                                        sizeInBlockStatements);
        result->Serial = INC(_nodeCounter);
        result->Body = (metac_sema_statement_t*)(result + 1);
    }
    (*result_ptr) = result;

    return result;
}

uint32_t BlockStatementIndex(metac_semantic_state_t* self,
                             sema_stmt_block_t* blockstmt)
{
    return blockstmt - self->BlockStatements;
}



static inline void HandoffIdentifier(metac_identifier_table_t* dstTable,
                                     const metac_identifier_table_t* srcTable,
                                     metac_identifier_ptr_t* idPtrP)
{
    assert(idPtrP->v);
    const char* idChars =
        IdentifierPtrToCharPtr((metac_identifier_table_t*)srcTable, *idPtrP);
    const uint32_t idLen = strlen(idChars);
    const uint32_t idHash = crc32c_nozero(~0, idChars, idLen);
    const uint32_t idKey = IDENTIFIER_KEY(idHash, idLen);

    metac_identifier_ptr_t newPtr =
        GetOrAddIdentifier(dstTable, idKey, idChars);

    idPtrP->v = newPtr.v;
}

static void HandoffField(metac_semantic_state_t* dstState,
                          const metac_semantic_state_t* srcState,
                          metac_type_aggregate_field_t* field)
{

}
static inline void HandoffType(metac_semantic_state_t* dstState,
                               const metac_semantic_state_t* srcState,
                               metac_type_index_t* typeIndexP)
{
    assert(typeIndexP->v);
    metac_type_index_t result = {0};

    metac_type_index_t idx = *typeIndexP;
    switch(TYPE_INDEX_KIND(idx))
    {

        case type_index_basic:
            result = *typeIndexP;
        break;
        case type_index_unknown: assert(0);
        case type_index_functiontype:
        {
            metac_type_functiontype_t* oldSlot
                = srcState->FunctionTypeTable.Slots + TYPE_INDEX_INDEX(idx);

            metac_type_functiontype_t* newSlot;
            metac_type_functiontype_t tmpSlot = *oldSlot;
            const uint32_t paramTypeCount = oldSlot->ParameterTypeCount;
            metac_type_index_t* newParams = cast(metac_type_index_t*)
                calloc(sizeof(metac_type_index_t), oldSlot->ParameterTypeCount);
            memcpy(newParams, oldSlot->ParameterTypes,
                sizeof(metac_type_index_t) * paramTypeCount);
            for(uint32_t i = 0;
                i < paramTypeCount;
                i++)
            {
                HandoffType(dstState, srcState, newParams + i);
            }
            if (tmpSlot.ParameterTypeCount != 0)
                tmpSlot.ParameterTypes = newParams;
            else
                tmpSlot.ParameterTypes = (metac_type_index_t*)emptyNode;

            result =
                MetaCTypeTable_GetOrEmptyFunctionType(&dstState->FunctionTypeTable,
                                                      &tmpSlot);
            if (result.v == 0)
            {
                result =
                    MetaCTypeTable_AddFunctionType(&dstState->FunctionTypeTable, &tmpSlot);
            }
            assert(result.v != -1 && result.v != 0);
        } break;

        case type_index_struct:
        {
            metac_type_aggregate_t* oldSlot =
                srcState->StructTypeTable.Slots + TYPE_INDEX_INDEX(idx);

            metac_type_aggregate_t tmpSlot = *oldSlot;

            const uint32_t fieldCount = oldSlot->FieldCount;

            metac_type_aggregate_field_t* newFields =
                (metac_type_aggregate_field_t*)
                    malloc(sizeof(metac_type_aggregate_field_t) * fieldCount);
            for(uint32_t i = 0; i < oldSlot->FieldCount; i++)
            {
                metac_type_aggregate_field_t field = oldSlot->Fields[i];
                HandoffIdentifier(dstState->ParserIdentifierTable,
                                  srcState->ParserIdentifierTable,
                                  &field.Identifier);
                HandoffType(dstState, srcState, &field.Type);
                newFields[i] = field;
            }
            tmpSlot.Fields = newFields;
            tmpSlot.Header.Hash = 0;

            uint32_t newHash = AggregateHash(&tmpSlot);
            tmpSlot.Header.Hash = newHash;

            result =
                MetaCTypeTable_GetOrEmptyStructType(&dstState->StructTypeTable,
                                                    &tmpSlot);
            assert(result.v == 0);
            result =
                MetaCTypeTable_AddStructType(&dstState->StructTypeTable, &tmpSlot);
            assert(result.v != -1 && result.v != 0);
        } break;
        case type_index_union:
        {
            assert(0);
#if 0
            metac_type_aggregate_t* oldSlot;
            metac_type_aggregate_t* newSlot;

            metac_type_aggregate_t tmpSlot = *oldSlot;

            oldSlot = srcState->UnionTypeTable.Slots + TYPE_INDEX_INDEX(idx);
#endif
        } break;
        LhandoffAggregate:
        {

        } break;
    }

    assert(result.v);
    (*typeIndexP) = result;
}

#include <string.h>
static inline int HandoffWalker(metac_node_t node, void* ctx)
{
    handoff_walker_context_t* context =
        (handoff_walker_context_t*) ctx;
    assert(crc32c_nozero(~0, __FUNCTION__, strlen(__FUNCTION__))
        == context->FunctionKey);

    const metac_semantic_state_t* srcState = context->Origin;
    metac_semantic_state_t* dstState = context->NewOwner;

    switch(node->Kind)
    {
        case node_decl_type_union:
        case node_decl_type_struct:
        {
            metac_type_aggregate_t* struct_ =
                cast(metac_type_aggregate_t*) node;
            uint32_t hash = struct_->Header.Hash;
            assert(hash);

            metac_type_index_t typeIndex =
                MetaCTypeTable_GetOrEmptyStructType(&srcState->StructTypeTable,
                                                    struct_);
            assert(typeIndex.v != 0 && typeIndex.v != -1);
            HandoffType(dstState, srcState, &typeIndex);

            context->result =
                (metac_node_t)StructPtr(dstState, TYPE_INDEX_INDEX(typeIndex));
            return 1;
        }
        case node_decl_type_functiontype:
        {
            metac_type_functiontype_t* functiontype =
                cast(metac_type_functiontype_t*) node;
            uint32_t hash = functiontype->Header.Hash;
            assert(hash);


            metac_type_index_t typeIndex =
                MetaCTypeTable_GetOrEmptyFunctionType(&srcState->FunctionTypeTable,
                                                      functiontype);
            assert(typeIndex.v != 0 && typeIndex.v != -1);
            HandoffType(dstState, srcState, &typeIndex);

            context->result =
                (metac_node_t)FunctiontypePtr(dstState, TYPE_INDEX_INDEX(typeIndex));
        } break;
        case node_decl_field:
        {

        } break;
    }

    return 0;
}

/// transfers ownership of decl and all it's dependents from self to newOwner
void MetaCSemantic_Handoff(metac_semantic_state_t* self, metac_sema_declaration_t** declP,
                           metac_semantic_state_t* newOwner)
{
    printf("Handoff\n");
    metac_sema_declaration_t* decl = *declP;

    handoff_walker_context_t handoff_context = {
        crc32c_nozero(~0, "HandoffWalker", sizeof("HandoffWalker") -1),
        self, newOwner, decl, 0
    };

    //TODO
    // MetaCSemaDeclaration_Walk(decl, HandoffWalker, &handoff_context);

    *declP = (metac_sema_declaration_t*)handoff_context.result;
}

void MetaCSemantic_Init(metac_semantic_state_t* self, metac_parser_t* parser,
                        metac_type_aggregate_t* compilerStruct)
{
#define INIT_TYPE_TABLE(TYPE_NAME, MEMBER_NAME, INDEX_KIND) \
    TypeTableInitImpl((metac_type_table_t*)&self->MEMBER_NAME, \
                      sizeof(metac_type_ ## TYPE_NAME ## _t), \
                      type_index_## INDEX_KIND);

    FOREACH_TYPE_TABLE(INIT_TYPE_TABLE)

    FOREACH_SEMA_STATE_ARRAY(self, INIT_ARRAY)

    self->TemporaryScopeDepth = 0;

    self->ExpressionStackCapacity = 64;
    self->ExpressionStackSize = 0;
    self->ExpressionStack = (metac_sema_expression_t*)
        calloc(sizeof(metac_sema_expression_t), self->ExpressionStackCapacity);

    self->Waiters.WaiterLock._rwctr = 0;
    self->Waiters.WaiterCapacity = 64;
    self->Waiters.WaiterCount = 0;
    self->Waiters.Waiters = cast(metac_semantic_waiter_t*)
                calloc(sizeof(*self->Waiters.Waiters), self->Waiters.WaiterCapacity);

    IdentifierTable_Init(&self->SemanticIdentifierTable, IDENTIFIER_LENGTH_SHIFT, 13);
    self->ParserIdentifierTable = &parser->IdentifierTable;
    self->ParserStringTable = &parser->StringTable;

    self->CurrentScope = 0;
    self->CurrentDeclarationState = 0;

    memset(&self->LRU, 0, sizeof(self->LRU));

    if (compilerStruct && ((metac_node_t)compilerStruct) != emptyNode)
    {
        self->CompilerInterface = compilerStruct;
    }
    else
    {
        self->CompilerInterface = 0;
    }

    self->initialized = true;
}

void MetaCSemantic_PopScope(metac_semantic_state_t* self)
{
    assert(self->CurrentScope);
    self->CurrentScope = self->CurrentScope->Parent;
}

metac_scope_parent_t ScopeParent(metac_semantic_state_t* sema,
                                 metac_scope_parent_kind_t parentKind,
                                 metac_node_t parentNode)
{
    metac_scope_parent_t scopeParent;

    uint32_t scopeParentIndex = 0;
    switch(parentKind)
    {
    case scope_parent_invalid :
    case scope_parent_extended :
    case scope_parent_unknown :
        assert(0);

    case scope_parent_module :
    //FIXME TODO this is not how it should be;
    // we should allocate module structures and so on
        scopeParentIndex = (uint32_t) parentNode;
    break;
    case scope_parent_function :
        scopeParentIndex = FunctionIndex(sema, (sema_decl_function_t*)parentNode);
    break;
    case scope_parent_struct :
        scopeParentIndex = StructIndex(sema, (metac_type_aggregate_t*)parentNode);
    break;
    case scope_parent_statement :
        scopeParentIndex = StatementIndex(sema, (metac_sema_statement_t*)parentNode);
    break;
    case scope_parent_block :
        scopeParentIndex = BlockStatementIndex(sema, (sema_stmt_block_t*)parentNode);
    break;
    case scope_parent_union :
        scopeParentIndex = UnionIndex(sema, (metac_type_aggregate_t*)parentNode);
    break;
    }
    scopeParent.v = SCOPE_PARENT_V(parentKind, scopeParentIndex);

    return scopeParent;
}

bool IsTemporaryScope(metac_scope_t* scope_)
{
    return (scope_->scopeFlags & scope_flag_temporary) != 0;
}
#define MetaCSemantic_PushTemporaryScope(SELF, TMPSCOPE) \
    MetaCSemantic_PushTemporaryScope_(SELF, TMPSCOPE, __LINE__, __FILE__)

metac_scope_t* MetaCSemantic_PushTemporaryScope_(metac_semantic_state_t* self,
                                                 metac_scope_t* tmpScope,
                                                 uint32_t line,
                                                 const char* file)
{
    printf("[%u]Pushing tmpScope {%s:%u}\n", self->TemporaryScopeDepth, file, line);
    assert((tmpScope->scopeFlags & scope_flag_temporary) != 0);

    assert ((!self->TemporaryScopeDepth) || IsTemporaryScope(self->CurrentScope));
    self->TemporaryScopeDepth++;

    tmpScope->Parent = self->CurrentScope;
    self->CurrentScope = tmpScope;

    return tmpScope;
}

void MetaCSemantic_PopTemporaryScope_(metac_semantic_state_t* self,
//                                      metac_scope_t* tmpScope,
                                      uint32_t line,
                                      const char* file)
{
    assert(self->TemporaryScopeDepth != 0 && IsTemporaryScope(self->CurrentScope));
    --self->TemporaryScopeDepth;
    printf("[%u] Popping tmpScope {%s:%u}\n", self->TemporaryScopeDepth, file, line);
    self->CurrentScope = self->CurrentScope->Parent;
}
/*
#define MetaCSemantic_PushMountedScope(SELF, MNTSCOPE) \
    MetaCSemantic_PushMountedScope_(SELF, MNTSCOPE, __LINE__, __FILE__)

metac_scope_t* MetaCSemantic_PushMountedScope_(metac_semantic_state_t* self,
                                               metac_scope_t* mntScope,
                                               uint32_t line,
                                               const char* file)
{
    printf("[%u]Pushing mntScope {%s:%u}\n", self->TemporaryScopeDepth, file, line);
    assert((tmpScope->scopeFlags & scope_flag_mounted) == 0);

    assert ((!self->TemporaryScopeDepth) || IsTemporaryScope(self->CurrentScope));
    self->MountedScopeDepth++;

    tmpScope->Parent = self->CurrentScope;
    self->CurrentScope = tmpScope;

    return tmpScope;
}
 *
#define MetaCSemantic_PopMountedScope(SELF) \
    MetaCSemantic_PopMountedScope_(SELF, __LINE__, __FILE__)
void MetaCSemantic_PopMountedScope_(metac_semantic_state_t* self,
//                                      metac_scope_t* tmpScope,
                                      uint32_t line,
                                      const char* file)
{
    assert(self->TemporaryScopeDepth != 0 && IsTemporaryScope(self->CurrentScope));
    --self->MountedScopeDepth;
    printf("[%u] Popping mntScope {%s:%u}\n", self->TemporaryScopeDepth, file, line);
    self->CurrentScope = self->CurrentScope->Parent;
}
*/
metac_scope_t* MetaCSemantic_PushNewScope(metac_semantic_state_t* self,
                                          metac_scope_parent_kind_t parentKind,
                                          metac_node_t parentNode)
{
    metac_scope_parent_t scopeParent = ScopeParent(self, parentKind, parentNode);
    self->CurrentScope = MetaCScope_PushNewScope(self,
                                                 self->CurrentScope,
                                                 scopeParent);
    return self->CurrentScope;
}

metac_sema_statement_t* MetaCSemantic_doStatementSemantic_(metac_semantic_state_t* self,
                                                           metac_statement_t* stmt,
                                                           const char* callFile,
                                                           uint32_t callLine)
{
    metac_sema_statement_t* result;
    switch (stmt->StmtKind)
    {
        case stmt_exp:
        {
            sema_stmt_exp_t* sse = AllocNewSemaStatement(self, stmt_exp, &result);
        } break;

        default: assert(0);

        case stmt_block:
        {
            stmt_block_t* blockStatement = (stmt_block_t*) stmt;
            uint32_t statementCount = blockStatement->StatementCount;
            sema_stmt_block_t* semaBlockStatement =
                AllocNewSemaStatement(self, stmt_block, &result);

            metac_scope_parent_t parent = {SCOPE_PARENT_V(scope_parent_statement,
                                           BlockStatementIndex(self, semaBlockStatement))};

            MetaCSemantic_PushNewScope(self,
                                       scope_parent_block,
                                       (metac_node_t)semaBlockStatement);

            metac_statement_t* currentStatement = blockStatement->Body;
            for(uint32_t i = 0;
                i < statementCount;
                i++)
            {
                ((&semaBlockStatement->Body)[i]) =
                    MetaCSemantic_doStatementSemantic(self, currentStatement);
                currentStatement = currentStatement->Next;
            }

            MetaCSemantic_PopScope(self);
        } break;

        case stmt_for:
        {
            stmt_for_t* for_ = (stmt_for_t*) stmt;
            sema_stmt_for_t* semaFor =
                AllocNewSemaStatement(self, stmt_for, for_);

            metac_scope_t* forScope = semaFor->Scope =
                MetaCSemantic_PushNewScope(self,
                                           scope_parent_statement,
                                           METAC_NODE(semaFor));

            metac_sema_declaration_t* ForInit =
                MetaCSemantic_doDeclSemantic(self, for_->ForInit);
            metac_sema_expression_t* ForCond =
                MetaCSemantic_doExprSemantic(self, for_->ForCond, 0);
            metac_sema_expression_t* ForPostLoop =
                MetaCSemantic_doExprSemantic(self, for_->ForPostLoop, 0);
        } break;

        //TODO for now stmt_yield and stmt_return
        // can share the same code as the layout is the same
        case stmt_yield:
        {
            stmt_yield_t* yieldStatement = (stmt_yield_t*) stmt;
            sema_stmt_yield_t* semaYieldStatement =
                AllocNewSemaStatement(self, stmt_yield, &result);

            metac_sema_expression_t* yieldValue =
                MetaCSemantic_doExprSemantic(self, yieldStatement->YieldExp, 0);
            semaYieldStatement->YieldExp = yieldValue;
        } break;

        case stmt_return:
        {
            stmt_return_t* returnStatement = (stmt_return_t*) stmt;
            sema_stmt_return_t* semaReturnStatement =
                AllocNewSemaStatement(self, stmt_return, &result);

            metac_sema_expression_t* returnValue =
                MetaCSemantic_doExprSemantic(self, returnStatement->ReturnExp, 0);
            semaReturnStatement->ReturnExp = returnValue;
        } break;
    }

    return result;
}

#ifndef INVALID_SIZE
#  define INVALID_SIZE \
      ((uint32_t)-1)
#endif

#ifndef U32
#  define U32(VAR) \
      (*(uint32_t*)(&VAR))
#endif

#ifdef SSE2
/// taken from https://github.com/AuburnSounds/intel-intrinsics/blob/master/source/inteli/emmintrin.d
/// Thanks Guillaume!
static inline uint32_t _mm_movemask_epi16( __m128i a )
{
    return _mm_movemask_epi8(_mm_packs_epi16(a, _mm_setzero_si128()));
}
#endif

#include "crc32c.h"
scope_insert_error_t MetaCSemantic_RegisterInScope(metac_semantic_state_t* self,
                                                   metac_identifier_ptr_t idPtr,
                                                   metac_node_t node)
{
    scope_insert_error_t result = no_scope;
    uint32_t idPtrHash = crc32c_nozero(~0, &idPtr.v, sizeof(idPtr.v));
    // first search for keys that might clash
    // and remove them from the LRU hashes
    uint16_t hash12 = idPtrHash & 0xFFF0;

    int16x8_t hashes;
    hashes = Load16(&self->LRU.LRUContentHashes);
    const int16x8_t hash12_8 = Set1_16(hash12);
    const int16x8_t hashMask = Set1_16((uint16_t)0xFFF0);
    const int16x8_t maskedHashes = And16(hashes, hashMask);
    const int16x8_t clearMask = Eq16(maskedHashes, hash12_8);
    const int16x8_t cleared = Andnot16(clearMask, hashes);
    Store16(&self->LRU.LRUContentHashes, cleared);

    if (self->CurrentScope != 0)
        result = MetaCScope_RegisterIdentifier(self->CurrentScope, idPtr, node);
#ifndef NO_FIBERS
#if 0
    EmitSignal(MetaCScope_RegisterIdentifier, idPtr);
#endif
    //RLOCK(&self->Waiters.WaiterLock);
    for(uint32_t i = 0; i < self->Waiters.WaiterCount; i++)
    {
        metac_semantic_waiter_t *waiter = &self->Waiters.Waiters[i];
        if (waiter->FuncHash == CRC32C_S("MetaCSemantic_LookupIdentifier")
         && waiter->NodeHash == CRC32C_VALUE(~0, idPtr))
        {
            task_t* waitingTask = ((task_t*)waiter->Continuation->arg);
            assert(waitingTask->TaskFlags == Task_Waiting);
            printf("Found matching waiter\n");
            waitingTask->TaskFlags &= (~Task_Waiting);
            waitingTask->TaskFlags |= Task_Resumable;
            //RWLOCK(&self->Waiters.WaiterLock);
            {
                *waiter = self->Waiters.Waiters[--self->Waiters.WaiterCount];
            }
            //RWUNLOCK(&self->Waiters.WaiterLock);
        }
    }
    //RUNLOCK(&self->Waiters.WaiterLock);
#endif
    return result;
}


sema_decl_function_t* MetaCSemantic_doFunctionSemantic(metac_semantic_state_t* self,
                                                       decl_function_t* func)
{
    // one cannot do nested function semantic at this point
    // assert(self->CurrentDeclarationState == 0);
    metac_sema_decl_state_t declState = {0};
    self->CurrentDeclarationState = &declState;

    sema_decl_function_t* f = AllocNewSemaFunction(self, func);
    // let's first do the parameters
    sema_decl_variable_t* params =
        f->Parameters =
            AllocFunctionParameters(self, f, func->ParameterCount);

    decl_parameter_t* currentParam = func->Parameters;
    for(uint32_t i = 0;
        i < func->ParameterCount;
        i++)
    {
        // let's do the parameter semantic inline
        // as we have an easier time if we know at which
        // param we are and how many follow
        f->Parameters[i].VarFlags |= variable_is_parameter;
        f->Parameters[i].TypeIndex =
            MetaCSemantic_doTypeSemantic(self,
                                         currentParam->Parameter->VarType);
        currentParam = currentParam->Next;
    }
    // now we should know the sizes
    assert(currentParam == emptyPointer);

    if (func->FunctionBody == emptyPointer)
    {
        return f;
    }

    metac_scope_parent_t Parent = {SCOPE_PARENT_V(scope_parent_function, FunctionIndex(self, f))};

    f->Scope = MetaCSemantic_PushNewScope(self, scope_parent_function, (metac_node_t)f);
    // now we compute the position on the stack and Register them in the scope.

	uint32_t frameOffset = ((f->ParentFunc != (sema_decl_function_t*)emptyNode)
                           ? f->ParentFunc->FrameOffset : 0);

    for(uint32_t i = 0;
        i < func->ParameterCount;
        i++)
    {
        metac_node_t ptr = (metac_node_t)(&f->Parameters[i]);
        params[i].Storage.v = STORAGE_V(storage_stack, frameOffset);
        frameOffset += Align(MetaCSemantic_GetTypeSize(self, params[i].TypeIndex), 4);

        scope_insert_error_t result =
            MetaCScope_RegisterIdentifier(f->Scope, params[i].VarIdentifier,
                                          ptr);
    }
    f->FrameOffset = frameOffset;
    f->FunctionBody = (sema_stmt_block_t*)
        MetaCSemantic_doStatementSemantic(self, func->FunctionBody);

    MetaCSemantic_PopScope(self);

    return f;
}

metac_node_t NodeFromTypeIndex(metac_semantic_state_t* sema,
                               metac_type_index_t typeIndex)
{
    const uint32_t index = TYPE_INDEX_INDEX(typeIndex);
    switch(TYPE_INDEX_KIND(typeIndex))
    {
        case type_index_struct:
            return (metac_node_t)StructPtr(sema, index);
        case type_index_union:
            return (metac_node_t)UnionPtr(sema, index);
        case type_index_typedef:
            return (metac_node_t)(TypedefPtr(sema, index));
    }

    return 0;
}
#ifndef NO_FIBERS
typedef struct MetaCSemantic_doDeclSemantic_task_context_t
{
    metac_semantic_state_t* Sema;
    metac_declaration_t* Decl;
    metac_sema_declaration_t* Result;
} MetaCSemantic_doDeclSemantic_task_context_t;

const char* doDeclSemantic_PrintFunction(task_t* task)
{
    MetaCSemantic_doDeclSemantic_task_context_t* ctx =
         (MetaCSemantic_doDeclSemantic_task_context_t*)
            task->Context;
    char* buffer = cast(char*)malloc(256);
    metac_printer_t printer;
    //MetaCPrinter_Init(&printer, ctx->)
    const char* declPrint = 0;
//        MetaCPrinter_PrintDeclaration(&printer, ctx->Decl);

    sprintf(buffer, "doDeclSemantic {Sema: %p, Decl: %s}\n",
                    ctx->Sema, declPrint);

    return buffer;
}
#endif

metac_sema_declaration_t* MetaCSemantic_declSemantic(metac_semantic_state_t* self,
                                                     metac_declaration_t* decl)
{
    metac_sema_declaration_t* result = cast(metac_sema_declaration_t*)0xFEFEFEFE;
    metac_identifier_ptr_t declId = {0};

    switch(decl->DeclKind)
    {
        case decl_function:
        {
            decl_function_t* f = cast(decl_function_t*) decl;
            result = (metac_sema_declaration_t*)
                MetaCSemantic_doFunctionSemantic(self, f);

        } break;
        case decl_parameter:
            assert(0);
        case decl_variable:
        {
            decl_variable_t* v = cast(decl_variable_t*) decl;
            sema_decl_variable_t* var = AllocNewSemaVariable(self, v, &result);
            var->TypeIndex = MetaCSemantic_doTypeSemantic(self, v->VarType);
            if (METAC_NODE(v->VarInitExpression) != emptyNode)
            {
                var->VarInitExpression = MetaCSemantic_doExprSemantic(self, v->VarInitExpression, 0);
            }
            else
            {
                METAC_NODE(var->VarInitExpression) = emptyNode;
            }
            //TODO RegisterIdentifier
            var->VarIdentifier = v->VarIdentifier;
            MetaCSemantic_RegisterInScope(self, var->VarIdentifier, METAC_NODE(var));
        } break;
        case decl_type_struct:
            ((decl_type_t*)decl)->TypeKind = type_struct;
            declId = ((decl_type_struct_t*) decl)->Identifier;
            goto LdoTypeSemantic;
        case decl_type_union:
            ((decl_type_t*)decl)->TypeKind = type_union;
            declId = ((decl_type_union_t*) decl)->Identifier;
            goto LdoTypeSemantic;
        case decl_type_array:
            ((decl_type_t*)decl)->TypeKind = type_array;
            goto LdoTypeSemantic;
        case decl_type_typedef:
            ((decl_type_t*)decl)->TypeKind = type_typedef;
            declId = ((decl_type_typedef_t*) decl)->Identifier;
        goto LdoTypeSemantic;
        case decl_type_ptr:
            ((decl_type_t*)decl)->TypeKind = type_ptr;
        goto LdoTypeSemantic;
    LdoTypeSemantic:
        {
            metac_type_index_t type_index =
                MetaCSemantic_doTypeSemantic(self, (decl_type_t*)decl);
            if (declId.v != 0 && declId.v != -1)
            {
                metac_node_t node =
                    NodeFromTypeIndex(self, type_index);
                MetaCSemantic_RegisterInScope(self, declId, node);
                                result = cast(metac_sema_declaration_t*)node;
            }
        } break;
    }
    assert(result != cast(metac_sema_declaration_t*)0xFEFEFEFE);
    return result;
}
#ifndef NO_FIBERS
void MetaCSemantic_doDeclSemantic_Task(task_t* task)
{
    const char* taskPrint = doDeclSemantic_PrintFunction(task);
    printf("Task: %s\n", taskPrint);
    free(taskPrint);

    MetaCSemantic_doDeclSemantic_task_context_t* ctx =
        (MetaCSemantic_doDeclSemantic_task_context_t*)
            task->Context;
    task_origin_t Origin = task->Origin;
    ctx->Result =
        MetaCSemantic_doDeclSemantic_(ctx->Sema, ctx->Decl, Origin.File, Origin.Line);
}
#endif
#define TracyMessage(MSG) \
    TracyCMessage(MSG, sizeof(MSG) - 1)
metac_sema_declaration_t* MetaCSemantic_doDeclSemantic_(metac_semantic_state_t* self,
                                                        metac_declaration_t* decl,
                                                        const char* callFile,
                                                        uint32_t callLine)
{
    metac_sema_declaration_t* result = 0;
    result = MetaCSemantic_declSemantic(self, decl);
    if (!result)
    {
#if !defined(NO_FIBERS)
        taskqueue_t* q = &CurrentWorker()->Queue;
        printf("Couldn't do the decl Semantic, yielding to try again\n");
        MetaCSemantic_doDeclSemantic_task_context_t CtxValue = {
            self, decl
        };

        task_t declTask;
        task_t* currentTask = CurrentTask();
        if (currentTask->TaskFunction == MetaCSemantic_doDeclSemantic_Task)
        {
            TracyMessage("pay attenetion now");
        }
        declTask.TaskFunction = MetaCSemantic_doDeclSemantic_Task;
                declTask.Origin.File = callFile;
        declTask.Origin.Line = callFile;
        declTask.Context = declTask._inlineContext;
        declTask.ContextSize = sizeof(CtxValue);
        assert(sizeof(CtxValue) < sizeof(declTask._inlineContext));
        (*(MetaCSemantic_doDeclSemantic_task_context_t*)declTask.Context) =
            CtxValue;
        MetaCSemantic_doDeclSemantic_task_context_t* CtxValuePtr = &CtxValue;
        printf("We should yield now\n");
        declTask.Continuation = currentTask;
        TaskQueue_Push(q, &declTask);
        currentTask->TaskFlags |= Task_Waiting;
        YIELD(waiting_for_declSemantic);
        printf("We are back\n");
        result = CtxValuePtr->Result;
#else
        printf("Fibers not enable cannot deal with out-of-order things without it\n");
#endif
    }
    return result;
}

#include "metac_printer.h"

/// retruns an emptyNode in case it couldn't be found in the cache
metac_node_t MetaCSemantic_LRU_LookupIdentifier(metac_semantic_state_t* self,
                                                 uint32_t idPtrHash,
                                                 metac_identifier_ptr_t idPtr)
{
    uint32_t mask = 0;
    int16x8_t hashes = Load16(&self->LRU.LRUContentHashes);

    metac_node_t result = emptyNode;
    uint16_t hash12 = idPtrHash & 0xFFF0;

    const int16x8_t hash12_8 = Set1_16(hash12);
    const int16x8_t hashMask = Set1_16(0xFFF0);
    const int16x8_t maskedHashes = And16(hashes, hashMask);
    const int16x8_t matches = Eq16(maskedHashes, hash12_8);
    mask = MoveMask16(matches);

    while(mask)
    {
        const uint32_t i = BSF(mask);
        // remove the bit we are going to check
        mask &= ~(i << i);
        if (self->LRU.Slots[i].Ptr.v == idPtr.v)
        {
            result = self->LRU.Slots[i].Node;
            break;
        }
    }

    return result;
}

/// Returns _emptyNode to signifiy it could not be found
/// a valid node otherwise
metac_node_t MetaCSemantic_LookupIdentifier(metac_semantic_state_t* self,
                                            metac_identifier_ptr_t identifierPtr)
{

    metac_node_t result = emptyNode;
    uint32_t idPtrHash = crc32c_nozero(~0, &identifierPtr.v, sizeof(identifierPtr.v));

    metac_scope_t *currentScope = self->CurrentScope;
    {
        while(currentScope)
        {
          metac_node_t lookupResult =
                MetaCScope_LookupIdentifier(currentScope, idPtrHash, identifierPtr);
            if (lookupResult)
            {
                result = lookupResult;
                break;
            }
            currentScope = currentScope->Parent;
        }
    }

    return result;
}

static inline void TypeToCharsP(metac_semantic_state_t* self,
                                metac_printer_t* printer,
                                metac_type_index_t typeIndex)
{
    uint32_t typeIndexIndex = TYPE_INDEX_INDEX(typeIndex);

    switch (TYPE_INDEX_KIND(typeIndex))
    {
        case type_index_array:
        {
            metac_type_array_t* arrayType =
                (self->ArrayTypeTable.Slots + TYPE_INDEX_INDEX(typeIndex));
            TypeToCharsP(self, printer, arrayType->ElementType);
            MetacPrinter_PrintStringLiteral(printer, "[");
            MetacPrinter_PrintI64(printer, (int64_t)arrayType->Dim);
            MetacPrinter_PrintStringLiteral(printer, "]");
        } break;
        case type_index_basic:
        {
            const char* typeString = BasicTypeToChars(typeIndex);
            MetacPrinter_PrintStringLiteral(printer, typeString);
        } break;
        case type_index_ptr:
        {
            metac_type_ptr_t* ptrType =
                (self->PtrTypeTable.Slots + TYPE_INDEX_INDEX(typeIndex));
            TypeToCharsP(self, printer, ptrType->ElementType);
            MetacPrinter_PrintStringLiteral(printer, "*");
        } break;
        case type_index_tuple:
        {
            metac_type_tuple_t* tupleType =
                (self->TupleTypeTable.Slots + TYPE_INDEX_INDEX(typeIndex));
            MetacPrinter_PrintStringLiteral(printer, "{");
            const uint32_t typeCount = tupleType->typeCount;
            for(uint32_t i = 0;
                i < tupleType->typeCount;
                i++)
            {
                TypeToCharsP(self, printer, tupleType->typeIndicies[i]);
                if (i != (typeCount - 1))
                {
                    MetacPrinter_PrintStringLiteral(printer, ", ");
                }
            }
            MetacPrinter_PrintStringLiteral(printer, "}");
        } break;
    }
}

const char* TypeToChars(metac_semantic_state_t* self, metac_type_index_t typeIndex)
{
    const char* result = 0;
    static metac_printer_t printer = {0};
    if (!printer.StringMemory)
        MetaCPrinter_InitSz(&printer, self->ParserIdentifierTable, 0, 32);
    else
        MetaCPrinter_Reset(&printer);
    TypeToCharsP(self, &printer, typeIndex);
    printer.StringMemory[printer.StringMemorySize++] = '\0';
    result = printer.StringMemory;

    return result;
}

#include <stdio.h>


#undef offsetof

#define offsetof(st, m) \
    ((size_t)((char *)&((st *)0)->m - (char *)0))



const metac_scope_t* GetAggregateScope(metac_type_aggregate_t* agg)
{
    return agg->Scope;
}


static inline int32_t GetConstI32(metac_semantic_state_t* self, metac_sema_expression_t* index, bool *errored)
{
    int32_t result = ~0;

    if (index->Kind == exp_signed_integer)
    {
        result = cast(int32_t) index->ValueI64;
    }
    else
    {
        *errored = true;
    }

    return result;
}

metac_sema_expression_t* MetaCSemantic_doIndexSemantic_(metac_semantic_state_t* self,
                                                        metac_expression_t* expr,
                                                        const char* callFun,
                                                        uint32_t callLine)
{
    metac_sema_expression_t* result = 0;

    metac_sema_expression_t* indexed = MetaCSemantic_doExprSemantic(self, expr->E1, 0/*, expr_asAddress*/);
    metac_sema_expression_t* index = MetaCSemantic_doExprSemantic(self, expr->E2, 0);
    if (indexed->Kind ==  exp_tuple)
    {
        bool errored = false;
        int32_t idx = GetConstI32(self, index, &errored);
        if ((int32_t)indexed->TupleExpressionCount > idx)
        {
            result = indexed->TupleExpressions + idx;
        }
        else if (!errored)
        {
            fprintf(stderr, "TupleIndex needs to be less than: %u", indexed->TupleExpressionCount);
        }
        else
        {
            fprintf(stderr, "index is not a constant value\n");
        }

    }

    return  result;
}

