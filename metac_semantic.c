#include "metac_semantic.h"
#include <assert.h>
#include "metac_alloc_node.h"
#include "metac_target_info.h"
#include "metac_default_target_info.h"
#include "bsf.h"
#include <stdlib.h>
#include "crc32c.h"
#define AT(...)

bool IsExpressionNode(metac_node_kind_t);

bool Expression_IsEqual_(metac_sema_expression_t* a,
                         metac_sema_expression_t* b)
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

static inline bool isBasicType(metac_type_kind_t typeKind)
{
    if ((typeKind >= type_void) & (typeKind <= type_unsigned_long_long))
    {
        return true;
    }
    return false;
}

static uint32_t _nodeCounter = 64;

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

uint32_t FieldHash(metac_type_aggregate_field_t* field)
{
#if NDEBUG
    if (field->Hash)
        return field->Hash;
#endif
    uint32_t hash = ~0;
    hash = CRC32C_VALUE(hash, field->Type);
    hash = CRC32C_VALUE(hash, field->Offset);
    assert(!field->Header.Hash || field->Header.Hash == hash);
    return hash;
}

uint32_t AggregateHash(metac_type_aggregate_t* agg)
{
    if (agg->Header.Hash)
        return agg->Header.Hash;

    uint32_t hash = ~0;
    for(int i = 0; i < agg->FieldCount; i++)
    {
        metac_type_aggregate_field_t* field = agg->Fields +i;
        if (!field->Header.Hash)
        {
            field->Header.Hash = FieldHash(field);
        }
        hash = CRC32C_VALUE(hash, field->Header.Hash);
    }

    return hash;
}

#ifndef ATOMIC
#define POST_ADD(v, b) \
    (v += b, v - b)
#else
#define POST_ADD(v, b)
    (__builtin_atomic_fetch_add(&v, b))
#endif

metac_sema_expression_t* AllocNewSemaExpression(metac_semantic_state_t* self, metac_expression_t* expr)
{
    metac_sema_expression_t* result = 0;

    REALLOC_BOILERPLATE(self->Expressions)

    {
        result = self->Expressions + INC(self->Expressions_size);
        (*(metac_expression_header_t*) result) = (*(metac_expression_header_t*) expr);

        result->TypeIndex.v = 0;
        memcpy(
               ((char*)result) + sizeof(metac_sema_expression_header_t),
               ((char*)expr) + sizeof(metac_expression_header_t),
               sizeof(metac_expression_t) - sizeof(metac_expression_header_t));
        result->Serial = INC(_nodeCounter);
    }

    if (expr->Kind == exp_tuple)
    {
        const uint32_t tupleExpCount = expr->TupleExpressionCount;
        REALLOC_N_BOILERPLATE(self->Expressions, tupleExpCount);

        metac_sema_expression_t* elements =
            self->Expressions + POST_ADD(self->Expressions_size, tupleExpCount);
        for(uint32_t i = 0;
            i < tupleExpCount;
            i++)
        {
            (elements + i)->Kind = exp_invalid;
            //(elements + i)->Serial = INC(_nodeCounter);
        }
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
        // result->Parent = 0;

        result->Serial = INC(_nodeCounter);
        result->Body = (metac_sema_statement_t*)(result + 1);
        // result->TypeIndex.v = 0;
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
            metac_type_index_t* newParams =
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
                        metac_type_aggregate_t* struct_ =
                cast(metac_type_aggregate_t*) node;
            uint32_t hash = struct_->Header.Hash;
            assert(hash);


            metac_type_index_t typeIndex =
                MetaCTypeTable_GetOrEmptyFunctionType(&srcState->FunctionTypeTable,
                                                    struct_);
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

    MetaCDeclaration_Walk(decl, HandoffWalker, &handoff_context);

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

    IdentifierTableInit(&self->SemanticIdentifierTable, IDENTIFIER_LENGTH_SHIFT);
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
#define MetaCSemantic_PopTemporaryScope(SELF) \
    MetaCSemantic_PopTemporaryScope_(SELF, __LINE__, __FILE__)
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
                                                           const char* callFun,
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

        case stmt_return:
        {
            stmt_return_t* returnStatement = (stmt_return_t*) stmt;
            sema_stmt_return_t* semaReturnStatement =
                AllocNewSemaStatement(self, stmt_return, &result);

            metac_sema_expression_t* returnValue =
                MetaCSemantic_doExprSemantic(self, returnStatement->Expression);
            semaReturnStatement->Expression = returnValue;
        } break;
    }

    return result;
}

metac_type_index_t MetaCSemantic_GetTypeIndex(metac_semantic_state_t* state,
                                              metac_type_kind_t typeKind,
                                              decl_type_t* type)
{
    metac_type_index_t result = {0};

    if (isBasicType(typeKind))
    {
        result.v = TYPE_INDEX_V(type_index_basic, (uint32_t) typeKind);

        assert((type == emptyPointer) || type->TypeKind == typeKind);
        if ((type != emptyPointer) && (type->TypeModifiers & typemod_unsigned))
        {
            if((typeKind >= type_char) & (typeKind <= type_long))
            {
                result.v +=
                    (((uint32_t)type_unsigned_char) - ((uint32_t)type_char));
            }
            else if (typeKind == type_long_long)
            {
                result.v +=
                    (((uint32_t)type_unsigned_long_long) - ((uint32_t)type_long_long));
            }
            else
            {
                //TODO Real error macro
                fprintf(stderr, "modifier unsigned cannot be applied to: %s\n", TypeToChars(state, result));
            }
        }
    }

    return result;
}

bool IsAggregateTypeDecl(metac_declaration_kind_t declKind)
{
    if (declKind == decl_type_struct || declKind == decl_type_union)
    {
        return true;
    }
    return false;
}

bool IsPointerType(metac_declaration_kind_t declKind)
{
    if (declKind == decl_type_ptr)
    {
        return true;
    }
    return false;
}

#define INVALID_SIZE ((uint32_t)-1)
#ifndef U32
#define U32(VAR) \
    (*(uint32_t*)(&VAR))
#endif

static inline uint32_t Align(uint32_t size, uint32_t alignment)
{
    assert(alignment >= 1);
    uint32_t alignmentMinusOne = (alignment - 1);
    uint32_t alignSize = (size + alignmentMinusOne);
    uint32_t alignMask = ~(alignmentMinusOne);
    return (alignSize & alignMask);
}


uint32_t MetaCSemantic_GetTypeAlignment(metac_semantic_state_t* self,
                                        metac_type_index_t typeIndex)
{
    uint32_t result = INVALID_SIZE;

    if (TYPE_INDEX_KIND(typeIndex) == type_index_basic)
    {
        uint32_t idx = TYPE_INDEX_INDEX(typeIndex);

        if ((idx >= type_unsigned_char)
         && (idx <= type_unsigned_long))
        {
            idx -= ((uint32_t)type_unsigned_char - (uint32_t)type_char);
        }
        else if (idx == type_unsigned_long_long)
        {
            idx = type_long_long;
        }
        result =
            MetaCTargetInfo_GetBasicAlign(&default_target_info, (basic_type_kind_t) idx);
    }
    else if (TYPE_INDEX_KIND(typeIndex) == type_index_ptr
        ||   TYPE_INDEX_KIND(typeIndex) == type_index_functiontype)
    {
        result = default_target_info.AlignmentSizeT;
    }
    else if (TYPE_INDEX_KIND(typeIndex) == type_index_typedef)
    {
        uint32_t idx = TYPE_INDEX_INDEX(typeIndex);
        metac_type_index_t elementTypeIndex =
            self->TypedefTypeTable.Slots[idx].Type;

        result = MetaCSemantic_GetTypeAlignment(self,
                                           elementTypeIndex);
    }
    else if (TYPE_INDEX_KIND(typeIndex) == type_index_struct)
    {
        metac_type_aggregate_t* struct_ = StructPtr(self, TYPE_INDEX_INDEX(typeIndex));
        result = struct_->Alignment;
    }
    else if (TYPE_INDEX_KIND(typeIndex) == type_index_array)
    {
        uint32_t idx = TYPE_INDEX_INDEX(typeIndex);
        metac_type_array_t* arrayType_ = ArrayTypePtr(self, idx);
        metac_type_index_t elementTypeIndex = arrayType_->ElementType;
        result = MetaCSemantic_GetTypeAlignment(self, elementTypeIndex);
    }

    else
    {
        assert(0);
    }

    return result;
}

/// Returns size in byte or INVALID_SIZE on error
uint32_t MetaCSemantic_GetTypeSize(metac_semantic_state_t* self,
                                   metac_type_index_t typeIndex)
{
    uint32_t result = INVALID_SIZE;

    if (TYPE_INDEX_KIND(typeIndex) == type_index_basic)
    {
        uint32_t idx = TYPE_INDEX_INDEX(typeIndex);

        if ((idx >= type_unsigned_char)
         && (idx <= type_unsigned_long))
        {
            idx -= ((uint32_t)type_unsigned_char - (uint32_t)type_char);
        }
        else if (idx == type_unsigned_long_long)
        {
            idx = type_long_long;
        }
        result =
            MetaCTargetInfo_GetBasicSize(&default_target_info, (basic_type_kind_t) idx);
    }
    else if (TYPE_INDEX_KIND(typeIndex) == type_index_ptr
        ||   TYPE_INDEX_KIND(typeIndex) == type_index_functiontype)
    {
        result = default_target_info.PtrSize;
    }
    else if (TYPE_INDEX_KIND(typeIndex) == type_index_typedef)
    {
        uint32_t idx = TYPE_INDEX_INDEX(typeIndex);
        metac_type_index_t elementTypeIndex =
            self->TypedefTypeTable.Slots[idx].Type;

        result = MetaCSemantic_GetTypeSize(self,
                                           elementTypeIndex);
    }
    else if (TYPE_INDEX_KIND(typeIndex) == type_index_struct)
    {
        metac_type_aggregate_t* struct_ = StructPtr(self, TYPE_INDEX_INDEX(typeIndex));
        result = struct_->Size;
    }
    else if (TYPE_INDEX_KIND(typeIndex) == type_index_array)
    {
        uint32_t idx = TYPE_INDEX_INDEX(typeIndex);
        metac_type_array_t* arrayType = ArrayTypePtr(self, idx);
        metac_type_index_t elementTypeIndex = arrayType->ElementType;

        uint32_t baseSize = MetaCSemantic_GetTypeSize(self,
                                                      elementTypeIndex);
        uint32_t alignedSize = Align(baseSize,
                                     MetaCSemantic_GetTypeAlignment(self,
                                                                    elementTypeIndex));
        result = alignedSize * arrayType->Dim;
    }
    else
    {
        assert(0);
    }

    return result;
}

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
//    if (idPtr.v == 40)
//        int breakMe = 2;
    scope_insert_error_t result = no_scope;
    uint32_t idPtrHash = crc32c_nozero(~0, &idPtr.v, sizeof(idPtr.v));
    // first search for keys that might clash
    // and remove them from the LRU hashes
    uint16_t hash12 = idPtrHash & 0xFFF0;
    int16x8_t hashes;

#ifdef SSE2
    hashes.XMM = _mm_load_si128((__m128i*) self->LRU.LRUContentHashes.E);
#else
    hashes = self->LRU.LRUContentHashes;
#endif

#ifdef SSE2
    const __m128i key = _mm_set1_epi16(hash12);
    const __m128i keyMask = _mm_set1_epi16(0xF);
    // mask out the scope_hash so we only compare keys
    // TODO can we make this store conditinal or would
    // the additional movemask make this slower?
    __m128i masked = _mm_andnot_si128(keyMask, hashes.XMM);
    __m128i cmp = _mm_cmpeq_epi16(masked, key);
    // clear out all the LRU hash entires where the key matched
    __m128i cleaned = _mm_andnot_si128(cmp, hashes.XMM);

    _mm_store_si128((__m128i*) self->LRU.LRUContentHashes.E, cleaned);
#else
    bool needsStore = false;
    for(int i = 0; i < ARRAY_SIZE(hashes.E); i++)
    {
        if ((hashes.E[i] & 0xFFF0) == hash12)
        {
            needsStore |= true;
            hashes.E[i] = 0;
        }
    }
    if (needsStore)
        self->LRU.LRUContentHashes = hashes;
#endif
    if (self->CurrentScope != 0)
        result = MetaCScope_RegisterIdentifier(self->CurrentScope, idPtr, node);

    return result;
}

bool MetaCSemantic_ComputeStructLayoutPopulateScope(metac_semantic_state_t* self,
                                                    decl_type_struct_t* agg,
                                                    metac_type_aggregate_t* semaAgg)
{
    bool result = true;

    assert(semaAgg->Fields && semaAgg->Fields != emptyPointer);

    uint32_t currentFieldOffset = 0;
    uint32_t alignment = 1;

    metac_type_aggregate_field_t* onePastLast =
        semaAgg->Fields + semaAgg->FieldCount;
    decl_field_t* declField = agg->Fields;

    // first determine all the types which will determine the sizes;
    for(metac_type_aggregate_field_t* semaField = semaAgg->Fields;
        semaField < onePastLast;
        semaField++)
    {
        semaField->Identifier = declField->Field->VarIdentifier;

        semaField->Type =
            MetaCSemantic_doTypeSemantic(self, declField->Field->VarType);
        declField = declField->Next;
    }
    uint32_t maxAlignment = semaAgg->FieldCount ?
        MetaCSemantic_GetTypeAlignment(self, semaAgg->Fields->Type) :
        ~0;

    for(metac_type_aggregate_field_t* semaField = semaAgg->Fields;
        semaField < onePastLast;
        semaField++)
    {
        uint32_t alignedSize = MetaCSemantic_GetTypeSize(self, semaField->Type);
        assert(alignedSize != INVALID_SIZE);
        if (semaField < (onePastLast - 1))
        {
            metac_type_aggregate_field_t *nextField = semaField + 1;
            uint32_t requestedAligment =
                MetaCSemantic_GetTypeAlignment(self, nextField->Type);
            assert(requestedAligment != -1);
            if (requestedAligment > maxAlignment)
                maxAlignment = requestedAligment;
            alignedSize = Align(alignedSize, requestedAligment);
            assert(((currentFieldOffset + alignedSize) % requestedAligment) == 0);
        }
        semaField->Offset = currentFieldOffset;
        MetaCScope_RegisterIdentifier(semaAgg->Scope,
                                      semaField->Identifier,
                                      (metac_node_t)semaField);
        currentFieldOffset += alignedSize;
    }

    // result =
    fprintf(stderr, "sizeof(struct) = %u\n", currentFieldOffset);//DEBUG

    semaAgg->Size = currentFieldOffset;
    semaAgg->Alignment = maxAlignment;

    return result;
}

metac_type_index_t MetaCSemantic_GetPtrTypeOf(metac_semantic_state_t* self,
                                              metac_type_index_t elementTypeIndex)
{
    uint32_t hash = elementTypeIndex.v;
    metac_type_ptr_t key =
            {(metac_type_header_t){node_decl_type_typedef, 0, hash, 0},
             elementTypeIndex};

    metac_type_index_t result =
        MetaCTypeTable_GetOrEmptyPtrType(&self->PtrTypeTable, &key);
    if (result.v == 0)
    {
        result = MetaCTypeTable_AddPtrType(&self->PtrTypeTable, &key);
    }
    return result;
}

#if 1
metac_type_aggregate_t* MetaCSemantic_PersistTemporaryAggregate(metac_semantic_state_t* self,
                                                                metac_type_aggregate_t* tmpAgg)
{
    // memcpy(semaAgg, tmpAgg, sizeof(metac_type_aggregate_t));

    metac_type_index_t typeIndex =
        MetaCTypeTable_AddStructType(&self->StructTypeTable, tmpAgg);
    metac_type_aggregate_t* semaAgg = StructPtr(self, TYPE_INDEX_INDEX(typeIndex));
    metac_type_aggregate_field_t* semaFields =
            calloc(sizeof(metac_type_aggregate_field_t), tmpAgg->FieldCount);
        //AllocAggregateFields(self, semaAgg, tmpAgg->Header.Kind, tmpAgg->FieldCount);

    //TODO FIXME this should use a deep walk through the fields
    //MetaCDeclaration_Walk(tmpAgg,)
    memcpy(semaFields, tmpAgg->Fields,
        sizeof(metac_type_aggregate_field_t) * tmpAgg->FieldCount);
    semaAgg->Fields = semaFields;

    return semaAgg;
}
#endif

metac_type_index_t MetaCSemantic_GetArrayTypeOf(metac_semantic_state_t* state,
                                                metac_type_index_t elementTypeIndex,
                                                uint32_t dimension)
{
    uint32_t hash = EntangleInts(TYPE_INDEX_INDEX(elementTypeIndex), dimension);
    metac_type_array_t key = {
            (metac_type_header_t){decl_type_array, 0, hash}, elementTypeIndex, dimension};

    metac_type_index_t result =
        MetaCTypeTable_GetOrEmptyArrayType(&state->ArrayTypeTable, &key);

    if (result.v == 0)
    {
        result =
            MetaCTypeTable_AddArrayType(&state->ArrayTypeTable, &key);
    }

    return result;
}
const char* MetaCExpressionKind_toChars(metac_expression_kind_t);

metac_type_index_t MetaCSemantic_doTypeSemantic_(metac_semantic_state_t* self,
                                                 decl_type_t* type,
                                                 const char* callFun,
                                                 uint32_t callLine)
{
    metac_type_index_t result = {0};

    metac_type_kind_t typeKind = type->TypeKind;

    if (type->DeclKind == decl_type && isBasicType(typeKind))
    {
        result = MetaCSemantic_GetTypeIndex(self, typeKind, type);
    }
    else if (type->DeclKind == decl_type_array)
    {
        decl_type_array_t* arrayType =
            cast(decl_type_array_t*)type;
        metac_type_index_t elementType =
            MetaCSemantic_doTypeSemantic(self, arrayType->ElementType);
        metac_sema_expression_t* dim =
            MetaCSemantic_doExprSemantic(self, arrayType->Dim);
        if (dim->Kind != exp_signed_integer)
        {
            printf("Array dimension should eval to integer but it is: %s\n",
                MetaCExpressionKind_toChars(dim->Kind));
        }
        result =
            MetaCSemantic_GetArrayTypeOf(self,
                                        elementType,
                                        (uint32_t)dim->ValueU64);
        int k = 12;
    }
    else if (type->DeclKind == decl_type_typedef)
    {
        decl_type_typedef_t* typedef_ = (decl_type_typedef_t*) type;
        metac_type_index_t elementTypeIndex =
            MetaCSemantic_doTypeSemantic(self, typedef_->Type);

        uint32_t hash = elementTypeIndex.v;

        metac_type_typedef_t key = {
            (metac_type_header_t){decl_type_typedef, 0, hash},
            elementTypeIndex, typedef_->Identifier
        };

        result =
            MetaCTypeTable_GetOrEmptyTypedefType(&self->TypedefTypeTable, &key);
        if (result.v == 0)
        {
            result = MetaCTypeTable_AddTypedefType(&self->TypedefTypeTable, &key);
        }
        metac_type_typedef_t* semaTypedef = TypedefPtr(self, TYPE_INDEX_INDEX(result));
        //semaTypedef->Type = elementTypeIndex;

        scope_insert_error_t scopeInsertError =
            MetaCSemantic_RegisterInScope(self, typedef_->Identifier, (metac_node_t)semaTypedef);
    }
    else if (IsAggregateTypeDecl(type->DeclKind))
    {
        decl_type_struct_t* agg = (decl_type_struct_t*) type;
        if (type->DeclKind == decl_type_struct)
            typeKind = type_struct;
        else if (type->DeclKind == decl_type_union)
            typeKind = type_union;
        else
            assert(0);
/*
        metac_type_aggregate_t* semaAgg = AllocNewAggregate(agg->DeclKind);
        semaAgg->FieldCount = agg->FieldCount;

        metac_type_aggregate_field_t* semaFields =
            AllocAggregateFields(semaAgg, typeKind, agg->FieldCount);
        semaAgg->Fields = semaFields;
*/
        // for starters we allocate a temporary structure of our aggregate
        // where we then do the layout determination we need to look it up in the cache


       metac_type_aggregate_field_t* tmpFields
            = alloca(sizeof(metac_type_aggregate_field_t)
            * agg->FieldCount);
        // ideally it wouldn't matter if this memset was here or not
        memset(tmpFields, 0x0, sizeof(*tmpFields) * agg->FieldCount);

        metac_type_aggregate_t tmpSemaAggMem = {0};
        tmpSemaAggMem.Header.Kind = agg->DeclKind;
        metac_type_aggregate_t* tmpSemaAgg = &tmpSemaAggMem;
        tmpSemaAgg->Fields = tmpFields;
        tmpSemaAgg->FieldCount = agg->FieldCount;
        metac_scope_t tmpScopeMem = { scope_flag_temporary };
        MetaCScopeTable_Init(&tmpScopeMem.ScopeTable);

        metac_scope_parent_kind_t scopeKind = scope_parent_invalid;

        switch(typeKind)
        {
            case type_struct:
                scopeKind = scope_parent_struct;
            break;
            case type_union:
                scopeKind = scope_parent_union;
            break;
            default: assert(0);
        }

        // XXX temporary scope might want to know thier parents
        tmpSemaAgg->Scope = MetaCSemantic_PushTemporaryScope(self, &tmpScopeMem);

        switch(typeKind)
        {
            case type_struct:
            {
                MetaCSemantic_ComputeStructLayoutPopulateScope(self, agg, tmpSemaAgg);

                uint32_t hash = AggregateHash(tmpSemaAgg);
                tmpSemaAgg->Header.Hash = hash;

                result =
                    MetaCTypeTable_GetOrEmptyStructType(&self->StructTypeTable, tmpSemaAgg);
                if (result.v == 0)
                {
                    metac_type_aggregate_t* semaAgg;
#if 1
// We should really get this persist stuff working.
                    semaAgg =
                        MetaCSemantic_PersistTemporaryAggregate(self, tmpSemaAgg);
#else
                    semaAgg = tmpSemaAgg;
                                       result =
                    MetaCTypeTable_AddStructType(&self->StructTypeTable, semaAgg);
#endif
                    result.v = TYPE_INDEX_V(type_index_struct, semaAgg - self->StructTypeTable.Slots);
                }
                // MetaCSemantic_RegisterInScopeStructNamespace()
            } break;

            case type_union:
            {

            } break;

            case type_class:
            {
                assert(0);
                // Not implemented yet
            } break;

            default: assert(0);

        }
        MetaCScopeTable_Free(&tmpSemaAgg->Scope->ScopeTable);

        MetaCSemantic_PopTemporaryScope(self);
    }
    else if (IsPointerType(type->DeclKind))
    {
        metac_type_index_t elementTypeIndex = {0};
        decl_type_ptr_t* typePtr = (decl_type_ptr_t*) type;
        elementTypeIndex =
            MetaCSemantic_doTypeSemantic(self, typePtr->ElementType);

        result = MetaCSemantic_GetPtrTypeOf(self, elementTypeIndex);
    }
    else if (type->DeclKind == decl_type_functiontype)
    {
        decl_type_functiontype_t* functionType =
            (decl_type_functiontype_t*) type;
        metac_scope_t tmpScope = { scope_flag_temporary };
        MetaCScopeTable_Init(&tmpScope.ScopeTable);

        MetaCSemantic_PushTemporaryScope(self, &tmpScope);

        metac_type_index_t returnType =
            MetaCSemantic_doTypeSemantic(self,
                functionType->ReturnType);

        uint32_t hash = crc32c_nozero(~0, &returnType, sizeof(returnType));
        decl_parameter_t* currentParam = functionType->Parameters;

        const uint32_t nParams = functionType->ParameterCount;
        metac_type_index_t* parameterTypes =
            malloc(sizeof(metac_type_index_t) * nParams);

        for(uint32_t i = 0;
            i < nParams;
            i++)
        {
            parameterTypes[i] =
                MetaCSemantic_doTypeSemantic(self, currentParam->Parameter->VarType);
            currentParam = currentParam->Next;
        }

        hash = crc32c_nozero(hash, &parameterTypes, sizeof(*parameterTypes) * nParams);
        metac_type_functiontype_t key = {
            {hash, 0, decl_type_functiontype }
            , returnType, parameterTypes, nParams };

        MetaCSemantic_PopTemporaryScope(self);

        result = MetaCTypeTable_GetOrEmptyFunctionType(&self->FunctionTypeTable, &key);
        if (result.v == 0)
        {
            result =
                MetaCTypeTable_AddFunctionType(&self->FunctionTypeTable, &key);
        }
        printf("Should have retrived function type\n");
    }
    else if (type->DeclKind == decl_type && type->TypeKind == type_identifier)
    {
        printf("MetaCNodeKind_toChars: %s\n", MetaCNodeKind_toChars(type->DeclKind));
        printf("TypeIdentifier: %s\n",
            IdentifierPtrToCharPtr(self->ParserIdentifierTable, type->TypeIdentifier));
        if (callLine == 1200 && self->TemporaryScopeDepth == 0)
        {
            int breakmeHere = 12;
        }
        metac_node_t node =
            MetaCSemantic_LookupIdentifier(self, type->TypeIdentifier);
        {
            printf("Lookup: %s\n", ((node != emptyNode) ?
                                       MetaCNodeKind_toChars(node->Kind) :
                                       "empty"));
        }
        if (node != emptyNode &&
            node->Kind == node_decl_type_typedef)
        {
            metac_type_typedef_t* typedef_ = (metac_type_typedef_t*)node;
            result = typedef_->Type;
        }
        if (node != emptyNode &&
            node->Kind == node_decl_type_struct)
        {
            metac_type_aggregate_t* struct_ = (metac_type_aggregate_t*)node;
            result.v = TYPE_INDEX_V(type_index_struct, StructIndex(self, (struct_)));
        }
/*
        if (node == emptyNode)
        {
            node =
                MetaCSemantic_Resolve(self, type);
        }
*/
    }
    else
    {
        assert(0); // me not no what do do.
    }

    assert(result.v != 0);
    assert(result.v != 40);
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
    for(int i = 0;
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

    uint32_t frameOffset = 0;

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
    self->FrameOffset = frameOffset;
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
    }

    return 0;
}

metac_sema_declaration_t* MetaCSemantic_doDeclSemantic_(metac_semantic_state_t* self,
                                                        metac_declaration_t* decl,
                                                        const char* callFun,
                                                        uint32_t callLine)
{
    metac_sema_declaration_t* result = 0;
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
                var->VarInitExpression = MetaCSemantic_doExprSemantic(self, v->VarInitExpression);
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
                result = node;
            }
        } break;
    }



    return result;
}

#ifndef ATOMIC
#define INC(v) \
    (v++)
#else
#define INC(v)
    (__builtin_atomic_fetch_add(&v, __ATOMIC_RELEASE))
#endif

#include "metac_printer.h"
static inline const char* BasicTypeToChars(metac_type_index_t typeIndex)
{
    assert(TYPE_INDEX_KIND(typeIndex) == type_index_basic);
    switch((metac_type_kind_t) TYPE_INDEX_INDEX(typeIndex))
    {
        case type_invalid :
            assert(0);

        case type_type:
            return "type";

        case type_void :
            return "void";

        case type_bool :
            return "bool";
        case type_char:
            return "char";
        case type_short:
            return "short";
        case type_int :
            return "int";
        case type_long :
            return "long";
        case type_size_t:
            return "size_t";
        case type_long_long:
            return "long long";

        case type_float :
            return "float";
        case type_double :
            return "double";

        case type_unsigned_char:
            return "unsigned char";
        case type_unsigned_short:
            return "unsigned short";
        case type_unsigned_int:
            return "unsigned int";
        case type_unsigned_long :
            return "unsigned long";
        case type_unsigned_long_long:
            return "unsigned long long";

        default: assert(0);
    }
    return 0;
}

/// retruns an emptyNode in case it couldn't be found in the cache
metac_node_t MetaCSemantic_LRU_LookupIdentifier(metac_semantic_state_t* self,
                                                 uint32_t idPtrHash,
                                                 metac_identifier_ptr_t idPtr)
{
    uint32_t mask = 0;
    int16x8_t hashes;

    metac_node_t result = emptyNode;
#ifdef SSE2
    hashes.XMM = _mm_load_si128((__m128i*) self->LRU.LRUContentHashes.E);
#else
    hashes = self->LRU.LRUContentHashes;
#endif
    uint16_t hash12 = idPtrHash & 0xFFF0;

    uint32_t startSearch = 0;

#ifdef SSE2
    __m128i key = _mm_set1_epi16(hash12);
     const __m128i keyMask = _mm_set1_epi16(0xF);
    // mask out the scope_hash so we only compare keys
    __m128i masked = _mm_andnot_si128(keyMask, hashes.XMM);
    __m128i cmp = _mm_cmpeq_epi16(masked, key);
    mask = _mm_movemask_epi16(cmp);
#else
    for(int i = 0; i < ARRAY_SIZE(hashes.E); i++)
    {
        if ((hashes.E[i] & 0xFFF0) == hash12)
        {
            mask |= (1 << i);
        }
    }
#endif

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

void MetaCSemantic_PushExpr(metac_semantic_state_t* self, metac_sema_expression_t* expr)
{
    if (self->ExpressionStackCapacity < self->ExpressionStackSize)
    {
        assert(0);
        // we would need to realloc in this case.
    }
}

void MetaCSemantic_PopExpr(metac_semantic_state_t* self,  metac_sema_expression_t* expr)
{

}

bool MetaCSemantic_CanHaveAddress(metac_semantic_state_t* self,
                                  metac_expression_t* expr)
{
    switch (expr->Kind)
    {
        case exp_identifier:
            return true;
        default: return false;
    }
}
#include <stdio.h>


#undef offsetof

#define offsetof(st, m) \
    ((size_t)((char *)&((st *)0)->m - (char *)0))

///TODO FIXME
/// this is not nearly complete!
metac_type_index_t MetaCSemantic_CommonSubtype(metac_semantic_state_t* self,
                                               metac_type_index_t a,
                                               metac_type_index_t b)
{
    if (a.v == b.v)
        return a;
    else
        return (metac_type_index_t){-1};
}

const metac_scope_t* GetAggregateScope(metac_type_aggregate_t* agg)
{
    return agg->Scope;
}

static bool IsAggregateType(metac_type_index_kind_t typeKind)
{
    switch(typeKind)
    {
        case type_index_struct:
        case type_index_union:
        case type_index_class:
            return true;
    }

    return false;
}
metac_sema_expression_t* MetaCSemantic_doExprSemantic_(metac_semantic_state_t* self,
                                                       metac_expression_t* expr,
                                                       const char* callFun,
                                                       uint32_t callLine)
{
    metac_sema_expression_t* result = 0;

    result = AllocNewSemaExpression(self, expr);

    if (IsBinaryExp(expr->Kind)
        && (expr->Kind != exp_arrow && expr->Kind != exp_dot))
    {
        MetaCSemantic_PushExpr(self, result);

        result->E1 = MetaCSemantic_doExprSemantic(self, expr->E1);
        result->E2 = MetaCSemantic_doExprSemantic(self, expr->E2);

        MetaCSemantic_PopExpr(self, result);
    }

    switch(expr->Kind)
    {
        case exp_invalid:
            assert(0);

        case exp_arrow:
        case exp_dot:
        {
            result->E1 = MetaCSemantic_doExprSemantic(self, expr->E1);
            // for the semantic of E2 we need to do the lookup in the scope of
            // the type of E1
            const metac_type_index_kind_t typeIndexKind
                = TYPE_INDEX_KIND(result->E1->TypeIndex);
            uint32_t typeIndexIndex = TYPE_INDEX_INDEX(result->E1->TypeIndex);

            if (IsAggregateType(typeIndexKind))
            {

                metac_type_aggregate_t* agg = 0;
                switch(typeIndexKind)
                {
                case type_index_struct:
                    agg = StructPtr(self, typeIndexIndex);
                break;
                case type_index_union:
                    agg = UnionPtr(self, typeIndexIndex);
                break;
                case type_index_class:
                    assert(0);
                }
                assert(agg != 0);

                assert(expr->E2->Kind == exp_identifier);
                uint32_t idPtrHash = crc32c(~0,
                                            &expr->E2->IdentifierPtr,
                                            sizeof(expr->E2->IdentifierPtr));
#if 0
// TODO enable scope search! which means we store a compacted scope table on persistance of the tmp scope
                metac_node_t node =
                    MetaCScope_LookupIdentifier(agg->Scope,
                                                idPtrHash,
                                                expr->E2->IdentifierPtr);

                if (node == emptyNode)
                {
                    break;
                }
#else
                result->AggExp =
                    MetaCSemantic_doExprSemantic(self, expr->E1);

                metac_type_aggregate_field_t* fields = agg->Fields;
                for(uint32_t i = 0;
                    i < agg->FieldCount;
                    i++)
                {
                    metac_type_aggregate_field_t field = fields[i];
                    if (field.Identifier.v == expr->E2->IdentifierPtr.v)
                    {
                        //printf("Found field: %s\n",
                        //    IdentifierPtrToCharPtr(self->ParserIdentifierTable, field.Identifier));
                        result->TypeIndex = field.Type;
                        result->AggOffset = field.Offset;
                    }
                }
#endif
            }
        } break;

        case exp_typeof:
        {
            metac_sema_expression_t* E1 =
                MetaCSemantic_doExprSemantic(self, expr->E1);
            //result->Kind = exp_type;
            result->TypeIndex.v =
                TYPE_INDEX_V(type_index_basic, type_type);
            result->TypeExp = E1->TypeIndex;
        } break;

        case exp_paren:
        {
            metac_sema_expression_t* E1 =
                MetaCSemantic_doExprSemantic(self, expr->E1);
            //result->Kind = exp_paren;
            result->TypeIndex = E1->TypeIndex;
            result->E1 = E1;
        } break;

        case exp_add:
            result->TypeIndex =
                MetaCSemantic_CommonSubtype(self, result->E1->TypeIndex, result->E2->TypeIndex);
        break;
        case exp_char :
            result->TypeIndex = MetaCSemantic_GetTypeIndex(self, type_char, (decl_type_t*)emptyPointer);
        break;
        case exp_string :
            result->TypeIndex = MetaCSemantic_GetArrayTypeOf(self,
                MetaCSemantic_GetTypeIndex(self, type_char, (decl_type_t*)emptyPointer),
                LENGTH_FROM_STRING_KEY(expr->StringKey) + 1);
        break;
        case exp_signed_integer :
            result->TypeIndex = MetaCSemantic_GetTypeIndex(self, type_int, (decl_type_t*)emptyPointer);
        break;
        case exp_tuple:
        {
            exp_tuple_t* tupleElement = expr->TupleExpressionList;
            const uint32_t tupleExpressionCount =
                expr->TupleExpressionCount;
            metac_sema_expression_t** resolvedExps = alloca(
                sizeof(metac_sema_expression_t*) * tupleExpressionCount);

            for(int i = 0;
                i < expr->TupleExpressionCount;
                i++)
            {
                metac_expression_t *e = tupleElement->Expression;
                tupleElement = tupleElement->Next;
                metac_printer_t printer;
                MetaCPrinter_Init(&printer, self->ParserIdentifierTable, self->ParserStringTable);
                printf("Doing semantic on tuple Element: %s\n", MetaCPrinter_PrintExpression(&printer, e));
                resolvedExps[i] = MetaCSemantic_doExprSemantic(self, e);
            }

            metac_type_index_t* indicies =
                alloca(sizeof(metac_type_index_t) * expr->TupleExpressionCount);

            for(uint32_t i = 0; i < tupleExpressionCount; i++)
            {
                *(result->TupleExpressions + i) = *resolvedExps[i];
                indicies[i] = resolvedExps[i]->TypeIndex;
            }

           uint32_t hash = crc32c(~0, indicies,
                sizeof(metac_type_index_t) * expr->TupleExpressionCount);

            metac_type_tuple_t typeTuple;
            typeTuple.Header.Kind = node_decl_type_tuple;
            typeTuple.Header.Hash = hash;
            typeTuple.typeCount = tupleExpressionCount;
            typeTuple.typeIndicies = indicies;

           // AllocNewTupleType()
            metac_type_index_t tupleIdx =
                MetaCTypeTable_GetOrEmptyTupleType(&self->TupleTypeTable, &typeTuple);
            if (tupleIdx.v == 0)
            {
                metac_type_index_t* newIndicies = (metac_type_index_t*)
                    malloc(expr->TupleExpressionCount * sizeof(metac_type_index_t));
                memcpy(newIndicies, indicies,
                    expr->TupleExpressionCount * sizeof(metac_type_index_t));
                typeTuple.typeIndicies = newIndicies;
                tupleIdx =
                    MetaCTypeTable_AddTupleType(&self->TupleTypeTable, &typeTuple);
            }
            result->TypeIndex = tupleIdx;
        }
        break;
        case exp_dot_compiler:
        {
            if (expr->E1->Kind != exp_call)
            {
                fprintf(stderr, "Only calls are supported not %s\n",
                    MetaCExpressionKind_toChars(expr->E1->Kind));
                break;
            }
            metac_expression_t* call = expr->E1;
            metac_expression_t* fn = call->E1;
            exp_argument_t* args = (METAC_NODE(call->E2) != emptyNode ?
                call->E2->ArgumentList : (exp_argument_t*)emptyNode);

            printf("Type(fn) %s\n", MetaCExpressionKind_toChars(fn->Kind));

            int callIdx = -1;

            for(int memberIdx = 0;
                memberIdx < self->CompilerInterface->FieldCount;
                memberIdx++)
            {
                metac_identifier_ptr_t id =
                    self->CompilerInterface->Fields[memberIdx].Identifier;
                if (id.v == fn->IdentifierPtr.v)
                {
                    printf("Field: %s\n",
                        IdentifierPtrToCharPtr(self->ParserIdentifierTable, id));

                    printf("Found\n");
                    callIdx = memberIdx;
                    break;
                }
            }
            if (callIdx == -1)
            {
                printf("CallNotFound\n");
                result->Kind = exp_signed_integer;
                result->ValueI64 = 0;
                result->TypeIndex.v = 0;
            }
            else
            {

            }
            // CompilerInterface_Call(
        } break;
        case exp_type:
        {
            metac_type_index_t TypeIndex
                = MetaCSemantic_doTypeSemantic(self, expr->TypeExp);
            result->TypeExp = TypeIndex;
            result->TypeIndex.v = TYPE_INDEX_V(type_index_basic, type_type);
        } break;
        case exp_sizeof:
        {
            uint32_t size = -1;
            metac_sema_expression_t* e1 =
                MetaCSemantic_doExprSemantic(self, expr->E1);
            metac_type_index_t type = e1->TypeIndex;
            if (type.v == TYPE_INDEX_V(type_index_basic, type_type))
            {
                type = e1->TypeExp;
            }

            if (e1->TypeIndex.v != 0 && e1->TypeIndex.v != -1)
                size = MetaCSemantic_GetTypeSize(self, type);

            result->TypeIndex.v = TYPE_INDEX_V(type_index_basic, type_size_t);
            result->Kind = exp_signed_integer;
            result->ValueU64 = size;
        } break;
        case exp_identifier:
        {
            metac_node_t node =
                MetaCSemantic_LookupIdentifier(self,
                                               result->IdentifierPtr);
            if (node == emptyPointer)
            {
                fprintf(stderr, "Identifier lookup failed\n");
            }
            else
            {
                if (node->Kind == (metac_expression_kind_t)exp_identifier)
                {
                    fprintf(stderr, "we should not be retured an identifier\n");
                }
                if (node->Kind == node_decl_variable)
                {
                    sema_decl_variable_t* v = (sema_decl_variable_t*)node;
                    result->Kind = exp_variable;
                    result->Variable = v;
                    result->TypeIndex = v->TypeIndex;
                }
            }
        }
        break;
        case exp_addr:
            MetaCSemantic_PushExpr(self, result);
            result->E1 = MetaCSemantic_doExprSemantic(self, expr->E1);
            MetaCSemantic_PopExpr(self, result);
            assert(result->E1->TypeIndex.v != 0 && result->E1->TypeIndex.v != ERROR_TYPE_INDEX_V);
            if (!MetaCSemantic_CanHaveAddress(self, expr->E1))
            {
                result->TypeIndex.v = ERROR_TYPE_INDEX_V;
                const char* e1String = "E1";
                //TODO FIXME use some global printer to do this
                //e1String = MetaCPrinter_PrintExpression(printer, expr->E1);

                SemanticError(self, "cannot take the address of %s", e1String);
            }
            else
            {
                result->TypeIndex = MetaCSemantic_GetPtrTypeOf(self, result->E1->TypeIndex);
            }
        break;
    }

    return result;
}
