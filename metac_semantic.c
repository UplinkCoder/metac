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
        metac_type_aggregate_field_t field = agg->Fields[i];
        if (!field.Header.Hash)
        {
            field.Header.Hash = FieldHash(&field);
        }
        hash = CRC32C_VALUE(hash, field.Header.Hash);
    }
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
        metac_type_aggregate_slot_t* agg;
        metac_type_aggregate_slot_t* newAgg;

        case type_index_basic: break;
        case type_index_unknown: assert(0);
        case type_index_struct:
        {
            agg = srcState->StructTypeTable.Slots + TYPE_INDEX_INDEX(idx);
            const uint32_t fieldCount = agg->FieldCount;

            metac_type_aggregate_field_t* newFields =
                (metac_type_aggregate_field_t*)
                    malloc(sizeof(metac_type_aggregate_field_t) * fieldCount);
            for(uint32_t i = 0; i < agg->FieldCount; i++)
            {
                metac_type_aggregate_field_t field = agg->Fields[i];
                HandoffIdentifier(dstState->ParserIdentifierTable,
                                  srcState->ParserIdentifierTable,
                                  &field.Identifier);
                HandoffType(dstState, srcState, &field.Type);
                newFields[i] = field;
            }
            uint32_t newHash = 0;
            metac_type_aggregate_slot_t newKey = {
                newHash, fieldCount, newFields
            };
            result =
                MetaCTypeTable_GetOrAddStructType(&dstState->StructTypeTable,
                                                  newHash, &newKey);
            goto LhandoffAggregate;
        }
        case type_index_union:
        {
            agg = srcState->UnionTypeTable.Slots + TYPE_INDEX_INDEX(idx);
            goto LhandoffAggregate;
        }
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
    const handoff_walker_context_t* context =
        (handoff_walker_context_t*) ctx;
    assert(crc32c_nozero(~0, __FUNCTION__, strlen(__FUNCTION__))
        == context->FunctionKey);

    const metac_semantic_state_t* srcState = context->Origin;
    metac_semantic_state_t* dstState = context->NewOwner;

    switch(node->Kind)
    {
        case node_decl_type_struct:
        {
            metac_type_aggregate_t* struct_ =
                cast(metac_type_aggregate_t*) node;
            uint32_t hash = struct_->Header.Hash;
            assert(hash);

            metac_type_aggregate_slot_t structKey = {
                hash,
            };
            metac_type_index_t typeIndex =
                MetaCTypeTable_GetOrAddStructType(&srcState->StructTypeTable,
                    struct_->Header.Hash, &structKey);

            HandoffType(dstState, srcState, &typeIndex);

            node =
                (metac_node_t)StructPtr(TYPE_INDEX_INDEX(typeIndex));
            return 1;
        }
        case node_decl_field:
        {

        } break;
    }

    return 0;
}

/// transfers ownership of decl and all it's dependents from self to newOwner
void MetaCSemantic_Handoff(metac_semantic_state_t* self, metac_sema_declaration_t* decl,
                           metac_semantic_state_t* newOwner)
{
    handoff_walker_context_t handoff_context = {
        crc32c_nozero(~0, "HandoffWalker", sizeof("HandoffWalker") -1),
        self, newOwner, decl
    };

    MetaCDeclaration_Walk(decl, HandoffWalker, &handoff_context);
}

void MetaCSemantic_Init(metac_semantic_state_t* self, metac_parser_t* parser,
                        metac_type_aggregate_t* compilerStruct)
{
#define INIT_TYPE_TABLE(TYPE_NAME, MEMBER_NAME, INDEX_KIND) \
    TypeTableInitImpl((metac_type_table_t*)&self->MEMBER_NAME, \
                      sizeof(metac_type_ ## TYPE_NAME ## _slot_t), \
                      type_index_## INDEX_KIND);

    FOREACH_TYPE_TABLE(INIT_TYPE_TABLE)

    self->ExpressionStackCapacity = 64;
    self->ExpressionStackSize = 0;
    self->ExpressionStack = (metac_sema_expression_t*)
        calloc(sizeof(metac_sema_expression_t), self->ExpressionStackCapacity);

    IdentifierTableInit(&self->SemanticIdentifierTable, IDENTIFIER_LENGTH_SHIFT);
    self->ParserIdentifierTable = &parser->IdentifierTable;

    self->CurrentScope = 0;
    self->CurrentDeclarationState = 0;

    MetaCPrinter_Init(&self->Printer, self->ParserIdentifierTable, &parser->StringTable);
    if (compilerStruct && ((metac_node_t)compilerStruct) != emptyNode)
    {
        self->CompilerInterface = MetaCSemantic_doDeclSemantic(self, compilerStruct);
    }
}

void MetaCSemantic_PopScope(metac_semantic_state_t* self)
{
    assert(self->CurrentScope);
    self->CurrentScope = self->CurrentScope->Parent;
}



metac_scope_t* MetaCSemantic_PushScope(metac_semantic_state_t* self,
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
        scopeParentIndex = FunctionIndex((sema_decl_function_t*)parentNode);
    break;
    case scope_parent_struct :
        scopeParentIndex = StructIndex((metac_type_aggregate_t*)parentNode);
    break;
    case scope_parent_statement :
        scopeParentIndex = StatementIndex((metac_sema_statement_t*)parentNode);
    break;
    case scope_parent_block :
        scopeParentIndex = BlockStatementIndex((sema_stmt_block_t*)parentNode);
    break;
    case scope_parent_union :
        scopeParentIndex = UnionIndex((metac_type_aggregate_t*)parentNode);
    break;
    }
    scopeParent.v = SCOPE_PARENT_V(parentKind, scopeParentIndex);

    self->CurrentScope = MetaCScope_PushScope(self->CurrentScope,
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
            sema_stmt_exp_t* sse = AllocNewSemaStatement(stmt_exp, &result);
        } break;

        default: assert(0);

        case stmt_block:
        {
            stmt_block_t* blockStatement = (stmt_block_t*) stmt;
            uint32_t statementCount = blockStatement->StatementCount;
            sema_stmt_block_t* semaBlockStatement =
                AllocNewSemaStatement(stmt_block, &result);

            metac_scope_parent_t parent = {SCOPE_PARENT_V(scope_parent_statement,
                                           BlockStatementIndex(semaBlockStatement))};

            MetaCSemantic_PushScope(self,
                                    scope_parent_block,
                                    (metac_node_t)semaBlockStatement);

            metac_statement_t* currentStatement = blockStatement->Body;
            for(int i = 0;
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
                AllocNewSemaStatement(stmt_return, &result);

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

bool IsAggregateType(metac_declaration_kind_t declKind)
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
            self->TypedefTypeTable.Slots[idx].ElementTypeIndex;

        result = MetaCSemantic_GetTypeSize(self,
                                           elementTypeIndex);
    }
    else
    {
        assert(0);
    }

    return result;
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
            idx -= ((uint32_t)type_char - (uint32_t)type_unsigned_char);
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
            self->TypedefTypeTable.Slots[idx].ElementTypeIndex;

        result = MetaCSemantic_GetTypeAlignment(self,
                                           elementTypeIndex);
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
    const __m128i key = _mm_set1_epi16(hash16);
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

static inline uint32_t Align(uint32_t size, uint32_t alignment)
{
    assert(alignment >= 1);
    uint32_t alignmentMinusOne = (alignment - 1);
    uint32_t alignSize = (size + alignmentMinusOne);
    uint32_t alignMask = ~(alignmentMinusOne);
    return (alignSize & alignMask);
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
    uint32_t maxAlignment = MetaCSemantic_GetTypeAlignment(self, semaAgg->Fields->Type);

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

metac_type_index_t MetaCSemantic_GetPtrTypeOf(metac_semantic_state_t* state,
                                              metac_type_index_t elementTypeIndex)
{
    uint32_t hash = elementTypeIndex.v;
    metac_type_ptr_slot_t key = {hash, elementTypeIndex};

    metac_type_ptr_t ptrType;

    metac_type_index_t result =
        MetaCTypeTable_GetOrAddPtrType(&state->PtrTypeTable, &key, &ptrType);

    return result;
}


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
    else if (type->DeclKind == decl_type_typedef)
    {
        decl_type_typedef_t* typedef_ = (decl_type_typedef_t*) type;
        metac_type_index_t elementTypeIndex =
            MetaCSemantic_doTypeSemantic(self, typedef_->Type);

        uint32_t hash = elementTypeIndex.v;

        metac_type_typedef_slot_t key = {hash, 0, elementTypeIndex};
        metac_type_typedef_t typeEntry = {type->TypeHeader, elementTypeIndex};

        result =
            MetaCTypeTable_GetOrAddTypedefType(&self->TypedefTypeTable, &key, &type);
        sema_decl_type_t* semaTypedef = AllocNewSemaType(result);

        scope_insert_error_t scopeInsertError =
            MetaCSemantic_RegisterInScope(self, typedef_->Identifier, (metac_node_t)semaTypedef);

    }
    else if (IsAggregateType(type->DeclKind))
    {
        decl_type_struct_t* agg = (decl_type_struct_t*) type;
        if (type->DeclKind == decl_type_struct)
            typeKind = type_struct;
        else if (type->DeclKind == decl_type_union)
            typeKind = type_union;
        else
            assert(0);
        metac_type_aggregate_t* semaAgg = AllocNewAggregate(typeKind, agg);
        semaAgg->FieldCount = agg->FieldCount;

        metac_type_aggregate_field_t* semaFields =
            AllocAggregateFields(semaAgg, typeKind, agg->FieldCount);
        semaAgg->Fields = semaFields;

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

        semaAgg->Scope = MetaCSemantic_PushScope(self, scopeKind, METAC_NODE(semaAgg));

        switch(typeKind)
        {
            case type_struct:
            {
                MetaCSemantic_ComputeStructLayoutPopulateScope(self, agg, semaAgg);
                metac_type_aggregate_field_t* fields = semaAgg->Fields;

                /*
                 *     metac_identifier_ptr_t Identifier;

    metac_scope_t* Scope;

    metac_type_aggregate_field_t* Fields;

    uint32_t FieldCount;

    uint32_t Size;

    uint32_t Alignment;
       */
                uint32_t hash = AggregateHash(semaAgg);

                metac_type_aggregate_slot_t structKey = {
                    hash, semaAgg->FieldCount, semaAgg->Fields
                };
                result =
                    MetaCTypeTable_GetOrAddStructType(&self->StructTypeTable, &structKey, semaAgg);
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

        MetaCSemantic_PopScope(self);
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
        metac_type_functiontype_t funcType = { type->TypeHeader, returnType, parameterTypes, nParams };
        metac_type_functiontype_slot_t key =
            { hash, 0, returnType, parameterTypes, nParams };

        result = MetaCTypeTable_GetOrAddFunctionType(&self->FunctionTypeTable, &key, &funcType);
    }
    else if (type->DeclKind == decl_type && type->TypeKind == type_identifier)
    {
        printf("MetaCNodeKind_toChars: %s\n", MetaCNodeKind_toChars(type->DeclKind));
        printf("TypeIdentifier: %s\n",
            IdentifierPtrToCharPtr(self->ParserIdentifierTable, type->TypeIdentifier));
        metac_node_t node =
            MetaCSemantic_LookupIdentifier(self, type->TypeIdentifier);
        {
            printf("Lookup: %s\n", ((node != emptyNode) ?
                                       MetaCNodeKind_toChars(node->Kind) :
                                       "empty"));
        }
        if (node->Kind == node_decl_type_typedef)
        {
            sema_decl_type_t* typedef_ = (sema_decl_type_t*)node;
            result = typedef_->typeIndex;
        }
        /*
        if (node == emptyNode)
        {
            node =
                MetaCSemantic_Resolve(self, type);
        }*/
    }
    else
    {

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

    sema_decl_function_t* f =
        AllocNewSemaFunction(func);
    // let's first do the parameters
    sema_decl_variable_t* params = f->Parameters =
        AllocFunctionParameters(f, func->ParameterCount);

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

    metac_scope_parent_t Parent = {SCOPE_PARENT_V(scope_parent_function, FunctionIndex(f))};

    f->Scope = MetaCSemantic_PushScope(self, scope_parent_function, (metac_node_t)f);
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

metac_node_t NodeFromTypeIndex(metac_type_index_t typeIndex)
{
    const uint32_t index = TYPE_INDEX_INDEX(typeIndex);
    switch(TYPE_INDEX_KIND(typeIndex))
    {
        case type_index_struct:
            return (metac_node_t)StructPtr(index);
        case type_index_union:
            return (metac_node_t)UnionIndex(index);
    }
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
            sema_decl_variable_t* var = AllocNewSemaVariable(v, &result);
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
                    NodeFromTypeIndex(type_index);
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

metac_type_index_t MetaCSemantic_GetArrayTypeOf(metac_semantic_state_t* state,
                                                metac_type_index_t elementTypeIndex,
                                                uint32_t dimension)
{
    uint32_t hash = EntangleInts(TYPE_INDEX_INDEX(elementTypeIndex), dimension);
    metac_type_array_slot_t key = {hash, elementTypeIndex, dimension};

    metac_type_index_t result =
        MetaCTypeTable_GetOrAddArrayType(&state->ArrayTypeTable, hash, &key);

    return result;
}


#include "metac_printer.h"
static inline const char* BasicTypeToChars(metac_type_index_t typeIndex)
{
    assert(TYPE_INDEX_KIND(typeIndex) == type_index_basic);
    switch((metac_type_kind_t) TYPE_INDEX_INDEX(typeIndex))
    {
        case type_invalid :
            assert(0);

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
    uint32_t searchResult = _mm_movemask_epi16(cmp);
    if (searchResult == 0)
        goto LtableLookup;
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
            metac_type_array_slot_t* arrayType =
                (self->ArrayTypeTable.Slots + TYPE_INDEX_INDEX(typeIndex));
            TypeToCharsP(self, printer, arrayType->ElementTypeIndex);
            MetacPrinter_PrintStringLiteral(printer, "[");
            MetacPrinter_PrintI64(printer, (int64_t)arrayType->Dimension);
            MetacPrinter_PrintStringLiteral(printer, "]");
        } break;
        case type_index_basic:
        {
            const char* typeString = BasicTypeToChars(typeIndex);
            MetacPrinter_PrintStringLiteral(printer, typeString);
        } break;
        case type_index_ptr:
        {
            metac_type_ptr_slot_t* ptrType =
                (self->PtrTypeTable.Slots + TYPE_INDEX_INDEX(typeIndex));
            TypeToCharsP(self, printer, ptrType->ElementTypeIndex);
            MetacPrinter_PrintStringLiteral(printer, "*");
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

const char* MetaCExpressionKind_toChars(metac_expression_kind_t);
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
metac_sema_expression_t* MetaCSemantic_doExprSemantic_(metac_semantic_state_t* self,
                                                       metac_expression_t* expr,
                                                       const char* callFun,
                                                       uint32_t callLine)
{
    metac_sema_expression_t* result = 0;

    result = AllocNewSemaExpression(expr);

    if (IsBinaryExp(expr->Kind))
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
                LENGTH_FROM_STRING_KEY(expr->StringKey));
        break;
        case exp_signed_integer :
            result->TypeIndex = MetaCSemantic_GetTypeIndex(self, type_int, (decl_type_t*)emptyPointer);
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
            printf("call: %s\n", MetaCPrinter_PrintExpression(&self->Printer, call));

            for(int memberIdx = 0;
                memberIdx < self->CompilerInterface->FieldCount;
                memberIdx)
            {
                printf("Field: %s\n",
                    IdentifierPtrToCharPtr(self->ParserIdentifierTable,
                        self->CompilerInterface->Fields->Identifier));
            }
            // CompilerInterface_Call(
        } break;
        case exp_type:
        {
            result->TypeIndex = MetaCSemantic_doTypeSemantic(self, expr->TypeExp);
        } break;
        case exp_sizeof:
        {
            uint32_t size = -1;
            metac_sema_expression_t* e1 =
                MetaCSemantic_doExprSemantic(self, expr->E1);
            if (e1->TypeIndex.v != 0 && e1->TypeIndex.v != -1)
                size = MetaCSemantic_GetTypeSize(self, e1->TypeIndex);

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
                    metac_sema_declaration_t* sd =
                        MetaCSemantic_doDeclSemantic(self, (metac_declaration_t*)node);
                    int k = 12;
                    // an exp_variable might resolve to something
                    // diffrent than a variable so check that it is one now
                    if(sd->DeclKind == decl_variable)
                    {
                        sema_decl_variable_t* variable =
                            (sema_decl_variable_t*) sd;
                        result->Kind = exp_variable;
                        result->TypeIndex = variable->TypeIndex;
                        result->Variable = variable;
                    }
                }
            }
            //assert(0);
            //
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
                SemanticError(self, "cannot take the address of %s", MetaCPrinter_PrintExpression(&self->Printer, expr->E1));
            }
            else
            {
                result->TypeIndex = MetaCSemantic_GetPtrTypeOf(self, result->E1->TypeIndex);
            }
        break;
    }

    return result;
}
