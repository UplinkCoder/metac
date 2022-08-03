
typedef struct handoff_walker_context_t
{
    uint32_t FunctionKey;

    const metac_semantic_state_t* Origin;
    metac_semantic_state_t* NewOwner;
    metac_sema_declaration_t* decl;
    metac_node_t result;
} handoff_walker_context_t;

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
