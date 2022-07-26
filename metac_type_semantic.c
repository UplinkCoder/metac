#include "metac_type_semantic.h"
#include "metac_task.h"

#ifndef INVALID_SIZE
#  define INVALID_SIZE \
      ((uint32_t)-1)
#endif

#ifndef U32
#  define U32(VAR) \
      (*(uint32_t*)(&VAR))
#endif


metac_type_index_t MetaCSemantic_GetPtrTypeOf(metac_semantic_state_t* self,
                                              metac_type_index_t elementTypeIndex)
{
    uint32_t hash = elementTypeIndex.v;
    metac_type_ptr_t key =
            {{decl_type_typedef, 0, hash, 0},
             elementTypeIndex};

    metac_type_index_t result =
        MetaCTypeTable_GetOrEmptyPtrType(&self->PtrTypeTable, &key);
    if (result.v == 0)
    {
        result = MetaCTypeTable_AddPtrType(&self->PtrTypeTable, &key);
    }
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

static inline bool IsAggregateTypeDecl(metac_declaration_kind_t declKind)
{
    if (declKind == decl_type_struct || declKind == decl_type_union)
    {
        return true;
    }
    return false;
}

static inline bool IsPointerType(metac_declaration_kind_t declKind)
{
    if (declKind == decl_type_ptr)
    {
        return true;
    }
    return false;
}


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

static inline metac_type_index_t CommonBasicType(const metac_type_index_t a,
                                                 const metac_type_index_t b)
{
    metac_type_index_t result;
    result.v = TYPE_INDEX_V(type_index_basic, basic_int);

    if (a.v == TYPE_INDEX_V(type_index_basic, basic_long_double) ||
        b.v == TYPE_INDEX_V(type_index_basic, basic_long_double))
    {
        result.v = TYPE_INDEX_V(type_index_basic, basic_long_double);
    } else if (a.v == TYPE_INDEX_V(type_index_basic, basic_double) ||
               b.v == TYPE_INDEX_V(type_index_basic, basic_double))
    {
        result.v = TYPE_INDEX_V(type_index_basic, basic_double);
    } else if (a.v == TYPE_INDEX_V(type_index_basic, basic_float) ||
               b.v == TYPE_INDEX_V(type_index_basic, basic_float))
    {
        result.v = TYPE_INDEX_V(type_index_basic, basic_float);
    } else if (a.v == TYPE_INDEX_V(type_index_basic, type_unsigned_long_long) ||
               b.v == TYPE_INDEX_V(type_index_basic, type_unsigned_long_long))
    {
        result.v = TYPE_INDEX_V(type_index_basic, type_unsigned_long_long);
    } else if (a.v == TYPE_INDEX_V(type_index_basic, type_long_long) ||
               b.v == TYPE_INDEX_V(type_index_basic, type_long_long))
    {
        result.v = TYPE_INDEX_V(type_index_basic, type_long_long);
    } else if (a.v == TYPE_INDEX_V(type_index_basic, type_unsigned_long) ||
               b.v == TYPE_INDEX_V(type_index_basic, type_unsigned_long))
    {
        result.v = TYPE_INDEX_V(type_index_basic, type_unsigned_long);
    } else if (a.v == TYPE_INDEX_V(type_index_basic, type_long) ||
               b.v == TYPE_INDEX_V(type_index_basic, type_long))
    {
        result.v = TYPE_INDEX_V(type_index_basic, type_long);
    } else if (a.v == TYPE_INDEX_V(type_index_basic, type_unsigned_int) ||
               b.v == TYPE_INDEX_V(type_index_basic, type_unsigned_int))
    {
        result.v = TYPE_INDEX_V(type_index_basic, type_unsigned_int);
    }

    return result;
}



///TODO FIXME
/// this is not nearly complete!
metac_type_index_t MetaCSemantic_CommonSubtype(metac_semantic_state_t* self,
                                               const metac_type_index_t a,
                                               const metac_type_index_t b)
{
        metac_type_index_t result = {-1};
    if (a.v == b.v)
        result = a;
    else
    {
        if (TYPE_INDEX_KIND(a) == TYPE_INDEX_KIND(b))
        {
            switch(TYPE_INDEX_KIND(a))
            {
                case type_index_basic:
                    result = CommonBasicType(a, b);
            }
        }
    }
        return result;
}

uint32_t FieldHash(metac_type_aggregate_field_t* field)
{
#if NDEBUG
    if (field->Header.Hash)
        return field->Header.Hash;
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

metac_type_aggregate_t* MetaCSemantic_PersistTemporaryAggregate(metac_semantic_state_t* self,
                                                                metac_type_aggregate_t* tmpAgg)
{
    // memcpy(semaAgg, tmpAgg, sizeof(metac_type_aggregate_t));

    metac_type_index_t typeIndex =
        MetaCTypeTable_AddStructType(&self->StructTypeTable, tmpAgg);
    metac_type_aggregate_t* semaAgg = StructPtr(self, TYPE_INDEX_INDEX(typeIndex));
    metac_type_aggregate_field_t* semaFields = cast(metac_type_aggregate_field_t*)
            calloc(sizeof(metac_type_aggregate_field_t), tmpAgg->FieldCount);
        //AllocAggregateFields(self, semaAgg, tmpAgg->Header.Kind, tmpAgg->FieldCount);

    //TODO FIXME this should use a deep walk through the fields
    //MetaCDeclaration_Walk(tmpAgg,)
    memcpy(semaFields, tmpAgg->Fields,
        sizeof(metac_type_aggregate_field_t) * tmpAgg->FieldCount);
    semaAgg->Fields = semaFields;

    return semaAgg;
}

metac_type_index_t MetaCSemantic_TypeSemantic(metac_semantic_state_t* self,
                                              decl_type_t* type)
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
            MetaCSemantic_doExprSemantic(self, arrayType->Dim, 0);
        if (dim->Kind != exp_signed_integer)
        {
            printf("Array dimension should eval to integer but it is: %s\n",
                MetaCExpressionKind_toChars(dim->Kind));
        }
        result =
            MetaCSemantic_GetArrayTypeOf(self,
                                        elementType,
                                        (uint32_t)dim->ValueU64);
    }
    else if (type->DeclKind == decl_type_typedef)
    {
        decl_type_typedef_t* typedef_ = (decl_type_typedef_t*) type;
        metac_type_index_t elementTypeIndex =
            MetaCSemantic_doTypeSemantic(self, typedef_->Type);

        uint32_t hash = elementTypeIndex.v;

        metac_type_typedef_t key = {
            {decl_type_typedef, 0, hash},
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


       metac_type_aggregate_field_t* tmpFields =
            cast(metac_type_aggregate_field_t* )
                                malloc(sizeof(metac_type_aggregate_field_t)
                                * agg->FieldCount);
        // ideally it wouldn't matter if this memset was here or not
        memset(tmpFields, 0x0, sizeof(*tmpFields) * agg->FieldCount);

        metac_type_aggregate_t tmpSemaAggMem = {(metac_declaration_kind_t)0};
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
                    semaAgg =
                        MetaCSemantic_PersistTemporaryAggregate(self, tmpSemaAgg);

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
        free(tmpFields);
        MetaCSemantic_PopTemporaryScope(self);
    }
    else if (IsPointerType(type->DeclKind))
    {
        metac_type_index_t elementTypeIndex = {0};
        decl_type_ptr_t* typePtr = (decl_type_ptr_t*) type;
        elementTypeIndex =
            MetaCSemantic_doTypeSemantic(self, typePtr->ElementType);
        assert(elementTypeIndex.v && elementTypeIndex.v != -1);
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
        metac_type_index_t* parameterTypes = cast(metac_type_index_t*)
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
            {decl_type_functiontype, hash, 0, },
            returnType, parameterTypes, nParams
        };

        MetaCSemantic_PopTemporaryScope(self);

        result = MetaCTypeTable_GetOrEmptyFunctionType(&self->FunctionTypeTable, &key);
        if (result.v == 0)
        {
            result =
                MetaCTypeTable_AddFunctionType(&self->FunctionTypeTable, &key);
        }
    }
    else if (type->DeclKind == decl_type && type->TypeKind == type_identifier)
    {
        //printf("MetaCNodeKind_toChars: %s\n", MetaCNodeKind_toChars((metac_node_kind_t)type->DeclKind));
        //printf("TypeIdentifier: %s\n", IdentifierPtrToCharPtr(self->ParserIdentifierTable, type->TypeIdentifier));
LtryAgian: {}
        metac_node_t node =
            MetaCSemantic_LookupIdentifier(self, type->TypeIdentifier);
        {
            printf("Lookup: %s\n", ((node != emptyNode) ?
                                       MetaCNodeKind_toChars(node->Kind) :
                                       "empty"));
            if (node == emptyNode)
            {
#ifndef NO_FIBERS
#if 0
                WaitForSignal(MetaCSemantic_RegisterInScope, type->TypeIdentifier);
                CancelIf(self->CurrentScope->Closed);
#endif
                aco_t* me = (aco_t*)CurrentFiber();
                task_t* task = CurrentTask();
                printf("Yield!\n");
                metac_semantic_waiter_t* meWaiter = &self->Waiters.Waiters[INC(self->Waiters.WaiterCount)];
                meWaiter->FuncHash = CRC32C_S("MetaCSemantic_LookupIdentifier");
                meWaiter->NodeHash = CRC32C_VALUE(~0, type->TypeIdentifier);
                meWaiter->Continuation = me;
                task->TaskFlags |= Task_Waiting;
                YIELD(WaitOnResolve);
                printf("Trying agian after yielding\n");
                goto LtryAgian;
#else
                printf("No fiber support ... cannot deal with deferred lookup\n");
#endif
            }
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
#ifndef NO_FIBERS
    const uint32_t funcHash = crc32c(~0, "MetaCSemantic_doTypeSemantic",
                                  sizeof("MetaCSemantic_doTypeSemantic") -1);

    uint32_t nodeHash = type->Hash;
    // check if someone waited for the node we just resolved
    for(uint32_t waiterIdx = 0;
        waiterIdx < self->Waiters.WaiterCount;
        waiterIdx++)
    {
        metac_semantic_waiter_t waiter = self->Waiters.Waiters[waiterIdx];
        if (waiter.FuncHash == funcHash
         && waiter.NodeHash == nodeHash)
        {
            printf("Found someone waiting for me\n");
            RESUME(waiter.Continuation);
        }
    }
#endif
    assert(result.v != 0);
    return result;
}

#ifndef NO_FIBERS
void MetaCSemantic_doTypeSemantic_Task(task_t* task)
{
    MetaCSemantic_doTypeSemantic_Fiber_t* ctx =
        cast(MetaCSemantic_doTypeSemantic_Fiber_t*) task->Context;
    metac_semantic_state_t* sema = ctx->Sema;
    decl_type_t* type = ctx->Type;

    ctx->Result = MetaCSemantic_TypeSemantic(sema, type);

}


void MetaCSemantic_doTypeSemantic_Fiber(void* caller, void* arg)
{
    MetaCSemantic_doTypeSemantic_Fiber_t* ctx =
        cast(MetaCSemantic_doTypeSemantic_Fiber_t*) arg;
    metac_semantic_state_t* sema = ctx->Sema;
    decl_type_t* type = ctx->Type;

    ctx->Result = MetaCSemantic_TypeSemantic(sema, type);

}
#endif

metac_type_index_t MetaCSemantic_doTypeSemantic_(metac_semantic_state_t* self,
                                                 decl_type_t* type,
                                                 const char* callFile,
                                                 uint32_t callLine)
{
    metac_type_index_t result = {0};




//   result =
//        DO_TASK(MetaCSemantic_TypeSemantic, self, type);

    result = MetaCSemantic_TypeSemantic(self, type);
    if (!result.v)
    {
#ifndef NO_FIBERS
        worker_context_t currentContext = *CurrentWorker();

        MetaCSemantic_doTypeSemantic_Fiber_t arg = {
                self, type
        };
        MetaCSemantic_doTypeSemantic_Fiber_t* argPtr = &arg;

        CALL_TASK_FN(MetaCSemantic_doTypeSemantic, argPtr);
        task_t typeSemTask = {0};

        typeSemTask.Context = argPtr;
        typeSemTask.ContextSize = sizeof(arg);
        typeSemTask.TaskFunction = MetaCSemantic_doTypeSemantic_Task;

        while (!TaskQueue_Push(&currentContext.Queue, &typeSemTask))
        {
            // yielding because queue is full;
            YIELD(QueueFull);
        }

        uint32_t funcHash = CRC32C_S("MetaCSemantic_doTypeSemantic");
        uint32_t nodeHash = type->TypeHeader.Hash;

        for(uint32_t waiterIdx = 0;
            waiterIdx < self->Waiters.WaiterCount;
            waiterIdx++)
        {
            metac_semantic_waiter_t waiter = self->Waiters.Waiters[waiterIdx];
            if (funcHash == waiter.FuncHash && nodeHash == waiter.NodeHash)
            {
                aco_yield2(waiter.Continuation);
            }
        }

    result = arg.Result;
#else
    printf("Without fiber no deferral\n");
#endif
    }
    return result;
}

metac_type_index_t MetaCSemantic_GetArrayTypeOf(metac_semantic_state_t* state,
                                                metac_type_index_t elementTypeIndex,
                                                uint32_t dimension)
{
    uint32_t hash = EntangleInts(TYPE_INDEX_INDEX(elementTypeIndex), dimension);
    metac_type_array_t key = {
                {decl_type_array, 0, hash}, elementTypeIndex, dimension};

    metac_type_index_t result =
        MetaCTypeTable_GetOrEmptyArrayType(&state->ArrayTypeTable, &key);

    if (result.v == 0)
    {
        result =
            MetaCTypeTable_AddArrayType(&state->ArrayTypeTable, &key);
    }

    return result;
}

bool ComputeStructLayout(metac_semantic_state_t* self, decl_type_t* type)
{
    return false;
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
#ifndef NO_FIBERS
        if (!semaField->Type.v)
        {
            printf("FieldType couldn't be resolved\n yielding fiber\n");
            aco_t* me = (aco_t*)CurrentFiber();
            if (me != 0)
            {
                metac_semantic_waiter_t waiter;
                waiter.FuncHash = CRC32C_S("MetaCSemantic_doTypeSemantic");
                waiter.NodeHash = declField->Field->VarType->TypeHeader.Hash;
                waiter.Continuation = me;

                assert(self->Waiters.WaiterCount < self->Waiters.WaiterCapacity);
                self->Waiters.Waiters[self->Waiters.WaiterCount++] = waiter;
                printf("We should Yield\n");
                (cast(task_t*)(me->arg))->TaskFlags |= Task_Waiting;
                YIELD(watingOnTypeSemantic);
                printf("Now we should be able to resolve\n");
                semaField->Type =
                    MetaCSemantic_doTypeSemantic(self, declField->Field->VarType);
            }
            // YIELD_ON(declField->Field->VarType, MetaCSemantic_doTypeSemantic);
        }
#endif
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
    semaAgg->Size = currentFieldOffset;
    semaAgg->Alignment = maxAlignment;

    fprintf(stderr, "sizeof(struct) = %u\n", semaAgg->Size);//DEBUG
    fprintf(stderr, "Alignof(struct) = %u\n", semaAgg->Alignment);//DEBUG

    return result;
}

