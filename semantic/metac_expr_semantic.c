#include "metac_semantic.h"
#include "../codegen/metac_codegen.h"

#ifndef NO_FIBERS
#  include "../os/metac_task.h"
#endif

#ifndef _emptyPointer
#  define _emptyPointer (void*)0x1
#  define emptyNode (metac_node_t) _emptyPointer
#endif

#ifndef NO_FIBERS
void MetaCSemantic_doExprSemantic_Task(task_t* task)
{
    MetaCSemantic_doExprSemantic_task_context_t* ctx
        = (MetaCSemantic_doExprSemantic_task_context_t*) task->Context;

    ctx->Result = MetaCSemantic_doExprSemantic_(ctx->Sema, ctx->Expr, ctx->Result,
                                                task->Origin.Func, task->Origin.Line);
}
#endif

#define report_error(FMT) \
    fprintf(stderr, FMT);

static bool IsAggregateType(metac_type_index_kind_t typeKind)
{
    // TODO this could be a constant

    switch(typeKind)
    {
        case type_index_struct:
        case type_index_union:
        case type_index_class:
            return true;
    }

    return false;
}

void ConvertTupleElementToExp(metac_semantic_state_t* sema,
                              metac_sema_expression_t** dstP, metac_type_index_t elemType,
                              uint32_t offset, BCHeap* heap)
{
    metac_expression_t exp = {exp_invalid};
    *dstP = AllocNewSemaExpression(sema, &exp);
    metac_sema_expression_t* dst = *dstP;

    if (elemType.v == TYPE_INDEX_V(type_index_basic, type_int))
    {
        int32_t value = *cast(int32_t*)(heap->heapData + offset);
        dst->Kind = exp_signed_integer;
        dst->TypeIndex = elemType;
        dst->ValueI64 = value;
    }
    else if (elemType.v == TYPE_INDEX_V(type_index_basic, type_type))
    {
        metac_type_index_t value = *cast(metac_type_index_t*)(heap->heapData + offset);
        dst->Kind = exp_type;
        dst->TypeIndex = elemType;
        dst->TypeExp = value;
    }
}

metac_sema_expression_t
EvaluateExpression(metac_semantic_state_t* sema,
                   metac_sema_expression_t* e,
                   BCHeap* heap)
{
    metac_alloc_t interpAlloc;
    Allocator_Init(&interpAlloc, &sema->TempAlloc, 0);

    metac_bytecode_ctx_t ctx;
    MetaCCodegen_Init(&ctx, 0);
    ctx.Sema = sema;

    for(uint32_t i = 0; i < sema->GlobalsCount; i++)
    {
        MetaCCodegen_doGlobal(&ctx, sema->Globals[i], i);
    }

    MetaCCodegen_Begin(&ctx, sema->ParserIdentifierTable, sema);

    metac_bytecode_function_t fCode =
        MetaCCodegen_GenerateFunctionFromExp(&ctx, e);

    MetaCCodegen_End(&ctx);

    uint32_t resultInt =
        MetaCCodegen_RunFunction(&ctx, fCode, &interpAlloc, heap, "", 0);

    MetaCCodegen_Free(&ctx);

    metac_sema_expression_t result = {0};
    // BCGen_printFunction(c);

    if (e->TypeIndex.v == TYPE_INDEX_V(type_index_basic, type_type))
    {
        result.Kind = exp_type;
        result.TypeIndex.v = TYPE_INDEX_V(type_index_basic, type_type);
        result.TypeExp.v = resultInt;
    }
    else if (TYPE_INDEX_KIND(e->TypeIndex) == type_index_tuple)
    {
        metac_type_tuple_t* typeTuple =
            TupleTypePtr(ctx.Sema, TYPE_INDEX_INDEX(e->TypeIndex));

        const uint32_t count = typeTuple->TypeCount;

        uint32_t currrentHeapOffset = resultInt;

        STACK_ARENA_ARRAY(metac_sema_expression_t*, tupleExps, 32, &sema->TempAlloc)

        ARENA_ARRAY_ENSURE_SIZE(tupleExps, count);

        result.Kind = exp_tuple;
        result.TypeIndex = e->TypeIndex;

        for(uint32_t i = 0; i < count; i++)
        {
            BCType bcType = MetaCCodegen_GetBCType(&ctx, typeTuple->TypeIndicies[i]);
            ConvertTupleElementToExp(sema, tupleExps + i, typeTuple->TypeIndicies[i],
                                     currrentHeapOffset, heap);
            currrentHeapOffset += MetaCCodegen_GetStorageSize(&ctx, bcType);
        }

        STACK_ARENA_ARRAY_TO_HEAP(tupleExps, &sema->TempAlloc);

        result.TupleExpressions = tupleExps;
        result.TupleExpressionCount = count;
    }
    else if (TYPE_INDEX_KIND(e->TypeIndex) == type_index_basic &&
             TYPE_INDEX_INDEX(e->TypeIndex) == (uint32_t) type_float)
    {
        result.Kind = exp_float;
        result.ValueF23 = *(float*) &resultInt;
        result.TypeIndex.v = TYPE_INDEX_V(type_index_basic, type_float);
    }
    else
    {
        result.Kind = exp_signed_integer;
        result.TypeIndex.v = e->TypeIndex.v;
        result.ValueI64 = resultInt;
    }
    result.LocationIdx = e->LocationIdx;

    Debug_RemoveAllocator(g_DebugServer, &interpAlloc);
    // Allocator_Release(&interpAlloc);

    return result;
}

void
MetaCSemantic_ConstantFold(metac_semantic_state_t* self, metac_sema_expression_t* exp)
{
    bool couldFold = false;

    (*exp) = EvaluateExpression(self, exp, 0);
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
            result = indexed->TupleExpressions[idx];
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
    else if (indexed->Kind == exp_variable)
    {
        assert(TYPE_INDEX_KIND(indexed->TypeIndex) == type_index_array
            || TYPE_INDEX_KIND(indexed->TypeIndex) == type_index_ptr);

        result = AllocNewSemaExpression(self, expr);
        result->TypeIndex = MetaCSemantic_GetElementType(self, indexed->TypeIndex);
        result->E1 = indexed;
        result->E2 = index;
    }

    return  result;
}

metac_sema_expression_t* doCmpExpSemantic(metac_semantic_state_t* self,
                                          metac_location_t loc,
                                          metac_expression_kind_t kind,
                                          metac_sema_expression_t* e1,
                                          metac_sema_expression_t* e2,
                                          metac_sema_expression_t* result)
{
    const metac_type_index_t tbool
        = MetaCSemantic_GetTypeIndex(self, type_bool, (decl_type_t*)emptyPointer);


    switch(kind)
    {
        default: {
            // InternalError("%s is not a comparison expression", MetaCExpressionKind_toChars(cmpExp->Kind));
        };

        case exp_lt:
        case exp_le:
        case exp_gt:
        case exp_ge:
        case exp_neq:
        case exp_eq: {
            if (TypeIsInteger(e1->TypeIndex) && TypeIsInteger(e2->TypeIndex))
                result->TypeIndex = tbool;
        } break;
    }

    return result;
}
/// this function doesn't really resolve anything as of now
/// as there is no function overloading. However in case I add function overloading
/// which I most certainly will; I want to have an entry point.
static inline
metac_sema_expression_t* ResloveFuncCall(metac_semantic_state_t* self,
                                         metac_expression_t* fn,
                                         metac_sema_expression_t** arguments,
                                         uint32_t nArgs)
{
    metac_expression_kind_t kind = fn->Kind;
    metac_sema_expression_t* result = MetaCSemantic_doExprSemantic(self, fn, 0);
    return result;
}

static inline
void MetaCSemantic_doCallSemantic(metac_semantic_state_t* self,
                                  metac_expression_t* call,
                                  metac_sema_expression_t** resultP)
{
    metac_sema_expression_t* result;
    metac_expression_t* fn = call->E1;

    exp_argument_t* argList = (METAC_NODE(call->E2) != emptyNode ?
        (exp_argument_t*)call->E2 : (exp_argument_t*)emptyNode);
    STACK_ARENA_ARRAY(metac_sema_expression_t*, arguments, 64, &self->TempAlloc);

    uint32_t nArgs = 0;
    while(METAC_NODE(argList) != emptyNode)
    {
        nArgs++;
        ARENA_ARRAY_ADD(arguments,
            MetaCSemantic_doExprSemantic(self, argList->Expression, 0));
        argList = argList->Next;
    }



    metac_sema_expression_t* func =
        ResloveFuncCall(self, fn, arguments, nArgs);
    if (func)
    {
        result = AllocNewSemaExpression(self, call);
        (*resultP) = result;
    }
    else
    {
        assert(!"Can't resolve function call\n");
    }
    result->Call.Function = func;

    if (func->Function)

    printf("function call with: %u arguments\n", nArgs);

    STACK_ARENA_ARRAY_TO_HEAP(arguments, &self->Allocator);

    metac_expression_t dummy;
    dummy.LocationIdx = call->LocationIdx;
    dummy.Kind = exp_variable;

    metac_type_functiontype_t* funcType = FunctiontypePtr(self, TYPE_INDEX_INDEX(func->TypeIndex));

    if (nArgs == 0)
    {
        METAC_NODE(result->Call.Arguments) = emptyNode;
    }
    else
    {
        result->Call.Arguments = arguments;
    }
    result->Call.ArgumentCount = nArgs;

    result->TypeIndex = funcType->ReturnType;

    //STACK_ARENA_ARRAY_FREE(arguments);
}

void ResolveIdentifierToExp(metac_semantic_state_t* self,
                            metac_node_t node,
                            metac_sema_expression_t** resultP,
                            uint32_t* hashP)
{
    uint32_t hash = *hashP;
    metac_sema_expression_t* result = *resultP;
    // printf("Resolving node: %p\n", node);

    if (node->Kind == (metac_node_kind_t)exp_identifier)
    {
        fprintf(stderr, "we should not be retured an identifier\n");
    }
    else if (node->Kind == node_decl_variable ||
             node->Kind == node_decl_parameter)
    {
        sema_decl_variable_t* v = cast(sema_decl_variable_t*)node;
        result->Kind = exp_variable;
        result->Variable = v;
        result->TypeIndex = v->TypeIndex;
        hash = v->Hash;
    }
    else if (node->Kind == node_decl_field)
    {
        metac_type_aggregate_field_t* field =
            cast(metac_type_aggregate_field_t*) node;
        result->Kind = exp_field;
        result->Field = field;
        result->TypeIndex = field->Type;
        hash = field->Header.Hash;
    }
    else if (node->Kind == node_decl_enum_member)
    {
        metac_enum_member_t* enumMember = cast(metac_enum_member_t*)node;
        (*resultP) = enumMember->Value;
        printf("node->Kind == node_decl_enum_member\n");
    }
    else if (node->Kind == node_decl_type_struct)
    {
        result->Kind = exp_type;
        result->TypeExp.v =
            TYPE_INDEX_V(type_index_struct, StructIndex(self, cast(metac_type_aggregate_t*)node));
        result->TypeIndex.v = TYPE_INDEX_V(type_index_basic, type_type);
    }
    else if (node->Kind == node_decl_type_typedef)
    {
        result->Kind = exp_type;
        result->TypeExp.v =
            TYPE_INDEX_V(type_index_typedef, TypedefIndex(self, cast(metac_type_typedef_t*)node));
    }
    else if (node->Kind == node_decl_function)
    {
        sema_decl_function_t* func = cast(sema_decl_function_t*) node;
        result->Kind = exp_function;
        result->Function = func;
        result->TypeIndex = func->TypeIndex;
    }
    else if (node->Kind == node_exp_unknown_value)
    {
        (*result) = *cast(metac_sema_expression_t*) node;
        hash = result->Hash;
    }
    else
    {
        printf("[%s:%u] NodeType unexpected: %s\n", __FILE__, __LINE__, MetaCNodeKind_toChars(node->Kind));
    }

    (*hashP) = hash;
}

metac_sema_expression_t* UnwrapParen(metac_sema_expression_t* e)
{
    while(e->Kind == exp_paren)
    {
        e = e->E1;
    }

    return e;
}

metac_sema_expression_t* ExtractCastExp(metac_sema_expression_t* e)
{
    while(e->Kind == exp_cast)
    {
        e = e->CastExp;
    }

    return e;
}

void MetaCSemantic_doAssignSemantic(metac_semantic_state_t* self,
                                    metac_expression_t* expr,
                                    metac_sema_expression_t* result)
{
    assert(expr->Kind == exp_assign);
    assert(result->Kind == exp_assign);

    if (result->E1->TypeIndex.v != result->E2->TypeIndex.v)
    {

    }
}

metac_identifier_ptr_t GetIdentifierPtr(metac_expression_t* expr)
{
    metac_identifier_ptr_t result = {0};

    switch(expr->Kind)
    {
        case exp_type:
        {
            if (expr->TypeExp->TypeKind == type_identifier)
            {
                result.v = expr->TypeExp->TypeIdentifier.v;
            }
        } break;
        case exp_identifier:
        {
            result.v = expr->IdentifierPtr.v;
        } break;

        default: assert(0);
    }

    return result;
}

void MetaCSemantic_PushResultType(metac_semantic_state_t* self, metac_type_index_t type_)
{

}

metac_sema_expression_t* MetaCSemantic_doExprSemantic_(metac_semantic_state_t* self,
                                                       metac_expression_t* expr,
                                                       metac_sema_expression_t* result,
                                                       const char* callFun,
                                                       uint32_t callLine)
{
    if (!result)
    {
        result = AllocNewSemaExpression(self, expr);
    }

    uint32_t hash = 0;

    if (IsBinaryExp(expr->Kind)
        && (   expr->Kind != exp_arrow
            && expr->Kind != exp_dot
            && expr->Kind != exp_index
        ) ||   expr->Kind == exp_ternary
    )
    {
        MetaCSemantic_PushExpr(self, result);

        if (expr->Kind == exp_ternary)
        {
            result->Econd = MetaCSemantic_doExprSemantic(self, expr->Econd, 0);
            hash = CRC32C_VALUE(hash, result->Econd->Hash);
        }

        result->E1 = MetaCSemantic_doExprSemantic(self, expr->E1, 0);
        result->E2 = MetaCSemantic_doExprSemantic(self, expr->E2, 0);

        hash = CRC32C_VALUE(hash, result->E1->Hash);
        hash = CRC32C_VALUE(hash, result->E2->Hash);

        MetaCSemantic_PopExpr(self, result);
    }

    switch(expr->Kind)
    {
        case exp_invalid:
        default:
        {
            fprintf(stderr, "expression semantic doesn't support %s yet.\n",
                    MetaCExpressionKind_toChars(expr->Kind));
            assert(0);
        }

        case exp_comma:
        {
            metac_sema_expression_t* r = result->E2;
            while(r->Kind == exp_comma)
            {
                r = r->E2;
            }
            result->TypeIndex = r->TypeIndex;
        } break;

        case exp_unary_dot:
        {
            metac_identifier_ptr_t idPtr = {0};
            uint32_t idKey = 0;
            const char* idString = 0;

            if (expr->E1->Kind == exp_type &&
                expr->E1->TypeExp->TypeKind == type_identifier)
            {
                metac_sema_expression_t holder;
                idPtr = expr->E1->TypeExp->TypeIdentifier;
                goto LswitchIdKey;
            }
            else if (result->E1->Kind == exp_identifier)
            {
                idPtr = (expr->E1->IdentifierPtr);
LswitchIdKey:
                idString = IdentifierPtrToCharPtr(self->ParserIdentifierTable, idPtr);
                int len = strlen(idString);
                idKey = IDENTIFIER_KEY(crc32c(~0, idString, len), len);

                switch(idKey)
                {
                    case compiler_key:
                    {
                        result->Kind = exp_variable;
                        result->Variable = &self->CompilerVariable;
                        // result->TypeIndex = MetaCSemantic_doTypeSemantic(self, result->Variable);
                        result->TypeIndex = result->Variable->TypeIndex;
                        hash = self->CompilerInterface->Header.Hash;
                    } break;
                }
            }
            else
            {
                assert(0);
            }
        } break;

        case exp_call:
        {
            MetaCSemantic_doCallSemantic(self, expr, &result);
        } break;

        case exp_deref:
        {
            result->E1 = MetaCSemantic_doExprSemantic(self, expr->E1, 0);
            metac_type_index_t e1type = result->E1->TypeIndex;
            if (TYPE_INDEX_KIND(e1type) == type_index_ptr)
            {
                metac_type_index_t elemType =
                    PtrTypePtr(self, TYPE_INDEX_INDEX(e1type))->ElementType;
                result->TypeIndex = elemType;
            }
            else
            {
                assert(!"lhs of * doesn't seem to be a pointer");
            }
        } break;

        case exp_arrow:
        {
            metac_type_index_t elementType = {0};
            metac_type_aggregate_t* e1AggType = 0;
            metac_type_ptr_t* ptrType = 0;
            metac_type_index_t e1type;

            result->E1 = MetaCSemantic_doExprSemantic(self, expr->E1, 0);
            e1type = result->E1->TypeIndex;

            if (TYPE_INDEX_KIND(e1type) == type_index_ptr)
            {
                elementType =
                    PtrTypePtr(self, TYPE_INDEX_INDEX(e1type))->ElementType;
                metac_sema_expression_t* e1 = UnwrapParen(result->E1);
                e1 = ExtractCastExp(e1);

                if (TYPE_INDEX_KIND(elementType) != type_index_struct)
                {
                    assert(!"not a struct");
                    // SemanticError(e1->LocationIndex)
                }

                {
                    e1AggType = StructPtr(self, TYPE_INDEX_INDEX(elementType));
                    MetaCSemantic_MountScope(self, e1AggType->Scope);
                    result->E2 = MetaCSemantic_doExprSemantic(self, expr->E2, 0);
                    MetaCSemantic_UnmountScope(self);

                    if (result->E2->Kind == exp_field)
                    {
                        // printf("e2.offset: %d\n", result->E2->Field->Offset);
                    }
                }
                int k = 12;
            }
            else
            {
                assert(!"lhs of -> doesn't seem to be a pointer");
            }
        } break;
        case exp_dot:
        {
            result->E1 = MetaCSemantic_doExprSemantic(self, expr->E1, 0);
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
                    assert(agg->Scope->Owner.Kind == scope_owner_struct);
                break;
                case type_index_union:
                    agg = UnionPtr(self, typeIndexIndex);
                break;
                case type_index_class:
                    assert(0);
                }
                assert(agg != 0);
                assert(agg->Scope != 0);

                MetaCSemantic_MountScope(self, agg->Scope);

                metac_identifier_ptr_t idPtr = {0};
                switch(expr->E2->Kind)
                {
                    case exp_type:
                    case exp_identifier:
                    {
                        idPtr = GetIdentifierPtr(expr->E2);
                    } break;
                    case exp_call:
                    {
                        idPtr = GetIdentifierPtr(expr->E2->E1);
                    } break;
                    default : {
                        printf("Unexpected expression kind in . or -> expression: %s\n",
                            MetaCExpressionKind_toChars(expr->E2->Kind));
                        assert(!"Don't know how to deal with this y of x.y expression");
                    }
                }

                assert(expr->E2->Kind == exp_identifier || expr->E2->Kind == exp_call || expr->E2->Kind == exp_type);
                result->AggExp = result->E1;

                if (expr->E2->Kind == exp_call)
                {
                    // the issue with calling through a member function pointer
                    // is that we have to synthesize the call
                    // such that dot->E1 = struct dot->E2 becomes
                    // call->E1 = dot
                    metac_sema_expression_t* dotExp = result;

                    metac_sema_expression_t * callExp = 0;
                    metac_node_t node =
                        MetaCSemantic_LookupIdentifier(self, idPtr);


                    if (node->Kind == node_decl_field)
                    {
                        metac_type_aggregate_field_t* field =
                            cast(metac_type_aggregate_field_t*)node;
                        result->AggMemberIndex = field->Index;
                        metac_expression_t* fieldExp = AllocNewExpression(exp_field);
                        fieldExp->LocationIdx = expr->E2->LocationIdx;
                        dotExp->DotE2 = AllocNewSemaExpression(self, fieldExp);
                        dotExp->DotE2->Field = field;
                        dotExp->DotE2->TypeIndex = field->Type;
                    }
                    else
                    {
                        int k = 12;
                        assert(0);
                    }

                    MetaCSemantic_doCallSemantic(self, expr->E2, &callExp);
                    if (!callExp)
                    {
                        assert(!"couldn't do call semantic or something");
                    }

                    result = callExp;
                    dotExp->TypeIndex = callExp->Call.Function->TypeIndex;
                    callExp->E1 = dotExp;
                }
                else if (expr->E2->Kind == exp_identifier || expr->E2->Kind == exp_type)
                {
                    result->DotE2 = MetaCSemantic_doExprSemantic(self, expr->E2, 0);
                    //printf("result->DotE2.Kind: %s\n",
                    //    MetaCNodeKind_toChars(result->DotE2->Kind));
                    result->TypeIndex = result->DotE2->TypeIndex;
                }

                MetaCSemantic_UnmountScope(self);

                // hash = 0xfefefefe;
            }
            else
            {
                assert(!"lhs of . doesn't seem to be an aggregate");
            }
        } break;

        case exp_decrement:
        case exp_increment:
        case exp_post_decrement:
        case exp_post_increment:
        case exp_compl:
        case exp_not:
        case exp_umin:
        case exp_paren:
        {
            metac_sema_expression_t* E1 =
                MetaCSemantic_doExprSemantic(self, expr->E1, 0);
            hash = CRC32C_VALUE(hash, E1->Hash);

            if (result->E1->Kind == exp_unknown_value)
                result->Kind = exp_unknown_value;

            //result->Kind = exp_paren;
            result->TypeIndex = E1->TypeIndex;
            result->E1 = E1;
        } break;


        case exp_cast:
        {
            metac_type_index_t castType =
                MetaCSemantic_doTypeSemantic(self, expr->CastType);
            MetaCSemantic_PushResultType(self, castType);

            metac_sema_expression_t* castExp =
                MetaCSemantic_doExprSemantic(self, expr->CastExp, 0);
            hash = CRC32C_VALUE(hash, castExp->Hash);
            result->TypeIndex = castType;
            result->CastType = castType;
            result->CastExp = castExp;
            if (castType.v == 0 || castExp->Kind == exp_unknown_value)
                result->Kind = exp_unknown_value;
        } break;
#define CASE(M) \
    case M:
        FOREACH_BIN_ARITH_EXP(CASE)
        case exp_ternary:
            if (result->E1->Kind == exp_unknown_value || result->E2->Kind == exp_unknown_value)
                result->Kind = exp_unknown_value;

            result->TypeIndex =
                MetaCSemantic_CommonSubtype(self, result->E1->TypeIndex, result->E2->TypeIndex);
        break;
        FOREACH_BIN_ARITH_ASSIGN_EXP(CASE)
            if (result->E1->Kind == exp_unknown_value)
                result->Kind = exp_unknown_value;
            result->TypeIndex = result->E1->TypeIndex;
        break;
#undef CASE
        case exp_index:
            result = MetaCSemantic_doIndexSemantic(self, expr);
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
            hash = CRC32C_VALUE(exp_signed_integer, expr->ValueU64);
            result->TypeIndex = MetaCSemantic_GetTypeIndex(self, type_int, (decl_type_t*)emptyPointer);
        break;
        case exp_float :
            hash = CRC32C_VALUE(exp_float, expr->ValueF23);
            result->TypeIndex = MetaCSemantic_GetTypeIndex(self, type_float, (decl_type_t*)emptyPointer);
        break;

        case exp_assign:
            MetaCSemantic_doAssignSemantic(self, expr, result);
            result->TypeIndex = result->E1->TypeIndex;
        break;
        case exp_lt:
        case exp_le:
        case exp_gt:
        case exp_ge:
        case exp_neq:
        case exp_eq:
            // doCmpExpSemantic()
            //TODO assert that E1 and E2 are comperable
        case exp_andand:
        case exp_oror:
            result->TypeIndex = MetaCSemantic_GetTypeIndex(self, type_bool, (decl_type_t*)emptyPointer);
        break;
        case exp_assert:
            result->E1 =
                MetaCSemantic_doExprSemantic(self, expr->E1, 0);
            result->TypeIndex = MetaCSemantic_GetTypeIndex(self, type_void, (decl_type_t*)emptyPointer);
        break;
        case exp_tuple:
        {
            exp_tuple_t* tupleElement = expr->TupleExpressionList;
            const uint32_t tupleExpressionCount = expr->TupleExpressionCount;
            STACK_ARENA_ARRAY(metac_type_index_t, typeIndicies, 32, &self->TempAlloc)
            STACK_ARENA_ARRAY(metac_sema_expression_t, tupleElements, 32, &self->TempAlloc)
            ARENA_ARRAY_ENSURE_SIZE(tupleElements, tupleExpressionCount);

            // result->TupleExpressionCount = tupleExpressionCount;
            // result->TupleExpressions = AllocNewSemaExpression(self, expr);

            bool isTypeTuple = true;

            for(uint32_t i = 0;
                i < expr->TupleExpressionCount;
                i++)
            {
                metac_expression_t *e = tupleElement->Expression;
                tupleElement = tupleElement->Next;
                metac_sema_expression_t* resultElem = result->TupleExpressions[i];
                MetaCSemantic_doExprSemantic(self, e, resultElem);
                ARENA_ARRAY_ADD(typeIndicies, resultElem->TypeIndex);
                if (resultElem->Kind != exp_type)
                {
                    isTypeTuple &= 0;
                }
            }

            if (isTypeTuple)
            {
                for(uint32_t i = 0; i < tupleExpressionCount; i++)
                {
                    metac_type_index_t typeIndex =
                        (result->TupleExpressions[i])->TypeExp;
                    typeIndicies[i] = typeIndex;
                }
                metac_expression_t typeExp = *expr;
                typeExp.Kind = exp_type;
                result = AllocNewSemaExpression(self, &typeExp);
            }

#define tuple_key 0x55ee11

            uint32_t hash = crc32c(tuple_key, typeIndicies,
                sizeof(metac_type_index_t) * expr->TupleExpressionCount);
            assert(typeIndiciesCount == tupleExpressionCount);

            metac_type_tuple_t typeTuple;
            typeTuple.Header.Kind = decl_type_tuple;
            typeTuple.Header.Hash = hash;
            typeTuple.TypeCount = tupleExpressionCount;
            typeTuple.TypeIndicies = typeIndicies;

           // AllocNewTupleType()
            metac_type_index_t tupleIdx =
                MetaCTypeTable_GetOrEmptyTupleType(&self->TupleTypeTable, &typeTuple);
            if (tupleIdx.v == 0)
            {
                metac_type_index_t* newIndicies =
                    Allocator_Calloc(&self->Allocator,
                        metac_type_index_t, tupleExpressionCount);

        //        metac_type_index_t* newIndicies = (metac_type_index_t*)
        //            malloc(expr->TupleExpressionCount * sizeof(metac_type_index_t));
                memcpy(newIndicies, typeIndicies,
                    expr->TupleExpressionCount * sizeof(metac_type_index_t));
                typeTuple.TypeIndicies = newIndicies;
                tupleIdx =
                    MetaCTypeTable_AddTupleType(&self->TupleTypeTable, &typeTuple);
            }

            ARENA_ARRAY_FREE(typeIndicies);
            if (isTypeTuple)
            {
                result->TypeExp = tupleIdx;
                result->TypeIndex.v = TYPE_INDEX_V(type_index_basic, type_type);
            }
            else
            {
                result->TypeIndex = tupleIdx;
            }
        }
        break;
        case exp_inject:
        {
            result->TypeIndex.v = TYPE_INDEX_V(type_index_basic, type_code);
        } break;
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
                (exp_argument_t*)call->E2 : (exp_argument_t*)emptyNode);

            if (!self->CompilerInterface)
            {
                report_error("There's no compiler interface loaded\n");
                return 0;
            }
#if 0
            // printf("Type(fn) %s\n", MetaCExpressionKind_toChars(fn->Kind));

            int callIdx = -1;

            for(int memberIdx = 0;
                memberIdx < g_compilerInterface->FieldCount;
                memberIdx++)
            {
                metac_identifier_ptr_t id =
                    g_compilerInterface->Fields[memberIdx].Identifier;
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
#endif
        } break;
        case exp_type:
        {
            // type identifiers are special since they may not be types
            decl_type_t* typeExpr = expr->TypeExp;
            if (typeExpr->TypeKind == type_identifier)
            {
                metac_identifier_ptr_t idPtr = typeExpr->TypeIdentifier;

                metac_node_t node =
                    MetaCSemantic_LookupIdentifier(self, idPtr);
                if (node == emptyNode)
                {
                    // TODO sticky couldn't resolve message
                }
                else
                {
                    const char* idString = IdentifierPtrToCharPtr(self->ParserIdentifierTable, idPtr);
                    // printf("Resolving id: '%s' to %p\n", idString, node);
                    ResolveIdentifierToExp(self, node, &result, &hash);
                }
            }
            // type ptrs can also be binary expressions
            else if (expr->TypeExp->TypeKind == type_ptr)
            {

            }
            // and so can type-arrays
            else if (expr->TypeExp->TypeKind == type_array)
            {

            }
            else
            {
                hash = type_key;
                metac_type_index_t typeIdx
                    = MetaCSemantic_doTypeSemantic(self, expr->TypeExp);
                result->TypeExp = typeIdx;
                result->TypeIndex.v = TYPE_INDEX_V(type_index_basic, type_type);
                hash = CRC32C_VALUE(hash, typeIdx);
            }
        } break;
        case exp_typeof:
        {
            hash = typeof_key;
            metac_sema_expression_t* e1 =
                MetaCSemantic_doExprSemantic(self, expr->E1, 0);

            metac_type_index_t type = e1->TypeIndex;
            // usually we assume the type of which we want
            // to get the size is the type of the expression
            if (e1->TypeIndex.v == TYPE_INDEX_V(type_index_basic, type_type))
            {
                // if the expression is any other kind of expression and it is of type type
                // it indicates we want this sizeof be resolved at a later time
                // possibly when calling a function
            }

            result->TypeIndex.v = TYPE_INDEX_V(type_index_basic, type_type);
            result->Kind = exp_type;
            result->TypeExp = type;
        } break;
        case exp_sizeof:
        {
            uint32_t size = -1;
            hash = sizeof_key;
            metac_sema_expression_t* e1 =
                MetaCSemantic_doExprSemantic(self, expr->E1, 0);

            metac_type_index_t typeIdx = e1->TypeIndex;
            // usually we assume the type of which we want
            // to get the size is the type of the expression
            if (e1->Kind == exp_type)
            {
                // Execpt if it's a exp_type expression
                // which is something that resolves to a type such as the identifier int
                typeIdx = e1->TypeExp;
                hash = CRC32C_VALUE(hash, e1->TypeExp);
            } else if (e1->TypeIndex.v == TYPE_INDEX_V(type_index_basic, type_type))
            {
                // if the expression is any other kind of expression and it is of type type
                // it indicates we want this sizeof be resolved at a later time
                // possibly when calling a function
            }

            if (e1->TypeIndex.v != 0 && e1->TypeIndex.v != -1)
                size = MetaCSemantic_GetTypeSize(self, typeIdx);

            result->TypeIndex.v = TYPE_INDEX_V(type_index_basic, type_size_t);
            result->Kind = exp_signed_integer;
            result->ValueU64 = size;
        } break;

        case exp_identifier:
        {

            //printf("Looking up: %s\n",
            //    IdentifierPtrToCharPtr(self->ParserIdentifierTable, result->IdentifierPtr));

            metac_node_t node =
                MetaCSemantic_LookupIdentifier(self,
                                               expr->IdentifierPtr);
            if (node == emptyPointer)
            {
                fprintf(stderr, "Identifier lookup failed\n");
                hash = expr->IdentifierPtr.v;
            }
            else
            {
                ResolveIdentifierToExp(self, node, &result, &hash);
            }
            //printf("Resolved exp_identifier to: %s\n",
            //    MetaCExpressionKind_toChars(result->Kind));
        }
        break;
        case exp_addr:
        {
            MetaCSemantic_PushExpr(self, result);
            result->E1 = MetaCSemantic_doExprSemantic(self, expr->E1, 0);
            MetaCSemantic_PopExpr(self, result);
            assert(result->E1->TypeIndex.v != 0);
            if (!MetaCSemantic_CanHaveAddress(self, result->E1))
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
        } break;
    }
Lret:
    {
    //assert(hash != 0);
    result->Hash = hash;
    }
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
                                  metac_sema_expression_t* expr)
{
    expr = ExtractCastExp(expr);
    expr = UnwrapParen(expr);
    expr = ExtractCastExp(expr);
    expr = UnwrapParen(expr);

    switch (expr->Kind)
    {
        case exp_variable:
            return true;

        default: return false;
    }
}
