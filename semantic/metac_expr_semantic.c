#include "metac_semantic.h"

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

#define report_error(FMT, ...) \
    fprintf(stderr, FMT "\n")

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
                     metac_sema_expression_t** arguments, uint32_t nArgs)
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
    metac_expression_t* fn = call->E1;
    metac_sema_expression_t* result = *resultP;

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
    result->Call.Function = func;

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
        )
    )
    {
        MetaCSemantic_PushExpr(self, result);

        result->E1 = MetaCSemantic_doExprSemantic(self, expr->E1, 0);
        result->E2 = MetaCSemantic_doExprSemantic(self, expr->E2, 0);

        hash = CRC32C_VALUE(hash, result->E1->Hash);
        hash = CRC32C_VALUE(hash, result->E2->Hash);

        MetaCSemantic_PopExpr(self, result);
    }

    bool isParameter = false;

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
            if (expr->E1->Kind == exp_identifier)
            {
                switch(expr->E1->IdentifierKey)
                {
                    case compiler_key:
                    {
                        sema_decl_variable_t fakeDotStruct = {};
                        fakeDotStruct.Kind = decl_variable;
                        uint32_t compilerStructIndex = StructIndex(self, self->CompilerInterface);
                        fakeDotStruct.TypeIndex.v = TYPE_INDEX_V(type_index_struct, compilerStructIndex);
                        fakeDotStruct.VarIdentifier = expr->E1->IdentifierPtr;
                        fakeDotStruct.Storage.v = STORAGE_V(storage_static, 1);

                        result->Kind = exp_variable;
                        result->Variable = &fakeDotStruct;
                        //result->TypeIndex = MetaCSemantic_doTypeSemantic(self, result->Variable);
                        result->TypeIndex = result->Variable->TypeIndex;
                        hash = 2560;
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

        case exp_arrow:
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
                    case exp_identifier:
                    {
                        idPtr = expr->E2->IdentifierPtr;
                    } break;
                    case exp_call:
                    {
                        idPtr = expr->E2->E1->IdentifierPtr;
                    } break;
                    default : {
                        printf("Unexpected expression kind in . or -> expression: %s\n",
                            MetaCExpressionKind_toChars(expr->E2->Kind));
                        assert(!"Don't know how to deal with this y of x.y expression");
                    }
                }

                assert(expr->E2->Kind == exp_identifier || expr->E2->Kind == exp_call);
                result->AggExp = result->E1;

                if (expr->E2->Kind == exp_call)
                {
                    // the issue with calling through a member function pointer
                    // is that we have to synthesize the call
                    // such that dot->E1 = struct dot->E2 becomes
                    // call->E1 = dot

                    metac_sema_expression_t _callExp = {};
                    metac_sema_expression_t* callExp = &_callExp;

                    MetaCSemantic_doCallSemantic(self, expr->E2, &callExp);
                    result->DotE2 = callExp;
                    result->TypeIndex = result->DotE2->TypeIndex;
                }
                else if (expr->E2->Kind == exp_identifier)
                {
                    result->DotE2 = MetaCSemantic_doExprSemantic(self, expr->E2, 0);
                    //printf("result->DotE2.Kind: %s\n",
                    //    MetaCNodeKind_toChars(result->DotE2->Kind));
                    result->TypeIndex = result->DotE2->TypeIndex;
                }

                MetaCSemantic_UnmountScope(self);

                hash = 0xfefefefe;
            }
            else
            {
                assert(!"lhs of . or -> doesn't seem to be an aggregate");
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

            //result->Kind = exp_paren;
            result->TypeIndex = E1->TypeIndex;
            result->E1 = E1;
        } break;

#define CASE(M) \
    case M:

        FOREACH_BIN_ARITH_EXP(CASE)
        case exp_ternary:
            result->TypeIndex =
                MetaCSemantic_CommonSubtype(self, result->E1->TypeIndex, result->E2->TypeIndex);
        break;
        FOREACH_BIN_ARITH_ASSIGN_EXP(CASE)
            result->TypeIndex = result->E1->TypeIndex;
        break;
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
        case exp_assign:
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
            typeTuple.typeCount = tupleExpressionCount;
            typeTuple.typeIndicies = typeIndicies;

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
                typeTuple.typeIndicies = newIndicies;
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
            hash = type_key;
            metac_type_index_t typeIdx
                = MetaCSemantic_doTypeSemantic(self, expr->TypeExp);
            result->TypeExp = typeIdx;
            result->TypeIndex.v = TYPE_INDEX_V(type_index_basic, type_type);
            hash = CRC32C_VALUE(hash, typeIdx);
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
               if (node->Kind == (metac_node_kind_t)exp_identifier)
                {
                    fprintf(stderr, "we should not be retured an identifier\n");
                }
                else if (node->Kind == node_decl_parameter)
                {
                    isParameter = true;
                    goto Lvar;
                }
                else if (node->Kind == node_decl_variable)
                Lvar:
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
                    // result->Kind =enumMember->Value.Kind;
                    // I don't love this cast but oh well
                    result = cast(metac_sema_expression_t*)enumMember;
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
                else
                {
                    printf("[%s:%u] NodeType unexpected: %s\n", __FILE__, __LINE__, MetaCNodeKind_toChars(node->Kind));
                }
            }
            //printf("Resolved exp_identifier to: %s\n",
            //    MetaCExpressionKind_toChars(result->Kind));
        }
        break;
        case exp_addr:
            MetaCSemantic_PushExpr(self, result);
            result->E1 = MetaCSemantic_doExprSemantic(self, expr->E1, 0);
            MetaCSemantic_PopExpr(self, result);
            assert(result->E1->TypeIndex.v != 0);
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

    //assert(hash != 0);
    result->Hash = hash;

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

