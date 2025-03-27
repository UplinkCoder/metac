#include "../semantic/metac_semantic.h"
#include "../codegen/metac_codegen.h"

#ifndef NO_FIBERS
#  include "../os/metac_task.h"
#endif

#include "../semantic/metac_expr_fold.c"

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

void ConvertTupleElementToExp(metac_sema_state_t* sema,
                              metac_sema_expr_t** dstP, metac_type_index_t elemType,
                              uint32_t offset, BCHeap* heap)
{
    metac_expr_t exp = {expr_invalid};
    *dstP = AllocNewSemaExpr(sema, &exp);
    metac_sema_expr_t* dst = *dstP;

    if (elemType.v == TYPE_INDEX_V(type_index_basic, type_int))
    {
        int32_t value = *cast(int32_t*)(heap->heapData + offset);
        dst->Kind = expr_signed_integer;
        dst->TypeIndex = elemType;
        dst->ValueI64 = value;
    }
    else if (elemType.v == TYPE_INDEX_V(type_index_basic, type_unsigned_int))
    {
        uint32_t value = *cast(uint32_t*)(heap->heapData + offset);
        dst->Kind = expr_signed_integer;
        dst->TypeIndex = elemType;
        dst->ValueI64 = value;
    }
    else if (elemType.v == TYPE_INDEX_V(type_index_basic, type_char))
    {
        char value = *cast(char*)(heap->heapData + offset);
        uint32_t charHash = CRC32C_VALUE(~0, value);
        dst->Kind = expr_char;
        dst->TypeIndex = elemType;
        dst->Chars[0] = value;
        dst->CharKey = CHAR_KEY(charHash, 1);
    }
    else if (elemType.v == TYPE_INDEX_V(type_index_basic, type_type))
    {
        metac_type_index_t value = *cast(metac_type_index_t*)(heap->heapData + offset);
        dst->Kind = expr_type;
        dst->TypeIndex = elemType;
        dst->TypeExp = value;
    }
    else assert(0);
}

EXTERN_C const BackendInterface BCGen_interface;
EXTERN_C const BackendInterface Printer_interface;
/*
#define CASE_(EXP) \
    case EXP:

bool IsLiteral(metac_expr_kind_t kind)
{
    switch(kind)
    {
        default: return false;

        FOREACH_LITERAL_EXP(CASE_)
            return true;
    }
}


bool IsConstant(metac_sema_state_t* sema, metac_sema_expr_t* expr)
{
    switch(expr->Kind)
    {
        default:
            return false;

        FOREACH_LITERAL_EXP(CASE_)
            return true;

        FOREACH_UNARY_EXP(CASE_)
            return IsConstant(sema, expr->E1);

        FOREACH_BINARY_EXP(CASE_)
            return (IsConstant(sema, expr->E1) && IsConstant(sema, expr->E2));
    }
}

#undef CASE_
*/

metac_sema_expr_t
EvaluateExpr(metac_sema_state_t* sema,
             metac_sema_expr_t* e,
             BCHeap* heap)
{
    metac_alloc_t interpAlloc;
    metac_bytecode_ctx_t ctx;
    metac_sema_expr_t result = {(metac_expr_kind_t)0};

    if (e == cast(metac_sema_expr_t*) emptyPointer)
    {
        return result;
    }

    if (IsLiteral(e->Kind))
    {
        return *e;
    }

    Allocator_Init(&interpAlloc, &sema->TempAlloc, 0);

    if (true)
    {
        MetaCCodegen_SetDefaultInterface(&Printer_interface);

        MetaCCodegen_Init(&ctx, &interpAlloc);
        ctx.Sema = sema;

        for(uint32_t i = 0; i < sema->GlobalsCount; i++)
        {
            MetaCCodegen_doGlobal(&ctx, sema->Globals[i], i);
        }

        MetaCCodegen_Begin(&ctx, sema->ParserIdentifierTable, sema->ParserStringTable, sema);

        metac_bytecode_function_t fCode =
            MetaCCodegen_GenerateFunctionFromExp(&ctx, e);

        MetaCCodegen_End(&ctx);

        // Printer_StreamToFile(ctx.c, stderr);
    }

    MetaCCodegen_SetDefaultInterface(&BCGen_interface);

    MetaCCodegen_Init(&ctx, 0);
    ctx.Sema = sema;

    for(uint32_t i = 0; i < sema->GlobalsCount; i++)
    {
        MetaCCodegen_doGlobal(&ctx, sema->Globals[i], i);
    }

    MetaCCodegen_Begin(&ctx, sema->ParserIdentifierTable, sema->ParserStringTable, sema);

    metac_bytecode_function_t fCode =
        MetaCCodegen_GenerateFunctionFromExp(&ctx, e);

    MetaCCodegen_End(&ctx);

    uint32_t resultInt =
        MetaCCodegen_RunFunction(&ctx, fCode, &interpAlloc, heap, "", 0);

    MetaCCodegen_UnsetDefaultInterface();

    MetaCCodegen_Free(&ctx);

    // BCGen_printFunction(c);

    if (e->TypeIndex.v == TYPE_INDEX_V(type_index_basic, type_type))
    {
        result.Kind = expr_type;
        result.TypeIndex.v = TYPE_INDEX_V(type_index_basic, type_type);
        result.TypeExp.v = resultInt;
    }
    else if (TYPE_INDEX_KIND(e->TypeIndex) == type_index_tuple)
    {
        metac_type_tuple_t* typeTuple =
            TupleTypePtr(ctx.Sema, TYPE_INDEX_INDEX(e->TypeIndex));

        const uint32_t count = typeTuple->TypeCount;

        uint32_t currrentHeapOffset = resultInt;

        STACK_ARENA_ARRAY(metac_sema_expr_t*, tupleExps, 32, &sema->TempAlloc)

        ARENA_ARRAY_ENSURE_SIZE(tupleExps, count);

        result.Kind = expr_tuple;
        result.TypeIndex = e->TypeIndex;

        for(uint32_t i = 0; i < count; i++)
        {
            BCType bcType = MetaCCodegen_GetBCType(&ctx, typeTuple->TypeIndicies[i]);
            ConvertTupleElementToExp(sema, tupleExps + i, typeTuple->TypeIndicies[i],
                                     currrentHeapOffset, heap);
            currrentHeapOffset += MetaCCodegen_GetStorageSize(&ctx, bcType);
        }

        STACK_ARENA_ARRAY_TO_HEAP(tupleExps, &sema->TempAlloc);

        result.TupleExprs = tupleExps;
        result.TupleExprCount = count;
    }
    else if (TYPE_INDEX_KIND(e->TypeIndex) == type_index_basic &&
             TYPE_INDEX_INDEX(e->TypeIndex) == (uint32_t) type_float)
    {
        result.Kind = expr_float;
        result.ValueF23 = *(float*) &resultInt;
        result.TypeIndex.v = TYPE_INDEX_V(type_index_basic, type_float);
    }
    else
    {
        result.Kind = expr_signed_integer;
        result.TypeIndex.v = e->TypeIndex.v;
        result.ValueI64 = resultInt;
    }
    result.LocationIdx = e->LocationIdx;

    Debug_RemoveAllocator(g_DebugServer, &interpAlloc);
    // Allocator_Release(&interpAlloc);

    return result;
}



/// a return value of INT32_MIN indicates an error
static inline int32_t GetConstI32(metac_sema_state_t* self, metac_sema_expr_t* index, bool *errored)
{
    int32_t result = INT32_MIN;

    if (index->Kind == expr_signed_integer)
    {
        result = cast(int32_t) index->ValueI64;
    }
    else
    {
        *errored = true;
    }

    return result;
}

metac_sema_expr_t* MetaCSemantic_doIndexSemantic_(metac_sema_state_t* self,
                                                        metac_expr_t* expr,
                                                        const char* callFun,
                                                        uint32_t callLine)
{
    metac_sema_expr_t* result = 0;

    metac_sema_expr_t* indexed = MetaCSemantic_doExprSemantic(self, expr->E1, 0/*, expr_asAddress*/);
    metac_sema_expr_t* index = MetaCSemantic_doExprSemantic(self, expr->E2, 0);

    if (indexed->Kind ==  expr_tuple)
    {
        bool errored = false;
        int32_t idx = GetConstI32(self, index, &errored);
        if ((int32_t)indexed->TupleExprCount > idx)
        {
            result = indexed->TupleExprs[idx];
        }
        else if (!errored)
        {
            xfprintf(stderr, "TupleIndex needs to be less than: %u", indexed->TupleExprCount);
        }
        else
        {
            xfprintf(stderr, "index is not a constant value\n");
        }
    }
    else if (indexed->Kind == expr_variable)
    {
        assert(TYPE_INDEX_KIND(indexed->TypeIndex) == type_index_array
            || TYPE_INDEX_KIND(indexed->TypeIndex) == type_index_ptr);

        result = AllocNewSemaExpr(self, expr);
        result->TypeIndex = MetaCSemantic_GetElementType(self, indexed->TypeIndex);
        result->E1 = indexed;
        result->E2 = index;
    }
    else if (indexed->Kind == expr_string)
    {
        metac_type_index_t charType =
            MetaCSemantic_GetTypeIndex(self, type_char, (decl_type_t*)emptyPointer);
        result = AllocNewSemaExpr(self, expr);
        result->TypeIndex = charType;
        // result->Chars[
    }

    return  result;
}

metac_sema_expr_t* doCmpExpSemantic(metac_sema_state_t* self,
                                          metac_location_t loc,
                                          metac_expr_kind_t kind,
                                          metac_sema_expr_t* e1,
                                          metac_sema_expr_t* e2,
                                          metac_sema_expr_t* result)
{
    const metac_type_index_t tbool
        = MetaCSemantic_GetTypeIndex(self, type_bool, (decl_type_t*)emptyPointer);


    switch(kind)
    {
        default: {
            // InternalError("%s is not a comparison expression", MetaCExprKind_toChars(cmpExp->Kind));
        };

        case expr_lt:
        case expr_le:
        case expr_gt:
        case expr_ge:
        case expr_neq:
        case expr_eq: {
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
metac_sema_expr_t* ResloveFuncCall(metac_sema_state_t* self,
                                         metac_expr_t* fn,
                                         metac_sema_expr_t** arguments,
                                         uint32_t nArgs)
{
    metac_expr_kind_t kind = fn->Kind;
    metac_sema_expr_t* result = MetaCSemantic_doExprSemantic(self, fn, 0);
    return result;
}

static inline
void MetaCSemantic_doCallSemantic(metac_sema_state_t* self,
                                  metac_expr_t* call,
                                  metac_sema_expr_t** resultP)
{
    metac_sema_expr_t* result;
    metac_expr_t* fn = call->E1;

    expr_argument_t* argList = (METAC_NODE(call->E2) != emptyNode ?
        (expr_argument_t*)call->E2 : (expr_argument_t*)emptyNode);
    STACK_ARENA_ARRAY(metac_sema_expr_t*, arguments, 64, &self->TempAlloc);

    uint32_t nArgs = 0;
    while(METAC_NODE(argList) != emptyNode)
    {
        nArgs++;
        ARENA_ARRAY_ADD(arguments,
            MetaCSemantic_doExprSemantic(self, argList->Expr, 0));
        argList = argList->Next;
    }



    metac_sema_expr_t* func =
        ResloveFuncCall(self, fn, arguments, nArgs);
    if (func)
    {
        result = AllocNewSemaExpr(self, call);
        (*resultP) = result;
    }
    else
    {
        assert(!"Can't resolve function call\n");
    }
    result->Call.Function = func;

    if (func->Function)

    xprintf("function call with: %u arguments\n", nArgs);

    STACK_ARENA_ARRAY_TO_HEAP(arguments, &self->Allocator);

    metac_expr_t dummy;
    dummy.LocationIdx = call->LocationIdx;
    dummy.Kind = expr_variable;

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

void ResolveIdentifierToExp(metac_sema_state_t* self,
                            metac_node_t node,
                            metac_sema_expr_t** resultP,
                            uint32_t* hashP)
{
    uint32_t hash = *hashP;
    metac_sema_expr_t* result = *resultP;
    // printf("Resolving node: %p\n", node);

    if (node->Kind == (metac_node_kind_t)expr_identifier)
    {
        xfprintf(stderr, "we should not be retured an identifier\n");
    }
    else if (node->Kind == node_decl_variable ||
             node->Kind == node_decl_parameter)
    {
        sema_decl_variable_t* v = cast(sema_decl_variable_t*)node;
        result->Kind = expr_variable;
        result->Variable = v;
        result->TypeIndex = v->TypeIndex;
        hash = v->Hash;
    }
    else if (node->Kind == node_decl_field)
    {
        metac_type_aggregate_field_t* field =
            cast(metac_type_aggregate_field_t*) node;
        result->Kind = expr_field;
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
        result->Kind = expr_type;
        result->TypeExp.v =
            TYPE_INDEX_V(type_index_struct, StructIndex(self, cast(metac_type_aggregate_t*)node));
        result->TypeIndex.v = TYPE_INDEX_V(type_index_basic, type_type);
    }
    else if (node->Kind == node_decl_type_typedef)
    {
        result->Kind = expr_type;
        result->TypeExp.v =
            TYPE_INDEX_V(type_index_typedef, TypedefIndex(self, cast(metac_type_typedef_t*)node));
    }
    else if (node->Kind == node_decl_function)
    {
        sema_decl_function_t* func = cast(sema_decl_function_t*) node;
        result->Kind = expr_function;
        result->Function = func;
        result->TypeIndex = func->TypeIndex;
    }
    else if (node->Kind == node_expr_unknown_value)
    {
        (*result) = *cast(metac_sema_expr_t*) node;
        hash = result->Hash;
    }
    else
    {
        printf("[%s:%u] NodeType unexpected: %s\n", __FILE__, __LINE__, MetaCNodeKind_toChars(node->Kind));
    }

    (*hashP) = hash;
}

metac_sema_expr_t* UnwrapParen(metac_sema_expr_t* e)
{
    metac_sema_expr_t* result = e;

    while(result->Kind == expr_paren)
    {
        result = result->E1;
    }

    return result;
}

metac_sema_expr_t* ExtractCastExp(metac_sema_expr_t* e)
{
    metac_sema_expr_t* result = e;

    while(result->Kind == expr_cast)
    {
        result = result->CastExp;
    }

    return result;
}

/// Strips casts and parens
metac_sema_expr_t* UnwrapCastExp(metac_sema_expr_t* e)
{
    metac_sema_expr_t* result = e;

    while(result->Kind == expr_paren || result->Kind == expr_cast)
    {
        if (e->Kind == expr_cast)
        {
            result = result->CastExp;
        }
        else if (e->Kind == expr_paren)
        {
            result = result->E1;
        }
    }

    return result;
}


void MetaCSemantic_doAssignSemantic(metac_sema_state_t* self,
                                    metac_expr_t* expr,
                                    metac_sema_expr_t* result)
{
    assert(expr->Kind == expr_assign);
    assert(result->Kind == expr_assign);

    if (result->E1->TypeIndex.v != result->E2->TypeIndex.v)
    {

    }
}

metac_identifier_ptr_t GetIdentifierPtr(metac_expr_t* expr)
{
    metac_identifier_ptr_t result = {0};

    switch(expr->Kind)
    {
        case expr_type:
        {
            if (expr->TypeExp->TypeKind == type_identifier)
            {
                result.v = expr->TypeExp->TypeIdentifier.v;
            }
        } break;
        case expr_identifier:
        {
            result.v = expr->IdentifierPtr.v;
        } break;

        default: assert(0);
    }

    return result;
}

void MetaCSemantic_PushResultType(metac_sema_state_t* self, metac_type_index_t type_)
{

}

decl_type_t* MetaCSemantic_DeclTypeFromTypeIndex(metac_sema_state_t* self, metac_type_index_t typeIndex)
{
    return NodeFromTypeIndex(self, typeIndex)->Origin;
}

metac_expr_t* RewriteAndtoAddrIfNeeded(metac_sema_state_t* self, metac_expr_t* expr)
{
    metac_expr_t* result = expr;
    assert(expr->Kind == expr_and);

    // check if lhs of & exp could be a parenthesized type
    // in which case we have a cast followed by an address of
    if (expr->E1->Kind == expr_paren
     && expr->E1->E1->Kind == expr_type)
    {
        metac_sema_expr_t E1 = {(metac_expr_kind_t)0};
        MetaCSemantic_doExprSemantic(self, expr->E1->E1, &E1);
        if (E1.Kind == expr_type)
        {
            result = AllocNewExpr(expr_cast);
            result->LocationIdx = expr->LocationIdx;
            result->CastType = MetaCSemantic_DeclTypeFromTypeIndex(self, E1.TypeExp);
            result->CastExp = expr->E2;
        }
    }
    else
    {

    }

    return result;
}

metac_sema_expr_t* MetaCSemantic_doExprSemantic_(metac_sema_state_t* self,
                                                 metac_expr_t* expr,
                                                 metac_sema_expr_t* result,
                                                 const char* callFun,
                                                 uint32_t callLine)
{
    static metac_printer_t debugPrinter = {0};
    if (!debugPrinter.StringTable)
    {
        MetaCPrinter_Init(&debugPrinter, self->ParserIdentifierTable, self->ParserStringTable, 0);
    }

    if (expr->Kind == expr_and)
    {
        expr = RewriteAndtoAddrIfNeeded(self, expr);
    }

    if (!result)
    {
        result = AllocNewSemaExpr(self, expr);
    }

    uint32_t hash = 0;

    if (IsBinaryExp(expr->Kind)
        && (   expr->Kind != expr_arrow
            && expr->Kind != expr_dot
            && expr->Kind != expr_index
            && expr->Kind != expr_call
        ) ||   expr->Kind == expr_ternary
    )
    {
        MetaCSemantic_PushExpr(self, result);

        if (expr->Kind == expr_ternary)
        {
            result->Econd = MetaCSemantic_doExprSemantic(self, expr->Econd, 0);
            hash = CRC32C_VALUE(hash, result->Econd->Hash);
        }

        result->E1 = MetaCSemantic_doExprSemantic(self, expr->E1, 0);
        result->E2 = MetaCSemantic_doExprSemantic(self, expr->E2, 0);

        if (METAC_NODE(result->E1) != emptyNode) hash = CRC32C_VALUE(hash, result->E1->Hash);
        if (METAC_NODE(result->E2) != emptyNode) hash = CRC32C_VALUE(hash, result->E2->Hash);

        MetaCSemantic_PopExpr(self, result);
    }

    switch(expr->Kind)
    {
        case expr_invalid:
        default:
        {
            xprintf("expression semantic doesn't support %s yet.\n",
                    MetaCExprKind_toChars(expr->Kind));
            assert(0);
        }

        case expr_comma:
        {
            metac_sema_expr_t* r = result->E2;
            while(METAC_NODE(r) != emptyNode && r->Kind == expr_comma)
            {
                r = r->E2;
            }
            if (METAC_NODE(r) != emptyNode)
            {
                result->TypeIndex = r->TypeIndex;
            }
            else
            {
                metac_type_index_t invalidTypeIndex = {0};
                result->TypeIndex = invalidTypeIndex;
            }
        } break;

        case expr_unary_dot:
        {
            metac_identifier_ptr_t idPtr = {0};
            uint32_t idKey = 0;
            const char* idString = 0;

            if (expr->E1->Kind == expr_type &&
                expr->E1->TypeExp->TypeKind == type_identifier)
            {
                metac_sema_expr_t holder;
                idPtr = expr->E1->TypeExp->TypeIdentifier;
                goto LswitchIdKey;
            }
            else if (result->E1->Kind == expr_identifier)
            {
                idPtr = (expr->E1->IdentifierPtr);
LswitchIdKey:
                idString = IdentifierPtrToCharPtr(self->ParserIdentifierTable, idPtr);
                int len = strlen(idString);
                idKey = IDENTIFIER_KEY(crc32c(~0, idString, len), len);

                switch(idKey)
                {
                    case Compiler_key:
                    {
                        result->Kind = expr_variable;
                        result->Variable = &self->CompilerVariable;
                        // result->TypeIndex = MetaCSemantic_doTypeSemantic(self, result->Variable);
                        result->TypeIndex = result->Variable->TypeIndex;
                        hash = self->CompilerInterface->Header.Hash;
                    } break;
                }
            }
            else
            {
                xprintf("Unary dot %s not recognized: %s\n", MetaCExprKind_toChars(result->E1->Kind), MetaCPrinter_PrintExpr(&debugPrinter, expr));
                assert(0);
            }
        } break;

        case expr_call:
        {
            MetaCSemantic_doCallSemantic(self, expr, &result);
        } break;

        case expr_deref:
        {
            result->E1 = MetaCSemantic_doExprSemantic(self, expr->E1, 0);
            result->E1 = UnwrapParen(result->E1);

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

        case expr_arrow:
        case expr_dot:
        {
            // for the semantic of E2 we need to do the lookup in the scope of
            // the type of E1
            metac_type_index_t e1Type = {0};
            uint32_t typeIndexIndex = 0;

            bool isArrow = expr->Kind == expr_arrow;
            metac_type_index_t elementType = {0};
            metac_type_aggregate_t* e1AggType = 0;
            metac_type_ptr_t* ptrType = 0;

            result->E1 = MetaCSemantic_doExprSemantic(self, expr->E1, 0);
            e1Type = result->E1->TypeIndex;
            typeIndexIndex = TYPE_INDEX_INDEX(e1Type);

            // first make the arrowExp into a dot exp essentailly
            if (isArrow || TYPE_INDEX_KIND(e1Type) == type_index_ptr)
            {
                elementType =
                    PtrTypePtr(self, TYPE_INDEX_INDEX(e1Type))->ElementType;
                metac_sema_expr_t* e1 = UnwrapParen(result->E1);
                e1 = ExtractCastExp(e1);

                if (TYPE_INDEX_KIND(e1Type) != type_index_ptr)
                {
                    SemanticError(expr->LocationIndex,
                        "lhs of -> is supposed to be a pointer but it is: %s\n",
                        TypeToChars(self, e1Type));
                    return (metac_sema_expr_t*)emptyNode;
                }

                e1Type = PtrTypePtr(self, TYPE_INDEX_INDEX(e1Type))->ElementType;
                typeIndexIndex = TYPE_INDEX_INDEX(e1Type);
            }

            if (IsAggregateType(TYPE_INDEX_KIND(e1Type)))
            {
                metac_type_aggregate_t* agg = 0;
                switch(TYPE_INDEX_KIND(e1Type))
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
                    case expr_type:
                    case expr_identifier:
                    {
                        idPtr = GetIdentifierPtr(expr->E2);
                    } break;
                    case expr_call:
                    {
                        idPtr = GetIdentifierPtr(expr->E2->E1);
                    } break;
                    default : {
                        printf("Unexpected expression kind in . or -> expression: %s\n",
                            MetaCExprKind_toChars(expr->E2->Kind));
                        assert(!"Don't know how to deal with this y of x.y expression");
                    }
                }

                assert(expr->E2->Kind == expr_identifier || expr->E2->Kind == expr_call || expr->E2->Kind == expr_type);
                result->AggExp = result->E1;

                if (expr->E2->Kind == expr_call)
                {
                    // the issue with calling through a member function pointer
                    // is that we have to synthesize the call
                    // such that dot->E1 = struct dot->E2 becomes
                    // call->E1 = dot
                    metac_sema_expr_t* dotExp = result;

                    metac_sema_expr_t * callExp = 0;
                    metac_node_t node =
                        MetaCSemantic_LookupIdentifier(self, idPtr);

                    if (node == emptyNode)
                    {
                        SemanticError(dotExp->LocatiionIndex,
                                      "function %s couldn't be found\n",
                                      IdentifierPtrToCharPtr(self->ParserIdentifierTable, idPtr)
                        );

                        METAC_NODE(result) = emptyNode;
                    }

                    if (node->Kind == node_decl_field)
                    {
                        metac_type_aggregate_field_t* field =
                            cast(metac_type_aggregate_field_t*)node;

                        metac_expr_t* fieldExp = AllocNewExpr(expr_field);
                        fieldExp->LocationIdx = expr->E2->LocationIdx;
                        dotExp->DotE2 = AllocNewSemaExpr(self, fieldExp);
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
                else if (expr->E2->Kind == expr_identifier || expr->E2->Kind == expr_type)
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

        case expr_decrement:
        case expr_increment:
        case expr_post_decrement:
        case expr_post_increment:
        case expr_compl:
        case expr_not:
        case expr_umin:
        case expr_paren:
        {
            metac_sema_expr_t* E1 =
                MetaCSemantic_doExprSemantic(self, expr->E1, 0);

            if (METAC_NODE(E1) == emptyNode)
            {
                METAC_NODE(result) = emptyNode;
                break;
            }

            hash = CRC32C_VALUE(hash, E1->Hash);

            if (result->E1->Kind == expr_unknown_value)
                result->Kind = expr_unknown_value;

            //result->Kind = expr_paren;
            result->TypeIndex = E1->TypeIndex;
            result->E1 = E1;
        } break;


        case expr_cast:
        {
            metac_type_index_t castType =
                MetaCSemantic_doTypeSemantic(self, expr->CastType);
            MetaCSemantic_PushResultType(self, castType);

            metac_sema_expr_t* castExp =
                MetaCSemantic_doExprSemantic(self, expr->CastExp, 0);
            hash = CRC32C_VALUE(hash, castExp->Hash);
            result->TypeIndex = castType;
            result->CastType = castType;
            result->CastExp = castExp;
            if (castType.v == 0 || castExp->Kind == expr_unknown_value)
                result->Kind = expr_unknown_value;
        } break;
#define CASE(M) \
    case M:
        FOREACH_BIN_ARITH_EXP(CASE)
        case expr_ternary:
            if (result->E1->Kind == expr_unknown_value || result->E2->Kind == expr_unknown_value)
                result->Kind = expr_unknown_value;

            result->TypeIndex =
                MetaCSemantic_CommonSubtype(self, result->E1->TypeIndex, result->E2->TypeIndex);
        break;
        FOREACH_BIN_ARITH_ASSIGN_EXP(CASE)
            if (result->E1->Kind == expr_unknown_value)
                result->Kind = expr_unknown_value;
            result->TypeIndex = result->E1->TypeIndex;
        break;
#undef CASE
        case expr_index:
            result = MetaCSemantic_doIndexSemantic(self, expr);
        break;
        case expr_char :
            result->TypeIndex = MetaCSemantic_GetTypeIndex(self, type_char, (decl_type_t*)emptyPointer);
        break;
        case expr_string :
            result->TypeIndex = MetaCSemantic_GetArrayTypeOf(self,
                MetaCSemantic_GetTypeIndex(self, type_char, (decl_type_t*)emptyPointer),
                LENGTH_FROM_STRING_KEY(expr->StringKey) + 1);
        break;
        case expr_signed_integer :
        {
            decl_type_t typeInfo = {(metac_decl_kind_t)0};
            metac_type_kind_t baseType = type_int;

            hash = CRC32C_VALUE(expr_signed_integer, expr->ValueU64);
            if (expr->NumberFlags & number_flag_unsigned)
            {
                U32(typeInfo.TypeModifiers) |= typemod_unsigned;
            }
            if (expr->NumberFlags & number_flag_long)
            {
                baseType = type_long;
            }
            if (expr->NumberFlags & number_flag_long_long)
            {
                baseType = type_long_long;
            }

            typeInfo.TypeKind = baseType;
            result->TypeIndex =
                MetaCSemantic_GetTypeIndex(self, baseType, &typeInfo);
        }
        break;
        case expr_float :
            hash = CRC32C_VALUE(expr_float, expr->ValueF23);
            result->TypeIndex = MetaCSemantic_GetTypeIndex(self, type_float, (decl_type_t*)emptyPointer);
        break;

        case expr_assign:
            result->Kind = expr_assign;
            MetaCSemantic_doAssignSemantic(self, expr, result);
            result->TypeIndex = result->E1->TypeIndex;
        break;
        case expr_lt:
        case expr_le:
        case expr_gt:
        case expr_ge:
        case expr_neq:
        case expr_eq:
            // doCmpExpSemantic()
            //TODO assert that E1 and E2 are comperable
        case expr_andand:
        case expr_oror:
            result->TypeIndex = MetaCSemantic_GetTypeIndex(self, type_bool, (decl_type_t*)emptyPointer);
        break;
        case expr_assert:
            result->E1 =
                MetaCSemantic_doExprSemantic(self, expr->E1, 0);
            result->TypeIndex = MetaCSemantic_GetTypeIndex(self, type_void, (decl_type_t*)emptyPointer);
        break;
        case expr_tuple:
        {
            expr_tuple_t* tupleElement = expr->TupleExprList;
            const uint32_t tupleExprCount = expr->TupleExprCount;
            STACK_ARENA_ARRAY(metac_type_index_t, typeIndicies, 32, &self->TempAlloc)
            STACK_ARENA_ARRAY(metac_sema_expr_t, tupleElements, 32, &self->TempAlloc)
            ARENA_ARRAY_ENSURE_SIZE(tupleElements, tupleExprCount);

            // result->TupleExprCount = tupleExprCount;
            // result->TupleExprs = AllocNewSemaExpr(self, expr);

            // an empty tuple is a value tuple by default
            bool isTypeTuple = (expr->TupleExprCount != 0);

            for(uint32_t i = 0;
                i < expr->TupleExprCount;
                i++)
            {
                metac_expr_t *e = tupleElement->Expr;
                tupleElement = tupleElement->Next;
                metac_sema_expr_t* resultElem = result->TupleExprs[i];
                MetaCSemantic_doExprSemantic(self, e, resultElem);
                ARENA_ARRAY_ADD(typeIndicies, resultElem->TypeIndex);
                if (resultElem->Kind != expr_type)
                {
                    isTypeTuple &= 0;
                }
            }

            if (isTypeTuple)
            {
                for(uint32_t i = 0; i < tupleExprCount; i++)
                {
                    metac_type_index_t typeIndex =
                        (result->TupleExprs[i])->TypeExp;
                    typeIndicies[i] = typeIndex;
                }
                metac_expr_t typeExp = *expr;
                typeExp.Kind = expr_type;
                result = AllocNewSemaExpr(self, &typeExp);
            }

#define tuple_key 0x55ee11

            uint32_t hash = crc32c(tuple_key, typeIndicies,
                sizeof(metac_type_index_t) * expr->TupleExprCount);
            assert(typeIndiciesCount == tupleExprCount);

            metac_type_tuple_t typeTuple;
            typeTuple.Header.Kind = decl_type_tuple;
            typeTuple.Header.Hash = hash;
            typeTuple.TypeCount = tupleExprCount;
            typeTuple.TypeIndicies = typeIndicies;

           // AllocNewTupleType()
            metac_type_index_t tupleIdx =
                MetaCTypeTable_GetOrEmptyTupleType(&self->TupleTypeTable, &typeTuple);
            if (tupleIdx.v == 0)
            {
                metac_type_index_t* newIndicies =
                    Allocator_Calloc(&self->Allocator,
                        metac_type_index_t, tupleExprCount);

        //        metac_type_index_t* newIndicies = (metac_type_index_t*)
        //            malloc(expr->TupleExprCount * sizeof(metac_type_index_t));
                memcpy(newIndicies, typeIndicies,
                    expr->TupleExprCount * sizeof(metac_type_index_t));
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
        case expr_inject:
        {
            result->TypeIndex.v = TYPE_INDEX_V(type_index_basic, type_code);
        } break;
        case expr_dot_compiler:
        {
            if (expr->E1->Kind != expr_call)
            {
               xfprintf(stderr, "Only calls are supported not %s\n",
                    MetaCExprKind_toChars(expr->E1->Kind));
                break;
            }
            metac_expr_t* call = expr->E1;
            metac_expr_t* fn = call->E1;
            expr_argument_t* args = (METAC_NODE(call->E2) != emptyNode ?
                (expr_argument_t*)call->E2 : (expr_argument_t*)emptyNode);

            if (!self->CompilerInterface)
            {
                report_error("There's no compiler interface loaded\n");
                return 0;
            }
#if 0
            // printf("Type(fn) %s\n", MetaCExprKind_toChars(fn->Kind));

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
                result->Kind = expr_signed_integer;
                result->ValueI64 = 0;
                result->TypeIndex.v = 0;
            }
            else
            {

            }
            // CompilerInterface_Call(
#endif
        } break;
        case expr_type:
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
                    const char* idString = IdentifierPtrToCharPtr(self->ParserIdentifierTable, idPtr);
                    result = cast(metac_sema_expr_t*) emptyNode;
                    // if (!ResolveErrorReported(self, typeExpr))
#ifndef NO_FIBER
                    YIELD("waiting_for_resolve");
#endif
                    {
                        xfprintf(stderr, "Could not resolve %s\n", idString);
                    }
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
                result->Kind = expr_type;
                result->TypeExp = typeIdx;
                result->TypeIndex.v = TYPE_INDEX_V(type_index_basic, type_type);
                hash = CRC32C_VALUE(hash, typeIdx);
            }
        } break;
        case expr_typeof:
        {
            hash = typeof_key;
            metac_sema_expr_t* e1 =
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
            result->Kind = expr_type;
            result->TypeExp = type;
        } break;
        case expr_sizeof:
        {
            int32_t size = -1;
            hash = sizeof_key;
            metac_sema_expr_t* e1 =
                MetaCSemantic_doExprSemantic(self, expr->E1, 0);

            while (e1->Kind == expr_paren)
            {
                e1 = e1->E1;
            }

            metac_type_index_t typeIdx = e1->TypeIndex;
            // usually we assume the type of which we want
            // to get the size is the type of the expression
            if (e1->Kind == expr_type)
            {
                // Execpt if it's a expr_type expression
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
            result->Kind = expr_signed_integer;
            result->ValueU64 = size;
        } break;

        case expr_identifier:
        {

            //printf("Looking up: %s\n",
            //    IdentifierPtrToCharPtr(self->ParserIdentifierTable, result->IdentifierPtr));

            metac_node_t node =
                MetaCSemantic_LookupIdentifier(self,
                                               expr->IdentifierPtr);
            if (node == emptyPointer)
            {
                xfprintf(stderr, "Identifier lookup failed\n");
                hash = expr->IdentifierPtr.v;
            }
            else
            {
                ResolveIdentifierToExp(self, node, &result, &hash);
            }
            //printf("Resolved expr_identifier to: %s\n",
            //    MetaCExprKind_toChars(result->Kind));
        }
        break;
        case expr_addr:
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
                //e1String = MetaCPrinter_PrintExpr(printer, expr->E1);

                SemanticError(self, "cannot take the address of %s", e1String);
            }
            else
            {
                result->TypeIndex = MetaCSemantic_GetPtrTypeOf(self, result->E1->TypeIndex);
                metac_sema_expr_t* varExp = UnwrapCastExp(result->E1);
                assert(varExp->Kind == expr_variable);
                varExp->Variable->VarFlags |= variable_address_taken;
            }
        } break;
    }
Lret:
    if (result != cast(metac_sema_expr_t*)emptyNode)
    {
    //assert(hash != 0);
    result->Hash = hash;
    }
    return result;
}

void MetaCSemantic_PushExpr(metac_sema_state_t* self, metac_sema_expr_t* expr)
{
    if (self->ExprStackCapacity < self->ExprStackSize)
    {
        assert(0);
        // we would need to realloc in this case.
    }
}

void MetaCSemantic_PopExpr(metac_sema_state_t* self,  metac_sema_expr_t* expr)
{

}

bool MetaCSemantic_CanHaveAddress(metac_sema_state_t* self,
                                  metac_sema_expr_t* expr)
{
    expr = UnwrapCastExp(expr);

    switch (expr->Kind)
    {
        case expr_variable:
            return true;

        default: return false;
    }
}
