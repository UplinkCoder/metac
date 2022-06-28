#include "metac_semantic.h"
#include "metac_task.h"


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
                                                task->Origin.Func, task->Origin.File);
}
#endif

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
                                                       metac_sema_expression_t* result,
                                                       const char* callFun,
                                                       uint32_t callLine)
{
    if (!result)
    {
        result = AllocNewSemaExpression(self, expr);
    }

    if (IsBinaryExp(expr->Kind)
        && (expr->Kind != exp_arrow && expr->Kind != exp_dot))
    {
        MetaCSemantic_PushExpr(self, result);

        result->E1 = MetaCSemantic_doExprSemantic(self, expr->E1, 0);
        result->E2 = MetaCSemantic_doExprSemantic(self, expr->E2, 0);

        MetaCSemantic_PopExpr(self, result);
    }

    switch(expr->Kind)
    {
        case exp_invalid:
            assert(0);

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
                break;
                case type_index_union:
                    agg = UnionPtr(self, typeIndexIndex);
                break;
                case type_index_class:
                    assert(0);
                }
                assert(agg != 0);

                assert(expr->E2->Kind == exp_identifier);

#if 0
// TODO enable scope search! which means we store a compacted scope table on persistance of the tmp scope
                uint32_t idPtrHash = crc32c(~0,
                                            &expr->E2->IdentifierPtr,
                                            sizeof(expr->E2->IdentifierPtr));
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
                    MetaCSemantic_doExprSemantic(self, expr->E1, 0);

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
                MetaCSemantic_doExprSemantic(self, expr->E1, 0);
            //result->Kind = exp_type;
            result->TypeIndex.v =
                TYPE_INDEX_V(type_index_basic, type_type);
            result->TypeExp = E1->TypeIndex;
        } break;

        case exp_paren:
        {
            metac_sema_expression_t* E1 =
                MetaCSemantic_doExprSemantic(self, expr->E1, 0);
            //result->Kind = exp_paren;
            result->TypeIndex = E1->TypeIndex;
            result->E1 = E1;
        } break;

#define CASE(M) case M:

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
            result->TypeIndex = MetaCSemantic_GetTypeIndex(self, type_int, (decl_type_t*)emptyPointer);
        break;
        case exp_tuple:
        {
            exp_tuple_t* tupleElement = expr->TupleExpressionList;
            const uint32_t tupleExpressionCount =
                expr->TupleExpressionCount;
            if (expr->TupleExpressionCount > 128)
            {
                SemanticError(0,
                    "Tuples with more than 128 elements are currently not supported, given %u\n",
                    tupleExpressionCount);
                return 0;
            }
            result->TupleExpressionCount = tupleExpressionCount;
            for(uint32_t i = 0;
                i < expr->TupleExpressionCount;
                i++)
            {
                metac_expression_t *e = tupleElement->Expression;
                tupleElement = tupleElement->Next;
                metac_sema_expression_t* resultElem = result->TupleExpressions + i;
                MetaCSemantic_doExprSemantic(self, e, resultElem);
            }

            metac_type_index_t typeIndicies[128];

            for(uint32_t i = 0; i < tupleExpressionCount; i++)
            {
                typeIndicies[i] = (result->TupleExpressions + i)->TypeIndex;
            }

           uint32_t hash = crc32c(~0, typeIndicies,
                sizeof(metac_type_index_t) * expr->TupleExpressionCount);

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
                metac_type_index_t* newIndicies = (metac_type_index_t*)
                    malloc(expr->TupleExpressionCount * sizeof(metac_type_index_t));
                memcpy(newIndicies, typeIndicies,
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
                MetaCSemantic_doExprSemantic(self, expr->E1, 0);
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
                if (node->Kind == (metac_node_kind_t)exp_identifier)
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
            result->E1 = MetaCSemantic_doExprSemantic(self, expr->E1, 0);
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
