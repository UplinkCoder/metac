#include "../os/compat.h"
#include "metac_semantic.h"
#include "metac_scope.h"

uint32_t StructIndex(metac_sema_state_t* self, metac_type_aggregate_t* struct_)
{
    uint32_t result = (struct_ - self->StructTypeTable.Slots);
    return result;
}

uint32_t UnionIndex(metac_sema_state_t* self, metac_type_aggregate_t* union_)
{
    uint32_t result = (union_ - self->UnionTypeTable.Slots);
    return result;
}


uint32_t FunctionIndex(metac_sema_state_t* self, sema_decl_function_t* func)
{
    uint32_t result = (func - self->Functions);
    return result;
}

uint32_t StmtIndex_(metac_sema_state_t* self, metac_sema_stmt_t* stmt)
{
    uint32_t result = (stmt - self->Stmts);
    return result;
}

uint32_t TypedefIndex(metac_sema_state_t* self, metac_type_typedef_t* typedef_)
{
    uint32_t result = (typedef_ - self->TypedefTypeTable.Slots);
    return result;
}

uint32_t ArrayTypeIndex(metac_sema_state_t* self, metac_type_array_t* array)
{
    uint32_t result = (array - self->ArrayTypeTable.Slots);
    return result;
}

uint32_t PtrTypeIndex(metac_sema_state_t* self, metac_type_ptr_t* ptr)
{
    uint32_t result = (ptr - self->PtrTypeTable.Slots);
    return result;
}

uint32_t FunctiontypeIndex(metac_sema_state_t* self, metac_type_functiontype_t* functiontype)
{
    uint32_t result = (functiontype - self->FunctionTypeTable.Slots);
    return result;
}

uint32_t EnumIndex(metac_sema_state_t* self, metac_type_enum_t* enumtype)
{
    uint32_t result = (enumtype - self->EnumTypeTable.Slots);
    return result;
}


uint32_t TupleTypeIndex(metac_sema_state_t* self, metac_type_tuple_t* tupletype)
{
    uint32_t result = (tupletype - self->TupleTypeTable.Slots);
    return result;
}

metac_type_aggregate_t* StructPtr(metac_sema_state_t* self, uint32_t index)
{
    metac_type_aggregate_t* result = (self->StructTypeTable.Slots + index);
    return result;
}

metac_type_aggregate_t* UnionPtr(metac_sema_state_t* self, uint32_t index)
{
    metac_type_aggregate_t* result = (self->UnionTypeTable.Slots + index);
    return result;
}

sema_decl_function_t* FunctionPtr(metac_sema_state_t* self, uint32_t index)
{
    sema_decl_function_t* result = (self->Functions + index);
    return result;
}

metac_sema_stmt_t* StmtPtr(metac_sema_state_t* self, uint32_t index)
{
    metac_sema_stmt_t* result = (self->Stmts + index);
    return result;
}

metac_type_typedef_t* TypedefPtr(metac_sema_state_t* self, uint32_t index)
{
    metac_type_typedef_t* result = (self->TypedefTypeTable.Slots + index);
    return result;
}

metac_type_ptr_t* PtrTypePtr(metac_sema_state_t* self, uint32_t index)
{
    metac_type_ptr_t* result = (self->PtrTypeTable.Slots + index);
    return result;
}

metac_type_array_t* ArrayTypePtr(metac_sema_state_t* self, uint32_t index)
{
    metac_type_array_t* result = (self->ArrayTypeTable.Slots + index);
    return result;
}

metac_type_template_t* TemplateTypePtr(metac_sema_state_t* self, uint32_t index)
{
    metac_type_template_t* result = (self->TemplateTypeTable.Slots + index);
    return result;
}

metac_type_functiontype_t* FunctiontypePtr(metac_sema_state_t* self, uint32_t index)
{
    metac_type_functiontype_t* result = (self->FunctionTypeTable.Slots + index);
    return result;
}

metac_type_enum_t* EnumTypePtr(metac_sema_state_t* self, uint32_t index)
{
    metac_type_enum_t* result = (self->EnumTypeTable.Slots + index);
    return result;
}


metac_type_tuple_t* TupleTypePtr(metac_sema_state_t* self, uint32_t index)
{
    metac_type_tuple_t* result = (self->TupleTypeTable.Slots + index);
    return result;
}

metac_scope_t* MetaCScope_PushNewScope(metac_sema_state_t* sema,
                                       metac_scope_t *parent,
                                       metac_scope_owner_t scopeOwner)
{
    metac_scope_t* result = AllocNewScope(sema, parent, scopeOwner);

    MetaCScopeTable_Init(&result->ScopeTable, &sema->Allocator);

    return result;
}

#define BREAK_ON_SERIAL(NODE, SERIAL) do { \
    if ((cast(metac_node_t)NODE)->Kind == node_exp_tuple) { \
        for(uint32_t i = 0; i < cast(expr_tuple_t)) \
    \ }

metac_sema_expr_t* AllocNewSemaExpr(metac_sema_state_t* self, metac_expr_t* expr)
{
    metac_sema_expr_t* result = 0;

    {
        metac_sema_expr_t exp = {(metac_expr_kind_t) expr->Kind};
        METAC_COPY_HEADER(expr, &exp);

        exp.TypeIndex.v = 0;
        exp.Serial = INC(_nodeCounter);
        ARENA_ARRAY_ADD(self->Exprs, exp);
        result = self->Exprs + self->ExprsCount - 1;
    }


    if (expr->Kind == expr_tuple)
    {
        const uint32_t tupleExpCount = expr->TupleExprCount;
        ARENA_ARRAY_ENSURE_SIZE(self->Exprs, tupleExpCount);

        uint32_t allocPos = POST_ADD(self->ExprsCount, tupleExpCount);
        metac_sema_expr_t* elements =
            self->Exprs + allocPos;
        metac_sema_expr_t** elemArray =
            Allocator_Calloc(&self->Allocator, metac_sema_expr_t*, tupleExpCount);
        expr_tuple_t* expList = expr->TupleExprList;

        metac_expr_t* elemExpr;
        for(uint32_t i = 0;
            i < tupleExpCount;
            i++)
        {
            elemExpr = expList->Expr;
            metac_sema_expr_t* semaElem = elements + i;
            semaElem->Serial = INC(_nodeCounter);
            METAC_COPY_HEADER(elemExpr, semaElem);

            memcpy(
                ((char*)semaElem) + sizeof(metac_sema_expr_header_t),
                ((char*)elemExpr) + sizeof(metac_expr_header_t),
                sizeof(metac_expr_t) - sizeof(metac_expr_header_t)
            );

            elemArray[i] = semaElem;
            expList = expList->Next;
        }
        result->TupleExprs = elemArray;
        result->TupleExprCount = tupleExpCount;
    }
    else
    {
        memcpy(
            ((char*)result) + sizeof(metac_sema_expr_header_t),
            ((char*)expr) + sizeof(metac_expr_header_t),
            sizeof(metac_expr_t) - sizeof(metac_expr_header_t)
        );
    }
#if 0
    if (result->Serial == 119)
    {
        asm ("int $3;");
    }
#endif
    return result;
}

metac_scope_t* AllocNewScope(metac_sema_state_t* self,
                             metac_scope_t* parent, metac_scope_owner_t owner)
{
    metac_scope_t scope_ = {(metac_scope_flags_t)0};
    metac_scope_t* result = 0;

    {
        scope_.Serial = INC(_nodeCounter);
        scope_.Owner = owner;
        scope_.Parent = parent;

        ARENA_ARRAY_ADD(self->Scopes, scope_);
        result = self->Scopes + self->ScopesCount - 1;
    }

    return result;
}


sema_decl_function_t* AllocNewSemaFunction(metac_sema_state_t* self,
                                           decl_function_t* declFunc)
{
    sema_decl_function_t* result = 0;
    sema_decl_function_t func = {(metac_decl_kind_t)0};

    {
        func.Serial = INC(_nodeCounter);
        func.TypeIndex.v = 0;
        ARENA_ARRAY_ADD(self->Functions, func);
        result = self->Functions + self->FunctionsCount - 1;
        (*(metac_node_header_t*) result) = (*(metac_node_header_t*) declFunc);
    }

    return result;
}

sema_decl_variable_t* AllocNewSemaVariable(metac_sema_state_t* self,
                                           decl_variable_t* declVar,
                                           metac_sema_decl_t** result_ptr)
{
    sema_decl_variable_t* result = 0;
    sema_decl_variable_t variable = {(metac_decl_kind_t)0};

    variable.Kind = decl_variable;
    variable.Serial = INC(_nodeCounter);

    result = self->Variables + INC(self->VariablesCount);
    (*result) = variable;
    (*result_ptr) = (metac_sema_decl_t*)result;

    return result;
}

sema_decl_variable_t* AllocFunctionParameters(metac_sema_state_t* self,
                                              sema_decl_function_t* func,
                                              uint32_t parameterCount)
{
    sema_decl_variable_t* result = 0;

    ARENA_ARRAY_ENSURE_SIZE(self->Variables,
                            self->VariablesCount + parameterCount);

    {
        result = self->Variables + POST_ADD(self->VariablesCount, parameterCount);
        for(uint32_t i = 0;
            i < parameterCount;
            i++)
        {
            (result + i)->Kind = decl_parameter;
            (result + i)->Serial = INC(_nodeCounter);
            (result + i)->Storage.v = STORAGE_V(storage_parameter, i);
        }

    }

    return result;
}
#if 0
metac_type_aggregate_field_t* AllocAggregateFields(metac_sema_state_t* self,
                                                   metac_type_aggregate_t* aggregate,
                                                   metac_decl_kind_t kind,
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
                POST_ADD(_newSemaStructFieldsCount, fieldCount);
            aggregateIndex = aggregate - _newSemaStructs_mem;
        } break;
        case decl_type_union:
        {
            REALLOC_BOILERPLATE(_newSemaUnionFields)
            result = _newSemaUnionFields_mem +
                POST_ADD(_newSemaUnionFieldsCount, fieldCount);
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

metac_sema_stmt_t* AllocNewSemaStmt_(metac_sema_state_t* self,
                                               metac_stmt_kind_t kind,
                                               size_t nodeSize, void** result_ptr)
{
    metac_sema_stmt_t* result = 0;
    metac_sema_stmt_t stmt = {(metac_stmt_kind_t)0};

    {
        // result->Parent = 0;
        stmt.Kind = kind;
        stmt.Serial = INC(_nodeCounter);
        // result->TypeIndex.v = 0;
        ARENA_ARRAY_ADD(self->Stmts, stmt);
    }
    result = self->Stmts + self->StmtsCount - 1;

    *result_ptr = result;

    return result;
}

#define AllocateArray(ALLOC, TYPE, COUNT) \
    (cast(TYPE*) calloc(sizeof(TYPE), (COUNT)))

sema_stmt_block_t* AllocNewSemaBlockStmt(metac_sema_state_t* self,
                                              sema_stmt_block_t* Parent, uint32_t stmtCount,
                                              void** result_ptr)
{
    sema_stmt_block_t* result = 0;
    sema_stmt_block_t stmt = {(metac_stmt_kind_t)0};

    {
        stmt.Kind = stmt_block;
        stmt.StmtCount = stmtCount;
        stmt.Serial = INC(_nodeCounter);

        ARENA_ARRAY_ADD(self->BlockStmts, stmt);
        result = self->BlockStmts + self->BlockStmtsCount - 1;
        ARENA_ARRAY_INIT_SZ(metac_sema_stmt_t*, result->Body, &self->Allocator, stmtCount);
    }
    (*result_ptr) = result;

    return result;
}

sema_stmt_casebody_t* AllocNewSemaCasebodyStmt(metac_sema_state_t* self,
                                                    uint32_t stmtCount,
                                                    void** result_ptr)
{
    sema_stmt_casebody_t* result;
    sema_stmt_casebody_t stmt = {(metac_stmt_kind_t)0};

    {
        result = cast(sema_stmt_casebody_t*)
            AllocNewSemaStmt(self, stmt_casebody, &result);

        ARENA_ARRAY_ADD(self->Stmts, *(metac_sema_stmt_t*)&stmt);
        result = (sema_stmt_casebody_t*) self->BlockStmts + self->BlockStmtsCount - 1;
        ARENA_ARRAY_INIT_SZ(metac_sema_stmt_t*, result->Stmts, &self->Allocator, stmtCount);

        result->StmtCount = stmtCount;
        result->Serial = INC(_nodeCounter);
    }
    (*result_ptr) = result;

    return result;
}



uint32_t BlockStmtIndex(metac_sema_state_t* self,
                             sema_stmt_block_t* blockstmt)
{
    return blockstmt - self->BlockStmts;
}

