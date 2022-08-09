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
                                       metac_scope_owner_t scopeOwner)
{
    metac_scope_t* result = AllocNewScope(sema, parent, scopeOwner);

    MetaCScopeTable_Init(&result->ScopeTable, &sema->Allocator);

    return result;
}

#define BREAK_ON_SERIAL(NODE, SERIAL) do { \
    if ((cast(metac_node_t)NODE)->Kind == node_exp_tuple) { \
        for(uint32_t i = 0; i < cast(exp_tuple_t)) \
    \ }

metac_sema_expression_t* AllocNewSemaExpression(metac_semantic_state_t* self, metac_expression_t* expr)
{
    metac_sema_expression_t* result = 0;
    REALLOC_BOILERPLATE(self->Expressions);

    result = self->Expressions + INC(self->Expressions_size);

    {
        metac_sema_expression_t exp;
        METAC_COPY_HEADER(expr, &exp);

        exp.TypeIndex.v = 0;
        exp.Serial = INC(_nodeCounter);
        (*result) = exp;
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
        result->TupleExpressionCount = tupleExpCount;
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

metac_scope_t* AllocNewScope(metac_semantic_state_t* self,
                             metac_scope_t* parent, metac_scope_owner_t owner)
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

    result->Kind = decl_variable;
    result->Serial = INC(_nodeCounter);

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
            (result + i)->Kind = decl_parameter;
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
        result->Kind = kind;
        result->Serial = INC(_nodeCounter);
        // result->TypeIndex.v = 0;
    }

    *result_ptr = result;

    return result;
}

#define AllocateArray(ALLOC, TYPE, COUNT) \
    (cast(TYPE*) calloc(sizeof(TYPE), (COUNT)))

sema_stmt_block_t* AllocNewSemaBlockStatement(metac_semantic_state_t* self,
                                              sema_stmt_block_t* Parent, uint32_t statementCount,
                                              void** result_ptr)
{
    sema_stmt_block_t* result = 0;

    REALLOC_BOILERPLATE(self->BlockStatements)

    {
        result = self->BlockStatements + INC(self->BlockStatements_size);
        metac_sema_statement_t** body =
            AllocateArray(self->BS_Allocator, metac_sema_statement_t*, statementCount);
        result->Body = body;
        result->Kind = stmt_block;
        result->StatementCount = statementCount;
        result->Serial = INC(_nodeCounter);
    }
    (*result_ptr) = result;

    return result;
}

sema_stmt_casebody_t* AllocNewSemaCasebodyStatement(metac_semantic_state_t* self,
                                                    uint32_t statementCount,
                                                    void** result_ptr)
{
    sema_stmt_casebody_t* result;

    REALLOC_BOILERPLATE(self->Statements)

    {
        result = cast(sema_stmt_casebody_t*)
            AllocNewSemaStatement(self, stmt_casebody, &result);

        result->StatementCount = statementCount;
        result->Statements =
            AllocateArray(self->BS_Allocator, metac_sema_statement_t*, statementCount);
        result->Serial = INC(_nodeCounter);
    }
    (*result_ptr) = result;

    return result;
}



uint32_t BlockStatementIndex(metac_semantic_state_t* self,
                             sema_stmt_block_t* blockstmt)
{
    return blockstmt - self->BlockStatements;
}

