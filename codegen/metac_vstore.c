#include "metac_vstore.h"

metac_identifier_ptr_t FindMatchingIdentifier(metac_identifier_table_t* searchTable,
                                              metac_identifier_table_t* sourceTable,
                                              metac_identifier_ptr_t sourcePtr,
                                              bool addIfNotFound)
{
    const char* idChars = IdentifierPtrToCharPtr(sourceTable, sourcePtr);
    uint32_t idLength = strlen(idChars);
    uint32_t idHash = crc32c_nozero(~0, idChars, idLength);
    uint32_t idKey = IDENTIFIER_KEY(idHash, idLength);

    metac_identifier_ptr_t result = IsIdentifierInTable(searchTable, idKey, idChars);

    if (!result.v && addIfNotFound)
    {
        result = GetOrAddIdentifier(searchTable, idKey, idChars);
    }

    return result;
}

metac_identifier_ptr_t AddVStoreID(variable_store_t* vstore,
                                   sema_decl_variable_t* var)
{
    metac_identifier_ptr_t vstoreId =
        FindMatchingIdentifier(&vstore->Table,
                               vstore->ExternalTable,
                               var->VarIdentifier, true);
    return vstoreId;
}

metac_identifier_ptr_t GetVStoreID(variable_store_t* vstore,
                                   sema_decl_variable_t* var)
{
    metac_identifier_ptr_t vstoreId =
        FindMatchingIdentifier(&vstore->Table,
                               vstore->ExternalTable,
                               var->VarIdentifier, false);
    return vstoreId;
}

static inline BCValue* GetValueFromVariableStore(variable_store_t* vstore,
                                                 metac_identifier_ptr_t vstoreId)
{
    for(uint32_t i = 0;
        i < vstore->VariablesCount;
        i++)
    {
        metac_vstore_variable_t var = vstore->Variables[i];
        if (var.IdentifierPtr.v == vstoreId.v)
        {
            return (BCValue*) var.Value;
        }
    }

    return 0;
}

void VariableStore_AddVariable(variable_store_t* vstore,
                               sema_decl_variable_t* varDecl,
                               void* value)
{
    metac_identifier_ptr_t vstoreId = GetVStoreID(vstore, varDecl);
    metac_vstore_variable_t var = {{0}};
    assert(vstoreId.v == 0);
    vstoreId = AddVStoreID(vstore, varDecl);
    var.IdentifierPtr = vstoreId;
    var.Value = value;
    BCValue* v = GetValueFromVariableStore(vstore, vstoreId);
    assert(!v);

    ARENA_ARRAY_ADD(vstore->Variables, var);
}

void VariableStore_RemoveVariable(variable_store_t* vstore, void* value)
{
    bool foundVar = false;
    for(uint32_t i = 0; i < vstore->VariablesCount; i++)
    {
        if (foundVar)
        {
            vstore->Variables[i - 1] = vstore->Variables[i];
        }
        else if (vstore->Variables[i].Value == value)
        {
            foundVar = true;
        }
    }
    assert(foundVar);
    --vstore->VariablesCount;
}


void VariableStore_SetValueI32(variable_store_t* vstore,
                               metac_sema_expr_t* varExp,
                               int32_t value)
{
    assert(varExp->Kind == expr_variable);

    metac_identifier_ptr_t vstoreId = GetVStoreID(vstore, varExp->Variable);
    BCValue* v = GetValueFromVariableStore(vstore, vstoreId);
    if (!v)
    {
        metac_vstore_variable_t var = { vstoreId, v };
        v = (BCValue*)malloc(sizeof(BCValue));
        vstore->Variables[vstore->VariablesCount++] = var;
    }
    *v = imm32(value);
}

void VariableStore_Init(variable_store_t* self, metac_identifier_table_t* externalTable, metac_alloc_t* allocator)
{
    ARENA_ARRAY_INIT_SZ(metac_vstore_variable_t, self->Variables, allocator, 32);
    self->ExternalTable = externalTable;

    IdentifierTable_Init(&self->Table, IDENTIFIER_LENGTH_SHIFT, 7, allocator);
}
