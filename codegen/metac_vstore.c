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
        i < vstore->VariableSize;
        i++)
    {
        variable_t var = vstore->Variables[i];
        if (var.IdentifierPtr.v == vstoreId.v)
        {
            return (BCValue*) var.value;
        }
    }

    return 0;
}

void VariableStore_AddVariable(variable_store_t* vstore,
                               sema_decl_variable_t* varDecl,
                               void* value)
{
    metac_identifier_ptr_t vstoreId = GetVStoreID(vstore, varDecl);
    assert(vstoreId.v == 0);
    vstoreId = AddVStoreID(vstore, varDecl);

    BCValue* v = GetValueFromVariableStore(vstore, vstoreId);
    assert(!v);
    assert(vstore->VariableCapacity > vstore->VariableSize);
    variable_t var = { vstoreId, value };
    vstore->Variables[vstore->VariableSize++] = var;
}

void VariableStore_RemoveVariable(variable_store_t* vstore, void* value)
{
    bool foundVar = false;
    for(uint32_t i = 0; i < vstore->VariableSize; i++)
    {
        if (foundVar)
        {
            vstore->Variables[i - 1] = vstore->Variables[i];
        }
        else if (vstore->Variables[i].value == value)
        {
            foundVar = true;
        }
    }
    assert(foundVar);
    --vstore->VariableSize;
}


void VariableStore_SetValueI32(variable_store_t* vstore,
                               metac_sema_expr_t* varExp,
                               int32_t value)
{
    assert(varExp->Kind == exp_variable);

    metac_identifier_ptr_t vstoreId = GetVStoreID(vstore, varExp->Variable);
    BCValue* v = GetValueFromVariableStore(vstore, vstoreId);
    if (!v)
    {
        v = (BCValue*)malloc(sizeof(BCValue));
        variable_t var = { vstoreId, v };
        vstore->Variables[vstore->VariableSize++] = var;
    }
    *v = imm32(value);
}

void VariableStore_Init(variable_store_t* self, metac_identifier_table_t* externalTable, metac_alloc_t* allocator)
{
    self->VariableCapacity = 32;
    self->VariableSize = 0;
    self->Variables = Allocator_Calloc(allocator, variable_t, self->VariableCapacity);
    self->ExternalTable = externalTable;

    IdentifierTable_Init(&self->Table, IDENTIFIER_LENGTH_SHIFT, 7, allocator);
}
