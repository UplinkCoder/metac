#ifndef  _METAC_VSTORE_H_
#define  _METAC_VSTORE_H_

#include "../parser/metac_identifier_table.h"

typedef struct variable_t
{
    metac_identifier_ptr_t IdentifierPtr;
    void* value;
} variable_t;

typedef struct variable_store_t
{
    variable_t* Variables;
    uint32_t  VariableSize;
    uint32_t VariableCapacity;

    metac_identifier_table_t* ExternalTable;
    metac_identifier_table_t Table;
} variable_store_t;
/*
void WalkTree(void* c, BCValue* result,
              metac_sema_expression_t* e,
              variable_store_t* vstore);

*/
void VariableStore_Init(variable_store_t* self, metac_identifier_table_t* externalTable, metac_alloc_t* allocator);

metac_sema_expression_t evalWithVariables(metac_sema_expression_t* e,
                                          variable_store_t* vars);

metac_identifier_ptr_t FindMatchingIdentifier(metac_identifier_table_t* searchTable,
                                              metac_identifier_table_t* sourceTable,
                                              metac_identifier_ptr_t sourcePtr,
                                              bool addIfNotFound);

void VariableStore_AddVariable(variable_store_t* vstore,
                               sema_decl_variable_t* varDecl,
                               void* value);

void VariableStore_RemoveVariable(variable_store_t* vstore, void* value);


void VariableStore_SetValueI32(variable_store_t* vstore,
                               metac_sema_expression_t* varExp,
                               int32_t value);

metac_identifier_ptr_t GetVStoreID(variable_store_t* vstore,
                                   sema_decl_variable_t* varDecl);


metac_identifier_ptr_t AddVStoreID(variable_store_t* vstore,
                                   sema_decl_variable_t* varDecl);

#endif // _METAC_VSTORE_H_
