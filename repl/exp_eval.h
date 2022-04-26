#include "../metac_identifier_table.h"

typedef struct stored_declaration_t
{
    metac_declaration_t* Declaration;
    union
    {
        uint32_t FunctionIndex;
        uint16_t frameOffset;
    };

} stored_declaration_t;

typedef struct declation_store_t
{
    stored_declaration_t* Declarations;
    uint32_t DeclarationSize;
    uint32_t DeclarationCapacity;

    uint32_t NextFunctionIndex;

    metac_identifier_table_t Table;
} declaration_store_t;

void DeclarationStore_Init(declaration_store_t* self);
metac_declaration_t* DeclarationStore_GetDecl(declaration_store_t* dstore,
                                              metac_identifier_ptr_t dStoreId);
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

    metac_identifier_table_t Table;
} variable_store_t;

void VariableStore_Init(variable_store_t* self);

metac_expression_t evalWithVariables(metac_expression_t* e,
                                     variable_store_t* vars,
                                     declaration_store_t* dstore);

typedef struct ReadI32_Ctx
{
    variable_store_t* vstore;
    metac_expression_t* exp;
} ReadI32_Ctx;

extern ReadI32_Ctx* _ReadContexts;
extern uint32_t _ReadContextSize;
extern uint32_t _ReadContextCapacity;
