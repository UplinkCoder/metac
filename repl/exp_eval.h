#include "../metac_identifier_table.h"

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

metac_expression_t* evalWithVariables(metac_expression_t* e, variable_store_t* vars);


typedef struct ReadI32_Ctx
{
    variable_store_t* vstore;
    metac_expression_t* exp;
} ReadI32_Ctx;

extern ReadI32_Ctx* _ReadContexts;
extern uint32_t _ReadContextSize;
extern uint32_t _ReadContextCapacity;
