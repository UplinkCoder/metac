typedef struct variable_t
{
    metac_identifier_ptr_t Identifier;
    void* value;
} variable_t;

typedef struct variable_store_t
{
    variable_t* Variables;
    uint32_t  VariableSize;
    uint32_t VariableCapacity;
} variable_store_t;

void VariableStore_Init(variable_store_t* self);

metac_expression_t* evalWithVariables(metac_expression_t* e, variable_store_t* vars);
