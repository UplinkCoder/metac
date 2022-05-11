#include "metac_parsetree.h"
#include "metac_type_table.h"

typedef struct metac_semantic_state_t
{
    metac_type_table_t* TypeTable;
} metac_semantic_state_t;


void RegisterType(metac_semantic_state_t* state, decl_type_t* type);
