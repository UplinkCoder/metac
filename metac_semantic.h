#include "metac_parsetree.h"
#include "metac_type_table.h"

typedef struct metac_semantic_state_t
{
    metac_identifier_table_t* IdentifierTable;

    // metac_type_table_t* TypeTable;
    METAC_TYPE_TABLE_T(array) ArrayTypeTable;
    METAC_TYPE_TABLE_T(struct) StructTypeTable;
} metac_semantic_state_t;


void RegisterType(metac_semantic_state_t* state, decl_type_t* type);
const char* TypeToChars(metac_semantic_state_t* self, metac_type_index_t typeIndex);
void MetaCSemantic_Init(metac_semantic_state_t* self);
