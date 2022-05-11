#include "metac_type_table.h"

type_index_t GetOrAddArrayType(metac_type_table_t* table, type_index_t elementType, uint32_t dimension)
{
    assert(table->Kind == type_array);
    return (type_index_t){0};
}

