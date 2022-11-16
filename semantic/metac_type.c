#include "metac_type.h"

const char* type_index_kind_toChars(metac_type_index_kind_t Kind)
{
    const char* result = 0;

#define CASE(KIND, VALUE) \
    case KIND: result = #KIND; break;

    switch(Kind)
    {
        FOREACH_TYPE_INDEX_KIND(CASE)
    }

    return result;
#undef CASE
}