#include "metac_type.h"

const char* type_index_kind_toChars(metac_type_index_kind_t kind)
{
    const char* result = 0;

#define CASE(KIND, VALUE) \
    case KIND: result = #KIND; break;

    switch(kind)
    {
        FOREACH_TYPE_INDEX_KIND(CASE)
    }

    return result;
#undef CASE
}

bool TypeIndex_isSigned(metac_type_index_t typeIndex)
{
    assert(TYPE_INDEX_KIND(typeIndex) == type_index_basic);
    switch(TYPE_INDEX_INDEX(typeIndex))
    {
        case type_char:
        case type_short:
        case type_int:
        case type_long:
        case type_long_long:
            return true;
    }

    return false;
}
