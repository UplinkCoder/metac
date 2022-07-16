#include "compat.h"
#include <stdlib.h>

#define DEF_STACK_ARRAY(TYPE, NAME, STACK_DIM) \
    TYPE NAME[STACK_DIM]; \
    uint32_t NAME ## Count = 0; \
    uint32_t NAME ## Capacity = ARRAY_SIZE(NAME);
/*
noinline void _newMemRealloc(void** memP, uint32_t* capacityP, const uint32_t elementSize)
{
    uint32_t capacity;
    if (!*memP)
    {
        capacity = cast(int)(8192 / 1.6f);
    }
    else
    {
        capacity = *capacityP;
    }

    {
        capacity = ALIGN4(cast(uint32_t) ((capacity - 1) * 1.6f));
        *memP = realloc(*memP, ((capacity) * elementSize));
    }

    *capacityP = capacity;
}
*/

void _StackArrayRealloc(void** arrayPtr, uint32_t* arrayCapacityPtr,
                        const uint32_t elementSize);


#define ADD_STACK_ARRAY(NAME, VALUE) \
if (NAME ## Count >= NAME ## Capacity) \
{ \
    if (ARRAY_SIZE(NAME) == NAME ## Capacity) \
        _StackArrayRealloc((cast(void**)&NAME), &NAME ## Capacity, sizeof(NAME[0])); \
    else \
        _newMemRealloc((cast(void**)&NAME), &NAME ## Capacity, sizeof(NAME[0])); \
} \
NAME[NAME ## Count++] = VALUE;
