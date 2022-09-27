#include "../os/compat.h"
#include <stdlib.h>

#define DEF_STACK_ARRAY(TYPE, NAME, STACK_DIM) \
    TYPE _##NAME[STACK_DIM] = {}; \
    TYPE ## _array NAME = { _##NAME, 0, STACK_DIM };


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
if (NAME.Count >= NAME.Capacity) \
{ \
    if (ARRAY_SIZE(_##NAME) == NAME.Capacity) \
        _StackArrayRealloc((cast(void**)&NAME.Ptr), &NAME.Capacity, sizeof(_##NAME[0])); \
    else \
        _newMemRealloc((cast(void**)&NAME.Ptr), &NAME.Capacity, sizeof(_##NAME[0])); \
} \
NAME.Ptr[NAME.Count++] = VALUE;

#define PERSIST_STACK_ARRAY(NAME) \
    if (NAME.Count < ARRAY_SIZE(_##NAME)) \
    { \
        void* ptr = malloc(sizeof(_##NAME[0]) * NAME.Count); \
        memcpy(ptr, NAME.Ptr, sizeof(_##NAME[0]) * NAME.Count); \
        (*cast(void**)&NAME.Ptr) = ptr; \
        NAME.Capacity = NAME.Count; \
    }

#ifndef _METAC_ARRAY_H_
#define _METAC_ARRAY_H_
typedef struct metac_identifier_ptr_t_array
{
    metac_identifier_ptr_t* Ptr;
    uint32_t Count;
    uint32_t Capacity;
} metac_identifier_ptr_t_array;

typedef struct metac_token_t_array
{
    metac_token_t* Ptr;
    uint32_t Count;
    uint32_t Capacity;
} metac_token_t_array;

typedef struct metac_token_t_array_array
{
    metac_token_t_array* Ptr;
    uint32_t Count;
    uint32_t Capacity;
} metac_token_t_array_array;


#endif
