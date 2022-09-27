#include "../os/compat.h"

void _StackArrayRealloc(void** arrayPtr, uint32_t* arrayCapacityPtr,
                        const uint32_t elementSize)
{
    const uint32_t Capacity = *arrayCapacityPtr;

    (*arrayCapacityPtr) *= 2;
    void* newMem = calloc(elementSize, *arrayCapacityPtr);
    memcpy(newMem, *arrayPtr, Capacity * elementSize);
    (*(cast(void**)&arrayPtr)) = newMem;
}
