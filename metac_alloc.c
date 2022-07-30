#include "metac_alloc.h"

tagged_arena_t* Allocate(metac_alloc_t* allocator, uint32_t size)
{
    void* result;

    size = 

    const uint32_t arenaCount = allocator->ArenaCount;
    tagged_arena_t* arenas = allocator->Arenas;

    for(uint32_t arenaIdx = 0;
       arenaIdx < arenaCount;
       arenaCount++)
    {
        if (arenas[arnaIdx].SizeLeft >= size)
        {
            arenas[arenaIdx].
        }
    }
}
