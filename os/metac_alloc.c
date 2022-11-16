#include "metac_alloc.h"
#include "../parser/metac_identifier_table.h"
#include "os.h"
#include <assert.h>
#include <string.h>
#include <stdlib.h>
#include "../hash/crc32c.h"

#include "../debug/debug_server.h"


#ifndef ALIGN16
#  define ALIGN16(N) \
      (((N) + 15) & ~15)
#endif

#ifndef ALIGN_BLOCKSIZE
#  define ALIGN_BLOCKSIZE(N) \
      (((N) + (BLOCK_SIZE - 1)) & ~(BLOCK_SIZE - 1))
#endif

metac_alloc_t*           g_allocators = 0;
uint32_t                 g_allocatorCount = 0;

void Allocator_Init_(metac_alloc_t* allocator, metac_alloc_t* parent,
                     const char* file, uint32_t line)
{
#ifdef DEBUG_SERVER
    Debug_Allocator(g_DebugServer, allocator);
#endif
    allocator->Parent = parent;

    allocator->Freelist = 0;
    allocator->FreelistCount = 0;
    allocator->FreelistCapacity = 0;

    allocator->ArenaCapacity = 64;
    allocator->ArenaCount = 8;
    allocator->Arenas = cast(tagged_arena_t*)
        calloc(sizeof(tagged_arena_t), allocator->ArenaCapacity);

    allocator->inuseArenaCount = 0;
    allocator->File = file;
    allocator->Line = line;

    uint32_t allocated = 0;
    void* firstBlock = 0;
#if DEBUG_MEMORY
    allocator->ArenaCount = 0;
    allocator->AllocatedBlocks = 0;
#else
    OS.PageAlloc(BLOCK_SIZE, &allocated, &firstBlock);

    uint32_t memoryPerArena = allocated / allocator->ArenaCount;

    for(uint32_t arenaIdx = 0;
        arenaIdx < allocator->ArenaCount;
        arenaIdx++)
    {
        tagged_arena_t* arena = &allocator->Arenas[arenaIdx];
        arena->Alloc = allocator;
        arena->File = file;
        arena->Line = line;
#ifndef DEBUG_MEMORY
        arena->Memory = (cast(char*)firstBlock)
                      + (memoryPerArena * arenaIdx);
#else
        arena->Memory = cast(char*) malloc(memoryPerArena);
#endif
        arena->Offset = 0;
        arena->SizeLeft = memoryPerArena;
    }

    allocator->AllocatedBlocks = allocated / BLOCK_SIZE;
#endif
}

arena_ptr_t Allocator_AddArena(metac_alloc_t* allocator, tagged_arena_t* arena)
{
    arena_ptr_t result = {(uint32_t)-1};

    tagged_arena_t* target = 0;
    if (allocator->ArenaCount < (allocator->ArenaCapacity - allocator->inuseArenaCount))
    {
LaddArena:
        if (arena->Flags & arena_flag_inUse)
        {
            result.Index = (allocator->ArenaCapacity - allocator->inuseArenaCount++);
        }
        else
        {
            result.Index = allocator->ArenaCount++;
        }
        target = &allocator->Arenas[result.Index];

        (*target) = *arena;

    }
    else
    {
        uint32_t newArenaCapa = ALIGN16(allocator->ArenaCapacity + 1);
        uint32_t inUseArenaCount = allocator->inuseArenaCount;
        allocator->Arenas = cast(tagged_arena_t*)
            realloc(allocator->Arenas, sizeof(tagged_arena_t) * newArenaCapa);

        memcpy(allocator->Arenas + newArenaCapa - inUseArenaCount,
               allocator->Arenas + allocator->ArenaCapacity - inUseArenaCount,
               inUseArenaCount * sizeof(tagged_arena_t));

        allocator->ArenaCapacity = newArenaCapa;
        goto LaddArena;
    }

    return result;
}
#define ADD_PAGELIST(PAGE)
tagged_arena_t nullArena = {0};

arena_ptr_t Allocate_(metac_alloc_t* allocator, uint32_t size,
                      const char* file, uint32_t line, bool forChild)
{
    arena_ptr_t result = {-1};
    if (!size)
        return result;

#if DEBUG_MEMORY
    tagged_arena_t arena = {0};
    char* memory = (char*)malloc(size);

    arena.SizeLeft = size;
    arena.Offset = 0;
    arena.Memory = memory;

    return Allocator_AddArena(allocator, &arena);
#else
//    Debug_Allocation(g_DebugServer, allocator, size, file, line);

    assert(allocator != allocator->Parent);

    tagged_arena_t* arena = 0;

    size = ALIGN16(size);

    uint32_t arenaCount = allocator->ArenaCount;
    tagged_arena_t* arenas = allocator->Arenas;
LsearchArena:
    for(uint32_t arenaIdx = 0;
       arenaIdx < arenaCount;
       arenaIdx++)
    {
        tagged_arena_t canidate = arenas[arenaIdx];
        if (!(canidate.Flags & arena_flag_inUse))
        {
            if (canidate.SizeLeft >= size)
            {
                arena = &arenas[arenaIdx];
                break;
            }
        }
    }

LsetResult:
    if (arena)
    {
        tagged_arena_t newArena = {0};

        newArena.Memory = ((cast(uint8_t*)arena->Memory) + arena->Offset);
        newArena.SizeLeft = size;

        newArena.Alloc = allocator;
        newArena.Line = line;
        newArena.File = file;

        arena->Offset += size;
        arena->SizeLeft -= size;
        result = Allocator_AddArena(allocator, &newArena);

        if (arenas && arenas == allocator->Freelist)
        {
            uint32_t freelistIdx = arena - arenas;
            memmove(arena, arena + 1, --allocator->ArenaCount - freelistIdx);
        }
    }

    if (!arena)
    {
        // we couldn't find a sufficiently sized arena to alloc from
        // let's look in our freelist
        if (arenas != allocator->Freelist)
        {
            arenas = allocator->Freelist;
            arenaCount = allocator->FreelistCount;
            goto LsearchArena;
        }

        // let's try to allocate in our parent since we are going
        // to add this as a new arena we need to align this one to blockSize
        if (allocator->Parent)
        {
            arena_ptr_t parentArenaPtr;
            uint32_t allocated_size =  ALIGN_BLOCKSIZE(size);
            parentArenaPtr = Allocate_(allocator->Parent, allocated_size,
                                       file, line, true);
            if (parentArenaPtr.Index == -1)
            {
                goto LAllocNewPage;
            }
            else
            {
                arena = &allocator->Parent->Arenas[parentArenaPtr.Index];
            }
        }
        else // We don't have a parent :-( we need to ask the OS for a block
    LAllocNewPage:
        {
            uint32_t allocatedSize;
            void* memory;
            tagged_arena_t newArena = {0};

            OS.PageAlloc(size, &allocatedSize, &memory);
            ADD_PAGELIST(memory);

            newArena.SizeLeft = allocatedSize;
            newArena.Offset = 0;
            newArena.Memory = memory;
            newArena.Alloc = allocator;
            newArena.Line = line;
            newArena.File = file;
            return Allocator_AddArena(allocator, &newArena);
        }

        if (!forChild)
            result = Allocator_AddArena(allocator, arena);
    }

    return result;
#endif
}

void* Allocator_Calloc_(metac_alloc_t* alloc, uint32_t elemSize, uint32_t elemCount,
                        const char* file, uint32_t line)
{
    tagged_arena_t* arena = 0;
    arena_ptr_t arenaPtr =
        Allocate_(alloc, elemSize * elemCount, file, line, false);
    if (arenaPtr.Index == -1)
        return 0;

    arena = &alloc->Arenas[arenaPtr.Index];
    arena->Flags |= arena_flag_inUse;
    assert(arena->SizeLeft >= elemSize * elemCount);
    arena->Offset = elemSize * elemCount;
    arena->SizeLeft -= arena->Offset;

    memset(arena->Memory, 0, arena->Offset);

    return arena->Memory;
}

void Allocator_FreeArena (metac_alloc_t* alloc, arena_ptr_t arena)
{

}

arena_ptr_t ReallocArenaArray(tagged_arena_t* arena, metac_alloc_t* alloc, uint32_t elemSize,
                              const char* file, uint32_t line)
{
    uint32_t newCapa = ALIGN16(cast(uint32_t)(arena->Offset + (arena->Offset / 4)) + elemSize);
    tagged_arena_t* newArena;
    arena_ptr_t newArenaPtr = Allocate_(alloc, newCapa, file, line, false);

    if (newArenaPtr.Index == -1)
        return newArenaPtr;

    newArena = &alloc->Arenas[newArenaPtr.Index];
    memcpy(newArena->Memory, arena->Memory, arena->Offset);
    if (arena->Alloc)
    {
        metac_alloc_t* arenaAlloc = arena->Alloc;
        for(uint32_t i = 0; i < arenaAlloc->ArenaCount; i++)
        {
            if (&arenaAlloc->Arenas[i] == arena)
            {
                arena_ptr_t arenaPtr = {i};
                Allocator_FreeArena(arenaAlloc, arenaPtr);
                break;
            }
        }

    }
    (*arena) = *newArena;

    return newArenaPtr;
}

