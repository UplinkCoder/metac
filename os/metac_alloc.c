#include "metac_alloc.h"
#include "../parser/metac_identifier_table.h"
#include "os.h"
#include <assert.h>
#include <string.h>
#include <stdlib.h>
#include "../hash/crc32c.h"

#include "../debug/debug_server.h"

#ifndef U32
#define U32(VAR) \
    (*(uint32_t*)&VAR)
#endif

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
                     const char* file, uint32_t line, const char* allocName)
{
    allocator->Parent = parent;
    allocator->Name = ((allocName && allocName[0] == '&')
                    ? allocName + 1 : allocName);
    allocator->Freelist = 0;
    allocator->FreelistCount = 0;
    allocator->FreelistCapacity = 0;

    allocator->ArenasCapacity = 256;
    allocator->ArenasCount = 16;
    allocator->Arenas = cast(tagged_arena_t*)
        calloc(sizeof(tagged_arena_t), allocator->ArenasCapacity);

    allocator->inuseArenasCount = 0;
    allocator->File = file;
    allocator->Line = line;

    uint32_t allocated = 0;
    void* firstBlock = 0;
#if DEBUG_MEMORY
    allocator->ArenasCount = 0;
    allocator->AllocatedBlocks = 0;
#else
    if (allocator->ArenasCount)
    {
        OS.PageAlloc(BLOCK_SIZE, &allocated, &firstBlock);
        uint32_t memoryPerArena = allocated / allocator->ArenasCount;

        for(uint32_t arenaIdx = 0;
            arenaIdx < allocator->ArenasCount;
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
    }
#endif
#ifdef DEBUG_SERVER
    Debug_Allocator(g_DebugServer, allocator);
#endif
}

arena_ptr_t Allocator_AddArena(metac_alloc_t* allocator, tagged_arena_t* arena)
{
    arena_ptr_t result = {-1};

    tagged_arena_t* target = 0;
    if (allocator->ArenasCount < (allocator->ArenasCapacity - allocator->inuseArenasCount))
    {
LaddArena:
        if (arena->Flags & arena_flag_inUse)
        {
            // Place the in-use arena towards the end
            result.Index = (allocator->ArenasCapacity - ++allocator->inuseArenasCount);
        }
        else
        {
            // Place the non-in-use arena at the current count position
            result.Index = allocator->ArenasCount++;
        }

        assert(result.Index < allocator->ArenasCapacity);
        target = &allocator->Arenas[result.Index];

        (*target) = *arena;
    }
    else
    {
        uint32_t newArenaCapa = ALIGN16(allocator->ArenasCapacity + 1);
        uint32_t inUseArenasCount = allocator->inuseArenasCount;
        tagged_arena_t* newArenas;
        ALIGN_STACK();
        newArenas = cast(tagged_arena_t*)
            realloc(allocator->Arenas, sizeof(tagged_arena_t) * newArenaCapa);

        // Move in-use arenas from old end to new end of array
        memmove(newArenas + newArenaCapa - inUseArenasCount,
                newArenas + allocator->ArenasCapacity - inUseArenasCount,
                inUseArenasCount * sizeof(tagged_arena_t));

        // Clear the space between old in-use arenas and new in-use arenas
        memset(newArenas + allocator->ArenasCapacity - inUseArenasCount,
               0,
               (newArenaCapa - allocator->ArenasCapacity) * sizeof(tagged_arena_t));

        RESTORE_STACK();
        allocator->ArenasCapacity = newArenaCapa;
        allocator->Arenas = newArenas;
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
    arena.Flags |= arena_flag_inUse;

    return Allocator_AddArena(allocator, &arena);
#else
//    Debug_Allocation(g_DebugServer, allocator, size, file, line);

    assert(allocator != allocator->Parent);

    tagged_arena_t* arena = 0;

    size = ALIGN16(size);

    uint32_t ArenasCount = allocator->ArenasCount;
    tagged_arena_t* arenas = allocator->Arenas;
LsearchArena:
    for(uint32_t arenaIdx = 0;
       arenaIdx < ArenasCount;
       arenaIdx++)
    {
        tagged_arena_t candidate = arenas[arenaIdx];
        if (!(candidate.Flags & arena_flag_inUse))
        {
            // we will create a new arena from a reused one
            if (candidate.SizeLeft >= size)
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
        U32(newArena.Flags) |= arena_flag_inUse;

        arena->Offset += size;
        arena->SizeLeft -= size;
        assert((newArena.Flags & arena_flag_inUse) == arena_flag_inUse);
        newArena.MaxCapacity = newArena.Offset + newArena.SizeLeft;

        result = Allocator_AddArena(allocator, &newArena);

        if (arenas && arenas == allocator->Freelist)
        {
            uint32_t freelistIdx = arena - arenas;
            memmove(arena, arena + 1, --allocator->ArenasCount - freelistIdx);
        }
        goto Lreturn;
    }

    if (!arena)
    {
        // we couldn't find a sufficiently sized arena to alloc from
        // let's look in our freelist
        if (arenas != allocator->Freelist)
        {
            arenas = allocator->Freelist;
            ArenasCount = allocator->FreelistCount;
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
                goto LsetResult;
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
            newArena.MaxCapacity = allocatedSize;
            U32(newArena.Flags) |= arena_flag_inUse;
            assert((U32(newArena.Flags) & arena_flag_inUse) == arena_flag_inUse);
            result = Allocator_AddArena(allocator, &newArena);
            goto Lreturn;
        }

        if (!forChild)
        {
            result = Allocator_AddArena(allocator, arena);
            goto Lreturn;
        }
    }
Lreturn:
    {
        tagged_arena_t resultArena = allocator->Arenas[result.Index];
        assert(resultArena.Offset == 0);
        assert((resultArena.Flags & arena_flag_inUse) == arena_flag_inUse);
        assert(resultArena.Offset + resultArena.SizeLeft == resultArena.MaxCapacity);

        // we will now memset the memory so we can be sure we stomp data of someone holding
        // the memory at that point
        // TODO replace this 0x0 with an 0xf4 to see where we depend on arenas being zerod
        // I assume it's in the printer
        memset(resultArena.Memory, 0x0, resultArena.SizeLeft);
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

void* Allocator_Realloc_(metac_alloc_t* alloc, void* oldMem,
                         uint32_t elemSize, uint32_t elemCount,
                         const char* file, uint32_t line)
{
    tagged_arena_t* oldArena = 0;
    arena_ptr_t oldArenaPtr = {-1};

    size_t requestedSize = elemSize * elemCount;
    size_t spaceLeft;

    {
        uint32_t i;
        for(i = 0; i < alloc->inuseArenasCount; i++)
        {
            tagged_arena_t* candidate = &alloc->Arenas[alloc->ArenasCapacity - (i + 1)];
            if (candidate->Memory == oldMem)
            {
                oldArena = candidate;
                oldArenaPtr.Index = i;
                break;
            }
        }
    }

    {
        uint32_t i;
        for(i = 0; i < alloc->ArenasCount; i++)
        {
            tagged_arena_t* candidate = &alloc->Arenas[i];
            if (candidate->Memory == oldMem)
            {
                oldArena = candidate;
                oldArenaPtr.Index = i;
                break;
            }
        }
    }

    assert(oldArena != 0 || !"Wrong old memory location given");

    // Calculate the available space in the old arena
    spaceLeft = oldArena->Offset + oldArena->SizeLeft;

    // Check if we can shrink or stay the same size
    if (spaceLeft >= requestedSize) {
        oldArena->Offset = requestedSize;
        oldArena->SizeLeft = spaceLeft - requestedSize;
        return oldMem;
    }

    // lastly we have no choice but to allocate a new Arena
    else
    {
         arena_ptr_t arenaPtr =
            Allocate_(alloc, requestedSize, file, line, false);
        tagged_arena_t* arena = &alloc->Arenas[arenaPtr.Index];

        if (arenaPtr.Index == -1)
            return 0;

        assert(arena->Offset == 0 && arena->SizeLeft >= requestedSize);

        arena->Flags |= arena_flag_inUse;
        arena->Offset = requestedSize;
        arena->SizeLeft -= arena->Offset;
        memcpy(arena->Memory, oldArena->Memory, oldArena->Offset);
        Allocator_FreeArena(alloc, oldArenaPtr);

        return arena->Memory;
    }
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
    newArena->Offset = arena->Offset;
    newArena->SizeLeft -= arena->Offset;

    if (arena->Alloc)
    {
        metac_alloc_t* arenaAlloc = arena->Alloc;
        for(int32_t i = 0; i < arenaAlloc->ArenasCount; i++)
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

#undef U32
