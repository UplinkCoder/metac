#include "metac_alloc.h"
#include "../parser/metac_identifier_table.h"
#include "os.h"
#include <assert.h>
#include <string.h>
#include <stdlib.h>
#include "../hash/crc32c.h"

#ifdef DEBUG_SERVER
#  include "../debug/debug_server.h"
#endif

#ifndef NDEBUG
#include "../debug/debug_server.h"
#endif

#ifndef ALIGN16
#  define ALIGN16(N) \
      (((N) + 15) & ~15)
#endif

#ifndef ALIGN_BLOCKSIZE
#  define ALIGN_BLOCKSIZE(N) \
      (((N) + (BLOCK_SIZE - 1)) & ~(BLOCK_SIZE - 1))
#endif

metac_identifier_table_t g_filenames = {0};
metac_alloc_t*           g_allocators = 0;
uint32_t                 g_allocatorCount = 0;
uint8_t                  g_memory[1536 * 1];
tagged_arena_t           g_arenas[4] = {
    {cast(void*)(cast(char*)&g_memory + (1536 * 0)), 0, sizeof(g_memory) / 1, 0},
    {0},
    {0},
    {0}
};

metac_alloc_t g_allocator = {g_arenas, 1, 4};

#ifdef NDEBUG
static const metac_identifier_ptr_t s_null_filename = {0};
#endif

inline metac_identifier_ptr_t Add_Filename(const char* file)
{
    if (g_filenames.Slots == 0)
        IdentifierTable_Init(&g_filenames, IDENTIFIER_LENGTH_SHIFT, 5, &g_allocator);

    const uint32_t len = cast(uint32_t) strlen(file);
    const uint32_t hash = crc32c_nozero(~0, file, len);
    const uint32_t key = IDENTIFIER_KEY(hash, len);

    return GetOrAddIdentifier(&g_filenames, key, file);
}

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
    allocator->ArenaCount = 16;
    allocator->Arenas = cast(tagged_arena_t*)
        calloc(sizeof(tagged_arena_t), allocator->ArenaCapacity);

    allocator->inuseArenaCount = 0;
    allocator->FileID = ADD_FILENAME(file);
    allocator->Line = line;

    uint32_t allocated = 0;
    void* firstBlock = 0;
    OS.PageAlloc(BLOCK_SIZE, &allocated, &firstBlock);

    uint32_t memoryPerArena = allocated / allocator->ArenaCount;

    for(uint32_t arenaIdx = 0;
        arenaIdx < allocator->ArenaCount;
        arenaIdx++)
    {
        tagged_arena_t* arena = &allocator->Arenas[arenaIdx];
        arena->Alloc = allocator;
        arena->FileID = allocator->FileID;
        arena->Line = line;
        arena->Memory = (cast(char*)firstBlock)
                      + (memoryPerArena * arenaIdx);
        arena->Offset = 0;
        arena->SizeLeft = memoryPerArena;
    }

    allocator->AllocatedBlocks = allocated / BLOCK_SIZE;
}

arena_ptr_t Allocator_AddArena(metac_alloc_t* allocator, tagged_arena_t* arena)
{
    arena_ptr_t result = {-1};

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

//    Debug_Allocation(g_DebugServer, allocator, size, file, line);


    assert(allocator != allocator->Parent);

    tagged_arena_t* arena = 0;

    size = ALIGN16(size) + ALIGN16(sizeof(tagged_arena_t));
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
            parentArenaPtr = Allocate_(allocator->Parent, ALIGN_BLOCKSIZE(size),
                                       file, line, true);
            if (parentArenaPtr.Index == -1)
                goto LAllocNewPage;
        }
        else // We don't have a parent :-( we need to ask the OS for a block
    LAllocNewPage:
        {
            uint32_t allocatedSize;

            OS.PageAlloc(size, &allocatedSize, cast(void**)&arena);
            ADD_PAGELIST(cast(void*)arena);

            arena->SizeLeft = allocatedSize;
            arena->Offset = 0;
            arena->Memory = cast(void*) arena;
            arena->Alloc = allocator;
            arena->Line = line;
            arena->FileID = ADD_FILENAME(file);
            goto LsetResult;
        }

        if (!forChild)
            result = Allocator_AddArena(allocator, arena);
    }

    return result;
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

    return arena->Memory;
}

/// After free Arena has been called
/// acessing the area pointer itself is invalid
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

