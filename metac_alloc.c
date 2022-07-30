#include "metac_alloc.h"
#include "metac_identifier_table.h"
#include "os.h"
#include <assert.h>
#include <string.h>
#include <stdlib.h>
#include "crc32c.h"

#ifndef ALIGN16
#  define ALIGN16(N) \
      (((N) + 15) & ~15)
#endif

#ifndef ALIGN_BLOCKSIZE
#  define ALIGN_BLOCKSIZE(N) \
      (((N) + (BLOCK_SIZE - 1)) & ~(BLOCK_SIZE - 1))
#endif

metac_identifier_table_t g_filenames = {0};

static inline metac_identifier_ptr_t Add_Filename(const char* file)
{
    if (g_filenames.Slots == 0)
        IdentifierTable_Init(&g_filenames, IDENTIFIER_LENGTH_SHIFT, 7);

    const uint32_t len = cast(uint32_t) strlen(file);
    const uint32_t hash = crc32c_nozero(~0, file, len);
    const uint32_t key = IDENTIFIER_KEY(hash, len);

    return GetOrAddIdentifier(&g_filenames, key, file);
}

#ifdef NDEBUG
#  define ADD_FILENAME(FILE)
#else
#  define ADD_FILENAME(FILE) \
    Add_Filename(FILE)
#endif


void Allocator_Init_(metac_alloc_t* allocator, const char* file, uint32_t line)
{
    allocator->ArenaCapacity = 64;
    allocator->ArenaCount = 4;
    allocator->Arenas = cast(tagged_arena_t*)
        calloc(sizeof(tagged_arena_t), allocator->ArenaCapacity);

    allocator->FileID = ADD_FILENAME(file);
    allocator->Line = line;

    uint32_t allocated;
    void* firstBlock = OS.PageAlloc(BLOCK_SIZE, &allocated);
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

void Allocator_AddArena(metac_alloc_t* allocator, tagged_arena_t* arena)
{
    if (allocator->ArenaCount < allocator->ArenaCapacity)
    {
LaddArena:
        allocator->Arenas[allocator->ArenaCount++] = *arena;
    }
    else
    {
        uint32_t newArenaCapa = ALIGN16(allocator->ArenaCapacity + 1);
        allocator->Arenas = cast(tagged_arena_t*)
            realloc(allocator->Arenas, sizeof(tagged_arena_t) * newArenaCapa);
        allocator->ArenaCapacity = newArenaCapa;
        goto LaddArena;
    }

    return;
}
#define ADD_PAGELIST(PAGE)
tagged_arena_t nullArena = {0};

tagged_arena_t* Allocate_(metac_alloc_t* allocator, uint32_t size,
                          const char* file, uint32_t line, bool forChild)
{
    if (!size)
        return &nullArena;

    tagged_arena_t* result = 0;
    tagged_arena_t* arena = 0;

    size = ALIGN16(size) + ALIGN16(sizeof(tagged_arena_t));
    uint32_t arenaCount = allocator->ArenaCount;
    tagged_arena_t* arenas = allocator->Arenas;
LsearchArena:
    for(uint32_t arenaIdx = 0;
       arenaIdx < arenaCount;
       arenaIdx++)
    {
        if (arenas[arenaIdx].SizeLeft >= size)
        {
            arena = &arenas[arenaIdx];
            break;
        }
    }

LsetResult:
    if (arena)
    {
        result = cast(tagged_arena_t*) (arena->Memory + arena->Offset);

        result->Memory = arena->Memory + arena->Offset
                       + ALIGN16(sizeof(tagged_arena_t));
        result->SizeLeft = size - ALIGN16(sizeof(tagged_arena_t));
        result->Alloc = arena->Alloc;
        result->Line = line;

        arena->Offset += size;
        arena->SizeLeft -= size;

        if (arenas == allocator->Freelist)
        {
            uint32_t freelistIdx = arena - arenas;
            memmove(arena, arena + 1, --allocator->ArenaCount - freelistIdx);
            Allocator_AddArena(allocator, arena);
        }
    }

    if (!arena)
    {
        // we couldn't find a sufficienlty sized arena to alloc from
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
            arena = Allocate_(allocator->Parent, ALIGN_BLOCKSIZE(size),
                              file, line, true);

        }
        else // We don't have a parent :-( we need to ask the OS for a block
        {
            uint32_t allocatedSize;

            arena = cast(tagged_arena_t*) OS.PageAlloc(size, &allocatedSize);
            ADD_PAGELIST(cast(void)arena);
            arena->SizeLeft = allocatedSize;
            arena->Offset = ALIGN16(sizeof(tagged_arena_t));
            arena->Alloc = allocator;
            arena->Line = line;
            assert(arena);
        }
        if (!forChild)
            Allocator_AddArena(allocator, arena);

        goto LsetResult;
    }

    return result;
}

void FreeArena (tagged_arena_t* arena)
{
    metac_alloc_t* alloc = arena->Alloc;

}
