/// our main allocator
#ifndef _METAC_ALLOC_H_
#define _METAC_ALLOC_H_

#include "compat.h"
#include "metac_identifier_table.h"


#ifndef KILOBYTES
#  define KILOBYTES(N) (N * 1024)
#endif

#define BLOCK_SIZE KILOBYTES(64)

extern metac_identifier_table_t;
static inline metac_identifier_ptr_t Add_Filename(const char* file);


typedef struct tagged_arena_t
{
    void* Memory;
    uint32_t Offset;
    uint32_t SizeLeft;

    struct metac_alloc_t* Alloc;
    metac_identifier_ptr_t FileID;
    uint32_t Line;

    uint32_t Flags;
} tagged_arena_t;

typedef struct metac_alloc_t
{
    tagged_arena_t* Arenas;
    uint32_t ArenaCount;
    uint32_t ArenaCapacity;

    uint32_t MaxAllocSize;
    uint32_t AllocatedBlocks;

    struct metac_alloc_t* Parent;

    metac_identifier_ptr_t FileID;
    uint32_t Line;

    tagged_arena_t* Freelist;
    uint32_t FreelistCount;
    uint32_t FreelistCapacity;

    uint8_t Padding[8];
} metac_alloc_t;

#endif

#ifdef NDEBUG
#  define ADD_FILENAME(FILE) {0}
#else
#  define ADD_FILENAME(FILE) \
    Add_Filename(FILE)
#endif

#define STACK_ARENA_ARRAY(TYPE, NAME, DIM, ALLOC) \
    TYPE _##NAME [DIM]; \
    uint32_t _##NAME##Count = 0; \
    metac_alloc_t* _##NAME##Alloc = (ALLOC); \
    bool _##NAME##FreeMemory = true; \
    tagged_arena_t _##NAME##Arena = { \
        cast(void*) _##NAME, 0, sizeof(_##NAME), \
        0, ADD_FILENAME(__FILE__), __LINE__ \
    }; \
    TYPE* NAME = _##NAME;

#define STACK_ARENA_ENSURE_SIZE(NAME, COUNT) do { \
    assert(_##NAME##Arena.Offset == 0); \
    uint32_t newCapa = (COUNT) * sizeof(*NAME); \
    if (_##NAME##Arena.SizeLeft < newCapa) \
    { \
       (_##NAME##Arena) = \
            *Allocate_(_##NAME##Alloc, newCapa, __FILE__, __LINE__, false); \
    } \
} while(0)

#define STACK_ARENA_ARRAY_ADD(NAME, VALUE) do { \
    if (_##NAME##Arena.SizeLeft < sizeof(*NAME)) \
    { \
        (*cast(void**)&NAME) = ReallocArenaArray( \
            &_##NAME##Arena, _##NAME##Alloc, \
            sizeof(*NAME)); \
    } \
    NAME[_##NAME##Count++] = (VALUE); \
} while(0)

#define STACK_ARENA_ARRAY_TO_HEAP(NAME) do { \
    _##NAME##FreeMemory = false; \
    if (_##NAME == NAME) { \
        uint32_t size = _##NAME##Count * sizeof(*NAME); \
        tagged_arena_t* newArena = \
            Allocate(_##NAME##Alloc, size); \
        memcpy(newArena->Memory, NAME, size); \
        (*cast(void**)&NAME) = newArena->Memory; \
    } \
} while(0)

#define Allocator_Init(ALLOC, PARENT) \
    Allocator_Init_((ALLOC), (PARENT), __FILE__, __LINE__)
void Allocator_Init_(metac_alloc_t* allocator, metac_alloc_t* parent,
                     const char* file, uint32_t line);

#define Allocate(ALLOC, SIZE) \
    Allocate_((ALLOC), (SIZE), __FILE__, __LINE__, false)
tagged_arena_t* Allocate_(metac_alloc_t* allocator, uint32_t size,
                          const char* file, uint32_t line, bool forChild);
