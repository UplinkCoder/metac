/// our main allocator
#ifndef _METAC_ALLOC_H_
#define _METAC_ALLOC_H_

#include "compat.h"
#include "../parser/metac_identifier_table.h"

#include "valgrind_helper.c"

#ifndef KILOBYTES
#  define KILOBYTES(N) (N * 1024)
#endif

#define BLOCK_SIZE KILOBYTES(64)
extern metac_identifier_table_t g_filenames;

static inline metac_identifier_ptr_t Add_Filename(const char* file);

typedef struct arena_ptr_t
{
    int32_t Index;
} arena_ptr_t;

enum arena_flags_t
{
    arena_flag_none = 0,
    arena_flag_inUse = (1 << 0)
};

typedef struct tagged_arena_t
{
    void* Memory;
    uint32_t Offset;
    uint32_t SizeLeft;

    struct metac_alloc_t* Alloc;
    const char* File;
    uint32_t Line;

    uint32_t Flags;
} tagged_arena_t;

typedef struct metac_alloc_t
{
    tagged_arena_t* Arenas;
    uint32_t ArenasCount;
    uint32_t ArenasCapacity;

    uint32_t inuseArenasCount;
    uint32_t AllocatedBlocks;

    struct metac_alloc_t* Parent;

    const char* File;
    uint32_t Line;

    tagged_arena_t* Freelist;
    uint32_t FreelistCount;
    uint32_t FreelistCapacity;

    const char* Name;
} metac_alloc_t;


extern metac_alloc_t g_allocator;

arena_ptr_t ReallocArenaArray(tagged_arena_t* arena, metac_alloc_t* alloc, uint32_t elemSize,
                              const char* file, uint32_t line);

#endif

#define IsValidArenaPtr(ARENA_PTR) (ARENA_PTR.Index != 1)

#define ARENA_ARRAY(TYPE, NAME) \
    TYPE* NAME; \
    uint32_t NAME##Count; \
    metac_alloc_t* NAME##Alloc; \
    tagged_arena_t NAME##Arena; \
    arena_ptr_t NAME##ArenaPtr;

#define ARENA_ARRAY_INIT_SZ(TYPE, NAME, ALLOC, COUNT) \
    { \
        (NAME) = cast(TYPE*)0; \
        (NAME##Count) = 0; \
        (NAME##Alloc) = (ALLOC); \
        (NAME##ArenaPtr) = AllocateArena(ALLOC, (sizeof(TYPE) * (COUNT))); \
        (NAME##Arena) = (ALLOC)->Arenas[(NAME##ArenaPtr).Index]; \
        (NAME) = cast(TYPE*) (NAME##Arena).Memory; \
    } while(0)

#define ARENA_ARRAY_INIT(TYPE, NAME, ALLOC) \
    ARENA_ARRAY_INIT_SZ(TYPE, NAME, ALLOC, 16)


#define STACK_ARENA_ARRAY(TYPE, NAME, DIM, ALLOC) \
    TYPE NAME##Stack [DIM]; \
    uint32_t NAME##Count = 0; \
    metac_alloc_t* NAME##Alloc = (ALLOC); \
    bool NAME##FreeMemory = true; \
    arena_ptr_t NAME##ArenaPtr = {-1}; \
    tagged_arena_t NAME##Arena = { \
        cast(void*) NAME##Stack, 0, sizeof(NAME##Stack), \
        0, __FILE__, __LINE__ \
    }; \
    TYPE* NAME = NAME##Stack;

#define ARENA_ARRAY_REFCOPY(SRC, DST) do { \
    (DST) = (SRC); \
    (DST##Count) = (SRC##Count); \
    (DST##Alloc) = (SRC##Alloc); \
    (DST##Arena) = (SRC##Arena); \
    (DST##ArenaPtr) = (SRC##ArenaPtr); \
} while (0)

#define ARENA_ARRAY_ENSURE_SIZE(NAME, COUNT) do { \
    /* assert((NAME##Arena).Offset == 0); */ \
    uint32_t newCapa = (COUNT) * sizeof(*NAME); \
    if ((NAME##Arena).SizeLeft < newCapa) \
    { \
        arena_ptr_t arena = \
            Allocate_((NAME##Alloc), newCapa, __FILE__, __LINE__, false); \
        (NAME##Arena) = ((NAME##Alloc)->Arenas[arena.Index]); \
        (*cast(void**)(&NAME)) = NAME##Arena.Memory; \
    } \
} while(0)

#define ARENA_ARRAY_CAPACITY(NAME) \
    (NAME##Arena.SizeLeft / sizeof(*NAME))

#define ARENA_ARRAY_ADD(NAME, VALUE) do { \
    if (NAME##Arena.SizeLeft < sizeof(*NAME)) \
    { \
        NAME##ArenaPtr = ReallocArenaArray( \
            &(NAME##Arena), (NAME##Alloc), \
            sizeof(*(NAME)), __FILE__, __LINE__); \
        (NAME##Arena) = ((NAME##Alloc)->Arenas[(NAME##ArenaPtr).Index]); \
        *(cast(void**)&NAME) = (NAME##Arena).Memory; \
    } \
    mark_memory_defined(NAME, (NAME##Count + 1) * sizeof(*NAME)); \
    NAME[NAME##Count++] = (VALUE); \
    NAME##Arena.SizeLeft -= sizeof(*NAME); \
    NAME##Arena.Offset   += sizeof(*NAME); \
    mark_memory_undefined(NAME, NAME##Count * sizeof(*NAME)); \
} while(0)

#define ARENA_ARRAY_ADD_N(NAME, PTR, COUNT) do { \
    size_t size_n = sizeof(*NAME) * (COUNT); \
    if (NAME##Arena.SizeLeft < size_n) \
    { \
        NAME##ArenaPtr = ReallocArenaArray( \
            &(NAME##Arena), (NAME##Alloc), \
            size_n, __FILE__, __LINE__); \
        (NAME##Arena) = ((NAME##Alloc)->Arenas[(NAME##ArenaPtr).Index]); \
        *(cast(void**)&NAME) = (NAME##Arena).Memory; \
    } \
    mark_memory_defined(NAME, ((NAME##Count) * sizeof(*NAME)) + size_n); \
    memcpy((NAME) + (NAME##Count), PTR, size_n); \
    NAME##Count += (COUNT); \
    NAME##Arena.SizeLeft -= size_n; \
    NAME##Arena.Offset   += size_n; \
    mark_memory_undefined(NAME, ((NAME##Count) * sizeof(*NAME)) + size_n); \
} while(0)


#define STACK_ARENA_ARRAY_TO_HEAP(NAME, ALLOC) do { \
    NAME##FreeMemory = false; \
    if (NAME##Stack == (NAME) && (NAME##Count) != 0) { \
        uint32_t size = (NAME##Count) * sizeof(*(NAME)); \
        tagged_arena_t* newArena = 0; \
        arena_ptr_t newArenaPtr = \
            AllocateArena((ALLOC), size); \
        newArena = &((ALLOC)->Arenas[newArenaPtr.Index]); \
        memcpy(newArena->Memory, (NAME), size); \
        (*cast(void**)&(NAME)) = newArena->Memory; \
    } \
} while(0)

#define ARENA_ARRAY_FREE(NAME) do { \
        if ((NAME##ArenaPtr).Index != -1) \
            Allocator_FreeArena((NAME##Alloc), NAME##ArenaPtr); \
} while(0)



#define ARENA_ARRAY_INDEX(NAME, IDX) \
    mark_memory_defined_if_adressable()

#define Allocator_Init(ALLOC, PARENT, ...) \
    Allocator_Init_((ALLOC), (PARENT), __FILE__, __LINE__, #ALLOC)
void Allocator_Init_(metac_alloc_t* allocator, metac_alloc_t* parent,
                     const char* file, uint32_t line, const char* allocName);

#define Allocator_Calloc(ALLOC, TYPE, ELEM_COUNT) \
    (cast(TYPE*)Allocator_Calloc_((ALLOC), sizeof(TYPE), (ELEM_COUNT), __FILE__, __LINE__))
void* Allocator_Calloc_(metac_alloc_t* alloc, uint32_t elemSize, uint32_t elemCount,
                        const char* file, uint32_t line);

#define Allocator_Realloc(ALLOC, OLD_MEM, TYPE, ELEM_COUNT) \
    (cast(TYPE*)Allocator_Realloc_((ALLOC), (OLD_MEM), sizeof(TYPE), (ELEM_COUNT), __FILE__, __LINE__))
void* Allocator_Realloc_(metac_alloc_t* alloc, void* oldMem,
                         uint32_t elemSize, uint32_t elemCount,
                         const char* file, uint32_t line);

#define AllocateArena(ALLOC, SIZE) \
    Allocate_((ALLOC), (SIZE), __FILE__, __LINE__, false)
arena_ptr_t  Allocate_(metac_alloc_t* allocator, uint32_t size,
                       const char* file, uint32_t line, bool forChild);

void Allocator_FreeArena (metac_alloc_t* alloc, arena_ptr_t arena);
