/// our main allocator
#ifndef _METAC_ALLOC_H_
#define _METAC_ALLOC_H_

#include "compat.h"
#include "metac_arena.h"
#include "../parser/metac_identifier_table.h"

#ifndef KILOBYTES
#  define KILOBYTES(N) (N * 1024)
#endif

#define BLOCK_SIZE KILOBYTES(64)
extern metac_identifier_table_t g_filenames;

static inline metac_identifier_ptr_t Add_Filename(const char* file);

#define Allocator_IDX2PTR(ALLOC, IDX) \
    ((IDX) < 0 ? \
        &((ALLOC)->Arenas[(ALLOC)->ArenasCapacity + (IDX)]) : \
     (IDX) > 0 ? \
        &((ALLOC)->Arenas[(IDX) - 1]) : \
        (cast(tagged_arena_t*)0))

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
    uint32_t MaxCapacity;
} tagged_arena_t;

typedef struct metac_alloc_t
{
    tagged_arena_t* Arenas;
    int32_t ArenasCount;
    int32_t ArenasCapacity;

    int32_t inuseArenasCount;
    int32_t AllocatedBlocks;

    struct metac_alloc_t* Parent;

    const char* File;
    int32_t Line;

    tagged_arena_t* Freelist;
    int32_t FreelistCount;
    int32_t FreelistCapacity;

    const char* Name;
    int32_t Unprot;
} metac_alloc_t;


extern metac_alloc_t g_allocator;

arena_ptr_t ReallocArenaArray(const tagged_arena_t * arena, metac_alloc_t* alloc, uint32_t elemSize,
                              const char* file, uint32_t line);

#endif

#define IsValidArenaPtr(ARENA_PTR) (ARENA_PTR.Index != 0)

#define ARENA_ARRAY(TYPE, NAME) \
    TYPE* NAME; \
    uint32_t NAME##Count; \
    metac_alloc_t* NAME##Alloc; \
    arena_ptr_t NAME##ArenaPtr;

#define ARENA_ARRAY_INIT_SZ(TYPE, NAME, ALLOC, COUNT) do { \
        (NAME) = cast(TYPE*)0; \
        (NAME##Count) = 0; \
        (NAME##Alloc) = (ALLOC); \
        (NAME##ArenaPtr) = AllocateArena(ALLOC, (sizeof(TYPE) * (COUNT))); \
        tagged_arena_t* arena = Allocator_IDX2PTR((NAME##Alloc), (NAME##ArenaPtr).Index); \
        arena->Flags |= arena_flag_inUse; \
        (NAME) = cast(TYPE*) arena->Memory; \
} while(0)

#define ARENA_ARRAY_INIT(TYPE, NAME, ALLOC) \
    ARENA_ARRAY_INIT_SZ(TYPE, NAME, ALLOC, 16)


#define STACK_ARENA_ARRAY(TYPE, NAME, DIM, ALLOC) \
    TYPE NAME##Stack [DIM]; \
    uint32_t NAME##Count = 0; \
    metac_alloc_t* NAME##Alloc = (ALLOC); \
    bool NAME##FreeMemory = true; \
    arena_ptr_t NAME##ArenaPtr = {-1}; \
    tagged_arena_t* NAME##Arena = &(tagged_arena_t) { \
        cast(void*) NAME##Stack, 0, sizeof(NAME##Stack), \
        0, __FILE__, __LINE__, 0, sizeof(NAME##Stack) \
    }; \
    TYPE* NAME = NAME##Stack;

#define ARENA_ARRAY_REFCOPY(SRC, DST) do { \
    (DST) = (SRC); \
    (DST##Count) = (SRC##Count); \
    (DST##Alloc) = (SRC##Alloc); \
    (DST##ArenaPtr) = (SRC##ArenaPtr); \
} while (0)

#define ARENA_ARRAY_ENSURE_SIZE(NAME, COUNT) do { \
    /* assert(arena.Offset == 0); */ \
    uint32_t newCapa = (COUNT) * sizeof(*NAME); \
    tagged_arena_t* arena = Allocator_IDX2PTR((NAME##Alloc), (NAME##ArenaPtr).Index); \
    \
    if (arena->SizeLeft < newCapa) \
    { \
        arena_ptr_t arenaPtr = \
            Allocate_((NAME##Alloc), newCapa, __FILE__, __LINE__, false); \
        (NAME##ArenaPtr) = arenaPtr; \
        arena = Allocator_IDX2PTR((NAME##Alloc), (NAME##ArenaPtr).Index); \
        NAME = arena->Memory; \
    } \
} while(0)

// #define ARENA_ARRAY_CAPACITY(NAME) \
//    (arena.SizeLeft / sizeof(*NAME))

#define ARENA_ARRAY_ADD(NAME, VALUE) do { \
    tagged_arena_t* arena = Allocator_IDX2PTR((NAME##Alloc), (NAME##ArenaPtr).Index); \
    if (arena->SizeLeft < sizeof(*NAME)) \
    { \
        NAME##ArenaPtr = ReallocArenaArray( \
            arena, (NAME##Alloc), \
            sizeof(*(NAME)), __FILE__, __LINE__); \
        arena = Allocator_IDX2PTR((NAME##Alloc), (NAME##ArenaPtr).Index); \
        NAME = arena->Memory; \
    } \
    NAME[NAME##Count++] = (VALUE); \
    arena->SizeLeft -= sizeof(*NAME); \
    arena->Offset   += sizeof(*NAME); \
} while(0)

#define ARENA_ARRAY_ADD_N(NAME, PTR, COUNT) do { \
    size_t size_n = sizeof(*NAME) * (COUNT); \
    tagged_arena_t* arena = \
        Allocator_IDX2PTR((NAME##Alloc), (NAME##ArenaPtr).Index); \
    if (arena->SizeLeft < size_n) \
    { \
        NAME##ArenaPtr = ReallocArenaArray( \
            arena, (NAME##Alloc), \
            size_n, __FILE__, __LINE__); \
        arena = Allocator_IDX2PTR((NAME##Alloc), (NAME##ArenaPtr).Index); \
        NAME = arena->Memory; \
    } \
    memcpy((NAME) + (NAME##Count), PTR, size_n); \
    NAME##Count += (COUNT); \
    arena->SizeLeft -= size_n; \
    arena->Offset   += size_n; \
} while(0)


#define STACK_ARENA_ARRAY_TO_HEAP(NAME, ALLOC) do { \
    NAME##FreeMemory = false; \
    if (NAME##Stack == (NAME) && (NAME##Count) != 0) { \
        uint32_t size = (NAME##Count) * sizeof(*(NAME)); \
        tagged_arena_t* newArena = 0; \
        arena_ptr_t newArenaPtr = \
            AllocateArena((ALLOC), size); \
        newArena = Allocator_IDX2PTR((ALLOC), (newArenaPtr).Index); \
        memcpy(newArena->Memory, (NAME), size); \
        NAME = newArena->Memory; \
    } \
} while(0)

/*
    Make sure that a stack backed array can take COUNT more elements.

    The stack buffer has a fixed size.  If the elements do not fit into
    it, the contents are moved to the heap and the array continues
    there.  This has to be used instead of ARENA_ARRAY_ENSURE_SIZE for
    stack backed arrays: the latter consults an arena pointer which
    does not exist for a stack buffer and would therefore happily write
    past the end of the stack buffer.
*/
#define STACK_ARENA_ARRAY_ENSURE_SIZE(NAME, COUNT) do { \
    if (NAME##Stack == (NAME)) \
    { \
        /* still on the stack: does everything fit into the buffer? */ \
        if (((NAME##Count) + (COUNT)) * sizeof(*(NAME)) > sizeof(NAME##Stack)) \
        { \
            uint32_t _size = (NAME##Count) * sizeof(*(NAME)); \
            uint32_t _heapSize = _size + ((COUNT) * sizeof(*(NAME))); \
            arena_ptr_t _newArenaPtr = \
                AllocateArena((NAME##Alloc), _heapSize); \
            tagged_arena_t* _newArena = \
                Allocator_IDX2PTR((NAME##Alloc), _newArenaPtr.Index); \
            memcpy(_newArena->Memory, (NAME), _size); \
            _newArena->Offset = _size; \
            _newArena->SizeLeft = _heapSize - _size; \
            _newArena->Flags |= arena_flag_inUse; \
            (NAME) = cast(METAC_TYPEOF(*(NAME))*) _newArena->Memory; \
            (NAME##ArenaPtr) = _newArenaPtr; \
        } \
    } \
    else \
    { \
        /* already on the heap: make sure the arena has room */ \
        tagged_arena_t* _arena = \
            Allocator_IDX2PTR((NAME##Alloc), (NAME##ArenaPtr).Index); \
        if (_arena->SizeLeft < ((COUNT) * sizeof(*(NAME)))) \
        { \
            (NAME##ArenaPtr) = ReallocArenaArray( \
                _arena, (NAME##Alloc), sizeof(*(NAME)), __FILE__, __LINE__); \
            (NAME) = cast(METAC_TYPEOF(*(NAME))*) \
                Allocator_IDX2PTR((NAME##Alloc), (NAME##ArenaPtr).Index)->Memory; \
        } \
    } \
} while(0)

#define ARENA_ARRAY_FREE(NAME) do { \
        if ((NAME##ArenaPtr).Index != 0) \
            Allocator_FreeArena((NAME##Alloc), NAME##ArenaPtr); \
} while(0)


/*
    ------------------------------------------------------------------------
    The pointer stable array.
    ------------------------------------------------------------------------

    ARENA_ARRAY above grows by *copying* its contents into a freshly
    allocated block.  Copying invalidates every pointer into the old
    block, which silently corrupts trees that hold such pointers.

    The macros below use metac_arena_array_t instead: a chunked array
    whose elements never move.  A pointer to an element stays valid for
    the lifetime of the array.

    The price is that the elements are no longer contiguous, so they
    have to be addressed with ARENA_CHUNK_ARRAY_AT() instead of NAME[i].

    Use the contiguous ARENA_ARRAY for byte buffers and string tables
    which are handed out as a single contiguous block, and the chunked
    variant for everything that hands out pointers to its elements.
*/

/*
    Address of the element with the given index.  The result is an
    lvalue of the element type, so it can be assigned to and have its
    members accessed.
*/
#define ARENA_CHUNK_ARRAY_AT(NAME, INDEX) \
    (*(METAC_TYPEOF(*(NAME##_ElementPtr))*) ArenaArray_At(&(NAME), (INDEX)))

/*
    Address of the element with the given index.  Unlike
    ARENA_CHUNK_ARRAY_AT this yields a pointer, not an lvalue.
*/
#define ARENA_CHUNK_ARRAY_PTR(NAME, INDEX) \
    ((METAC_TYPEOF(*(NAME##_ElementPtr))*) ArenaArray_At(&(NAME), (INDEX)))

#define ARENA_CHUNK_ARRAY(TYPE, NAME) \
    metac_arena_array_t NAME; \
    TYPE* NAME##_ElementPtr; \
    uint32_t NAME##Count; \
    metac_alloc_t* NAME##Alloc;

#define ARENA_CHUNK_ARRAY_INIT_SZ(TYPE, NAME, ALLOC, COUNT) do { \
        ArenaArray_Init(&(NAME), (ALLOC), sizeof(TYPE), \
                        METAC_ARENA_ARRAY_ALIGNMENT, (COUNT)); \
        (NAME##Count) = 0; \
        (NAME##Alloc) = (ALLOC); \
        (NAME##_ElementPtr) = cast(TYPE*)0; \
} while(0)

#define ARENA_CHUNK_ARRAY_INIT(TYPE, NAME, ALLOC) \
    ARENA_CHUNK_ARRAY_INIT_SZ(TYPE, NAME, ALLOC, METAC_ARENA_ARRAY_MIN_CHUNK)

/*
    Reserve a contiguous run of COUNT elements at the end of the array.
    The address of the run is stored in NAME##_ElementPtr and the
    element count is advanced by COUNT.
*/
#define ARENA_CHUNK_ARRAY_RESERVE(NAME, COUNT) do { \
    (NAME##_ElementPtr) = ArenaArray_Reserve(&(NAME), (COUNT)); \
    (NAME##Count) += (COUNT); \
} while(0)

/*
    Make sure that COUNT more elements fit into the array without
    having to allocate a new chunk.  The elements are appended one by
    one afterwards, the element count is *not* changed here.
*/
#define ARENA_CHUNK_ARRAY_ENSURE_SIZE(NAME, COUNT) do { \
    ArenaArray_EnsureRoom(&(NAME), (COUNT)); \
} while(0)

/*
    Append one element.  The element type is taken from the declaration
    of the array, so the assignment keeps the usual C conversion rules.
*/
#define ARENA_CHUNK_ARRAY_ADD(NAME, VALUE) do { \
    (NAME##_ElementPtr) = ArenaArray_AppendSlot(&(NAME)); \
    *((NAME##_ElementPtr)) = (VALUE); \
    (NAME##Count)++; \
} while(0)

#define ARENA_CHUNK_ARRAY_ADD_N(NAME, PTR, COUNT) do { \
    ArenaArray_AddN(&(NAME), (PTR), (COUNT)); \
    (NAME##Count) += (COUNT); \
} while(0)

#define ARENA_CHUNK_ARRAY_FREE(NAME) do { \
    ArenaArray_Free(&(NAME)); \
} while(0)

/*
    A chunked array whose first chunk lives in a stack buffer.  Once the
    stack buffer is exhausted the array continues on the heap.
*/
#define STACK_CHUNK_ARRAY(TYPE, NAME, DIM, ALLOC) \
    TYPE NAME##Stack[DIM]; \
    uint32_t NAME##Count = 0; \
    metac_alloc_t* NAME##Alloc = (ALLOC); \
    metac_arena_array_chunk_t NAME##Chunk = { \
        0, cast(void*)0, cast(uint8_t*)NAME##Stack, 0, 0, (uint32_t)(DIM), \
        (uint32_t)sizeof(TYPE), true \
    }; \
    metac_arena_array_t NAME = { \
        &NAME##Chunk, &NAME##Chunk, &NAME##Chunk, \
        (uint32_t)sizeof(TYPE), METAC_ARENA_ARRAY_ALIGNMENT, \
        (uint32_t)(DIM), 1, 0, (ALLOC) \
    }; \
    TYPE* NAME##_ElementPtr = cast(TYPE*)0;

/*
    Move the contents of the stack chunk to the heap so that they
    outlive the stack frame of the enclosing function.
*/
#define STACK_CHUNK_ARRAY_TO_HEAP(NAME, ALLOC) \
    ArenaArray_DetachStack(&(NAME))



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
