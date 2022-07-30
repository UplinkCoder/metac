/// our main allocator
#include "compat.h"
#include "metac_identifier_table.h"

#ifndef KILOBYTES
#  define KILOBYTES(N) (N * 1024)
#endif

#define BLOCK_SIZE KILOBYTES(64)

typedef struct tagged_arena_t
{
    void* Memory;
    uint32_t Offset;
    uint32_t SizeLeft;

    struct metac_alloc_t* Alloc;
    metac_identifier_ptr_t FileID;
    uint32_t Line;
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

typedef struct metac_tree16_t
{
    struct metac_tree_16_t* Children[16];

    tagged_arena_t Arena;
} meta_tree_t;


#define Allocator_Init(ALLOC, PARENT) \
    Allocator_Init_((ALLOC), (PARENT), __FILE__, __LINE__)
void Allocator_Init_(metac_alloc_t* allocator, metac_alloc_t* parent,
                     const char* file, uint32_t line);

#define Allocate(ALLOC, SIZE) \
    Allocate_((ALLOC), (SIZE), __FILE__, __LINE__, false)
tagged_arena_t* Allocate_(metac_alloc_t* allocator, uint32_t size,
                          const char* file, uint32_t line, bool forChild);
