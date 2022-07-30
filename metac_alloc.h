/// our main allocator

#ifndef KILOBYTES
#  define KILOBYTES(N) (N * 1024)
#endif

#define BLOCK_SIZE KILOBYTES(64)

struct tagged_arena_t
{
    void* Memory;
    uint32_t Count;
    uint32_t Capacity;

    struct metac_alloc_t* Alloc;
    uint32_t FileID;
    uint32_t Line;
} tagged_arena_t;

typedef struct metac_alloc_t
{
    tagged_arena_t Arenas[];
    uint32_t ArenaCount;
    uint32_t ArenaCapacity;

    uint32_t MaxBlockSize;
    uint32_t AllocatedBlocks;

    metac_alloc_t* Parent;

    uint32_t FileID;
    uint32_t Line;

    tagged_arena_t Freelist[];
    uint32_t FreelistCount;
    uint32_t FreelistCapacity;

    uint8_t Padding[8];
} metac_alloc_t;

typedef struct metac_tree16_t
{
    (struct metac_tree_16_t*)[16] Children;

    tagged_arena_t Arena;
} meta_tree_t;
