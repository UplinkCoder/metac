#include "../hash/crc32c.c"
#include "../parser/metac_identifier_table.c"
#include "../os/os.c"
#include "../os/metac_alloc.c"
#include "../os/metac_alloc.h"
#include <stdio.h>
#include <assert.h>
#include <string.h>

int main(int argc, const char* argv[])
{
    printf("Starting Allocator Tests...\n");

    metac_alloc_t mainAlloc;
    Allocator_Init(&mainAlloc, 0);
    printf("Main Allocator Initialized.\n");

    // 1. Basic Allocation Test
    printf("Test 1: Basic Allocation...\n");
    uint32_t allocSize = 1024;
    arena_ptr_t arenaPtr = AllocateArena(&mainAlloc, allocSize);
    assert(IsValidArenaPtr(arenaPtr));
    tagged_arena_t* arena = Allocator_IDX2PTR(&mainAlloc, arenaPtr.Index);
    assert(arena->SizeLeft == allocSize);
    assert(arena->Offset == 0);
    assert(arena->Flags & arena_flag_inUse);

    // 3. Calloc Test
    printf("Test 3: Calloc...\n");
    int* callocArray = Allocator_Calloc(&mainAlloc, int, 10);
    assert(callocArray != NULL);
    for(int i=0; i<10; i++) {
        assert(callocArray[i] == 0);
    }

    // 4. Realloc Test
    printf("Test 4: Realloc...\n");
    int* reallocArray = Allocator_Realloc(&mainAlloc, callocArray, int, 10);
    assert(reallocArray != NULL);
    reallocArray[0] = 42;
    reallocArray[9] = 99;

    int* largerArray = Allocator_Realloc(&mainAlloc, reallocArray, int, 20);
    assert(largerArray != NULL);
    assert(largerArray[0] == 42);
    assert(largerArray[9] == 99);
    largerArray[19] = 123;

    // 5. Arena Array Macro Test
    printf("Test 5: ARENA_ARRAY macros...\n");
    ARENA_ARRAY(int, myIntArray);
    assert(myIntArray == NULL);
    assert(myIntArrayCount == 0);

    ARENA_ARRAY_INIT_SZ(int, myIntArray, &mainAlloc, 5);
    assert(myIntArray != NULL);
    assert(myIntArrayCount == 0); // Count is 0 after init

    for(uint32_t i=0; i<5; i++) {
        ARENA_ARRAY_ADD(myIntArray, i * 10);
    }
    assert(myIntArrayCount == 5);
    assert(myIntArray[4] == 40);

    // Test ENSURE_SIZE
    printf("Test 5b: ARENA_ARRAY_ENSURE_SIZE...\n");
    ARENA_ARRAY_ENSURE_SIZE(myIntArray, 10);
    assert(myIntArrayCount == 5); // Count should still be 5
    // But now we can add more
    for(uint32_t i=5; i<10; i++) {
        ARENA_ARRAY_ADD(myIntArray, i * 10);
    }
    assert(myIntArrayCount == 10);
    assert(myIntArray[9] == 90);

    // 6. Hierarchical Allocator Test
    printf("Test 6: Hierarchical Allocator...\n");
    metac_alloc_t childAlloc;
    Allocator_Init(&childAlloc, &mainAlloc);

    // Allocate from child
    int* childAllocArray = Allocator_Calloc(&childAlloc, int, 5);
    assert(childAllocArray != NULL);
    childAllocArray[0] = 7;

    // 7. Test Arena Freeing (if supported)
    printf("Test 7: Arena Freeing...\n");
    arena_ptr_t arenaToFree = AllocateArena(&mainAlloc, 100);
    Allocator_FreeArena(&mainAlloc, arenaToFree);

    // If it was freed, we might be able to reuse it.
    arena_ptr_t reusedArena = AllocateArena(&mainAlloc, 50);
    assert(IsValidArenaPtr(reusedArena));

    printf("All Allocator Tests Passed!\n");
    return 0;
}
