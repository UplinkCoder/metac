#include "crc32c.c"
#include "metac_identifier_table.c"
#include "os.c"
#include "metac_alloc.c"

int main(int argc, const char* argv[])
{
    void* mem = 0;
    metac_alloc_t mainAlloc;
    Allocator_Init(&mainAlloc);
    printf("Allocator_File: %s\n",
        IdentifierPtrToCharPtr(&g_filenames, mainAlloc.FileID));
    printf("Allocator.AllocatedBlocks: %u\n",
        mainAlloc.AllocatedBlocks);
    // printf("Allocator.Used: %u\n");

    tagged_arena_t* firstA = &mainAlloc.Arenas[0];
    printf("firstA.Memory: %p\n", firstA->Memory);
    printf("firstA.SizeLeft: %d\n", firstA->SizeLeft);
    printf("firstA.Offset: %d\n", firstA->Offset);


    for(uint32_t i = 0; i < 5; i++)
    {
        uint32_t allocSize = 13 * i;
        printf("Allocate(%u)\n", allocSize);
        tagged_arena_t* arena = Allocate(&mainAlloc, allocSize);

        printf("Allocation.Memory: %p\n", arena->Memory);
        printf("Allocation.SizeLeft: %d\n", arena->SizeLeft);
        printf("Allocation.Offset: %d\n", arena->Offset);

        printf("firstA.Memory: %p\n", firstA->Memory);
        printf("firstA.SizeLeft: %d\n", firstA->SizeLeft);
        printf("firstA.Offset: %d\n", firstA->Offset);
    }
}
