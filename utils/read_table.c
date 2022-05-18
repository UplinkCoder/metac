#include "compat.h"
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include "cache/crc32.c"
#include "metac_identifier_table.h"
#include <unistd.h>
#include <alloca.h>

typedef identifier_table_file_header_t header_t;
typedef metac_identifier_table_slot_t slot_t;

#define LOG2(X) \
    (BSR(X) + 1)

#define NEXTPOW2(X) \
    (1 << LOG2(X))

#if defined(_MSC_VER)
    unsigned long BSR(uint32_t x)
    {
        unsigned long result;
        _BitScanReverse(&result, x);
        return result;
    }
#elif defined(__TINYC__)
#  include "3rd_party/bsr.c"
#else
#  define BSR(X) \
    (__builtin_clz(X) ^ 31)
#endif

void binPrint(uint64_t number)
{
    char format_buffer[66];
    {
        int i = 65;
        for(; i--;)
        {
            format_buffer[i] = '0';
            if (number)
            {
                format_buffer[i] = (number & 1 ? '1' : '0');
                number >>= 1;
                continue;
            }
            //break;
        }
        format_buffer[64] = '\n';
        write(1, format_buffer, 65);
    }

}

void PrintOccupyVector(uint64_t* vec, uint32_t tableSize)
{
    for(int i = 0; i < (tableSize / 64); i++)
    {
        binPrint(vec[i]);
    }
    write(1, "\n", 1);
}

uint32_t Occupy(uint64_t *bitVector, uint32_t hash, uint32_t tableSize)
{
    const uint32_t slotIndexMask = (tableSize - 1);
    const uint32_t initialSlotIndex = (hash & slotIndexMask);
    uint32_t displacement = 0;
    // TracyCPlot("TargetIndex", initialSlotIndex);
    for(
        uint32_t slotIndex = initialSlotIndex;
        (++slotIndex & slotIndexMask) != initialSlotIndex;
    )
    {
        uint32_t address =  ((slotIndex - 1) & slotIndexMask);
        uint32_t vec_idx = (address >> 6);
        uint32_t bitN      = (address & 63);
        // printf("a: %u, vi: %u bn: %u\n", address, vec_idx, bitN);
        if ( (bitVector[vec_idx] & (1 << bitN)) != 0 )
        {
            displacement++;
        }
        else
        {
            bitVector[vec_idx] |= (1 << bitN);
            //PrintOccupyVector(bitVector, slotIndexMask + 1);
            char fmt[512];
            //int sz = sprintf(fmt, "Setting arr[%u] bit %u\n", vec_idx, bitN);
            //write(1, fmt, sz);
            return displacement;
        }
    }
    return -1;
}


int main(int argc, char* argv[])
{
    if (argc != 2)
    {
        fprintf(stderr, "read_table requires one argument exactly, the name of the table\n");
        return -1;
    }

    FILE* fd = fopen(argv[1], "rb");
    fseek(fd, 0, SEEK_END);
    uint32_t sz = ftell(fd);
    fseek(fd, 0, SEEK_SET);

    void * TableMem = malloc(sz);
    uint32_t bytes_read = fread(TableMem, 1, sz, fd);

    const header_t header = *(header_t*) TableMem;
    const char* strings = TableMem + header.OffsetStrings;
    const slot_t* slots = TableMem + header.OffsetSlots;

    printf("NumberOfSlots: %u\n", header.NumberOfSlots);
    printf("SizeofSlot: %u\n", header.SizeofSlot);
    printf("OffsetSlots: %u\n", header.OffsetSlots);
    printf("OffsetStrings: %u\n", header.OffsetStrings);
    printf("StringMemorySize: %u\n", header.StringMemorySize);
    printf("Version: %u\n",header.Version);
    printf("HeaderCommentSize: %u\n", header.HeaderCommentSize);


    uint32_t tableSize = NEXTPOW2(header.NumberOfSlots);
    uint64_t* occupancyBitvector = malloc(tableSize >> 3);
    memset(occupancyBitvector, 0x00, tableSize >> 3);

    PrintOccupyVector(occupancyBitvector, tableSize);

    for(int i = 0;
        i < header.NumberOfSlots;
        i++)
    {
        slot_t slot = slots[i];
        uint32_t displacement = Occupy(occupancyBitvector, slot.HashKey, tableSize);
        uint32_t length = slot.HashKey >> header.LengthShift;
        printf("Hash:%x Data: %.*s | displacement:%d\n", slot.HashKey, length, strings + (slot.Ptr.v - 4), displacement);
        printf("StringPtr:%u StringPtrHash: %x\n", slot.Ptr.v, crc32c(~0, &slot.Ptr.v, sizeof(slot.Ptr.v)));
    }

    return 0;
}
