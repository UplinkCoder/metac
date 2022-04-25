#ifndef _IDENTIFIER_TABLE_C_
#define _IDENTIFIER_TABLE_C_

#include "compat.h"
#include <assert.h>
#include <stdlib.h>
#include "metac_lexer.h"
#include "metac_identifier_table.h"
#include <string.h>
#include <stdio.h>

#ifdef _MSC_VER
#include <intrin.h>
#endif

#include "3rd_party/tracy/TracyC.h"

#ifdef TRACY_ENABLE
#  define TRACY_COUNTER(COUNTER) static uint32_t COUNTER
#else
#  define TRACY_COUNTER(COUNTER)
#endif

#if defined(_MSC_VER) || defined (__TINYC__)
#else
#  define memcpy __builtin_memcpy
#  define memcmp __builtin_memcmp
#endif
const metac_identifier_ptr_t empty_identifier = {~0u};

static inline bool IsFilled(metac_identifier_table_slot_t slot)
{
    return slot.HashKey != 0;
}

const char* IdentifierPtrToCharPtr(metac_identifier_table_t* table,
                                   metac_identifier_ptr_t ptr)
{
    assert(ptr.v);
    return table->StringMemory + (ptr.v - 4);
}

void IdentifierTableInit(metac_identifier_table_t* table)
{
    table->SlotCount_Log2 = 13;
    const uint32_t maxSlots = (1 << table->SlotCount_Log2);
    table->Slots = (metac_identifier_table_slot_t*) calloc(maxSlots, sizeof(metac_identifier_table_slot_t));
    table->StringMemory = (char*)malloc(32768 * 8);
    table->StringMemoryCapacity = 32768 * 8;
    table->StringMemorySize = 0;
    table->SlotsUsed = 0;
    table->MaxDisplacement = 0;
}

#define ALIGN4(N) (((N) + 3) & ~3)
metac_identifier_ptr_t GetOrAddIdentifier(metac_identifier_table_t* table,
                                          uint32_t identifierKey,
                                          const char* identifier, uint32_t length)
{
    TracyCZone(ctx, true);

    metac_identifier_ptr_t result = {0};
    metac_identifier_ptr_t insertPtr;
    const uint32_t slotIndexMask = ((1 << table->SlotCount_Log2) - 1);
    const uint32_t initialSlotIndex = (identifierKey & slotIndexMask);
    // TracyCPlot("TargetIndex", initialSlotIndex);
    uint32_t displacement = 0;
    for(
        uint32_t slotIndex = initialSlotIndex;
        (++slotIndex & slotIndexMask) != initialSlotIndex;
    )
    {
        metac_identifier_table_slot_t* slot = &table->Slots[(slotIndex - 1) & slotIndexMask];
        const char* stringEntry;

        if (slot->HashKey == identifierKey)
        {
            stringEntry = IdentifierPtrToCharPtr(table, slot->Ptr);
            if (memcmp(identifier, stringEntry, length) == 0)
            {
                TRACY_COUNTER(Hits);

                TracyCPlot("Hits", ++Hits);

                result = slot->Ptr;
#ifdef REFCOUNT
                slot->RefCount++;
#endif
                break;
            }
            else
            {
                TRACY_COUNTER(collisions);
#ifdef TRACY_ENABLE
                static char msgBuffer[256];
                uint32_t msgLength = 0;
                msgLength = snprintf(msgBuffer, sizeof(msgBuffer), "'%s' collided with '%.*s'", stringEntry, length, identifier);
                TracyCMessage(msgBuffer, msgLength);
                TracyCPlot("Collisions", ++collisions);
#endif

            }
        }
        else if (slot->HashKey == 0)
        {
            uint32_t expected;
            uint32_t newValue;
            // TracyCPlot("MaxDisplacement", table->MaxDisplacement);
            TracyCPlot("Displacement", displacement);
            do {
                assert(table->StringMemorySize + ALIGN4(length + 1)
                       < table->StringMemoryCapacity);
                result.v = (table->StringMemorySize + 4);
                // Compare xchange here
                expected = table->StringMemorySize;
                newValue = (result.v - 4) + ALIGN4(length + 1);
#ifndef ATOMIC
                table->StringMemorySize = newValue;
            } while (false);
            ++table->SlotsUsed;
            TracyCPlot("LoadFactor", (float)table->SlotsUsed / (float)slotIndexMask);
            TracyCPlot("StringMemorySize", table->StringMemorySize);


#else
            } while(!__atomic_compare_exchange(&table->StringMemorySize, &expected, &newValue,
                false, __ATOMIC_ACQUIRE, __ATOMIC_ACQUIRE));
            // atomic compare exchange has been done.
            __atomic_add_fetch(&table->SlotsUsed, 1, __ATOMIC_ACQUIRE)
#endif
            char* tableMem = (table->StringMemory + (result.v - 4));
            memcpy(tableMem, identifier, length);
            tableMem[length] = '\0';
            static uint32_t misses = 0;
            TracyCPlot("Misses", misses++);
            slot->HashKey = identifierKey;
            slot->Ptr = result;
#ifdef REFCOUNT
            slot->RefCount++;
            slot->Displacement = displacement;
#endif
            break;
        }
#if 0
        else
        {
            const uint32_t TargetSlot = (slot->HashKey & slotIndexMask);
            int targetDisplacement = (slotIndex - TargetSlot);
            if (++displacement > targetDisplacement)
            {
                uint32_t nextInsertKey = slot->HashKey;
                metac_identifier_ptr_t nextInsertPtr = slot->Ptr;
                slot->HashKey = identifierKey;
                slot->Ptr = insertPtr;

                insertPtr = nextInsertPtr;
                identifierKey = nextInsertKey;
            }
        }
 #endif
            //if (++displacement > table->MaxDisplacement)
            //{
            //    table->MaxDisplacement = displacement;
            //}
            TracyCPlot("MaxDisplacement", table->MaxDisplacement);
            TracyCPlot("LoadFactor", (float)table->SlotsUsed / (float)slotIndexMask);
            TracyCPlot("StringMemorySize", table->StringMemorySize);
            displacement++;
        continue;
    }
    TracyCZoneEnd(ctx);

    return result;
}

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
#else
#  define BSR(X) \
    (__builtin_clz(X) ^ 31)
#endif

#define slot_t metac_identifier_table_slot_t

void InsertSlot(slot_t* slots, slot_t slot, const uint32_t slotIndexMask)
{
    const uint32_t initialSlotIndex = (slot.HashKey & slotIndexMask);
    // TracyCPlot("TargetIndex", initialSlotIndex);
    for(
        uint32_t slotIndex = initialSlotIndex;
        (++slotIndex & slotIndexMask) != initialSlotIndex;
    )
    {
        slot_t* dstSlot = slots + ((slotIndex - 1) & slotIndexMask);
        if (dstSlot->HashKey == 0)
        {
            *dstSlot = slot;
            break;
        }
    }
}

bool IsInTable(const metac_identifier_table_t* table,
               uint32_t key, metac_identifier_ptr_t value)
{
    const uint32_t slotIndexMask = ((1 << table->SlotCount_Log2) - 1);
    const uint32_t initialSlotIndex = (key & slotIndexMask);
    // TracyCPlot("TargetIndex", initialSlotIndex);
    for(
        uint32_t slotIndex = initialSlotIndex;
        (++slotIndex & slotIndexMask) != initialSlotIndex;
    )
    {
        metac_identifier_table_slot_t slot =
            table->Slots[(slotIndex - 1) & slotIndexMask];

        if (slot.HashKey == 0)
            return false;
        if (slot.HashKey == key && slot.Ptr.v == value.v)
            return true;
    }
    assert(0);
}

#define CRT_SECURE_NO_WARNINGS
metac_identifier_table_t ReadTable(const char* filename)
{
    metac_identifier_table_t result = {0};

    identifier_table_file_header_t header;
    FILE* fd = fopen(filename, "rb");

    fread(&header, 1, sizeof(header), fd);
    if (header.HeaderCommentSize)
    {
        char* comment = (char*)malloc(header.HeaderCommentSize);
        fread(&comment, 1, header.HeaderCommentSize, fd);
        printf("Comment: %.*s\n", header.HeaderCommentSize, comment);
        free((void*)comment);
    }

    uint32_t stringMemoryCapacity = ((header.StringMemorySize + 64) & ~63);

    result.StringMemoryCapacity = stringMemoryCapacity;
    result.StringMemorySize = header.StringMemorySize;
    result.SlotCount_Log2 = LOG2(header.NumberOfSlots);

    result.Slots = cast(slot_t*)
                    calloc((1 << result.SlotCount_Log2),
                           sizeof(metac_identifier_table_slot_t));

    uint32_t slotMemorySize = ( (sizeof(slot_t) > header.SizeofSlot)
                                ? sizeof (slot_t) : header.SizeofSlot );

    const uint32_t slotIndexMask = ((1 << result.SlotCount_Log2) - 1);

    slot_t* readSlot = (slot_t*)alloca(slotMemorySize);
    memset(readSlot, 0, slotMemorySize);

    for(int i = 0; i < header.NumberOfSlots; i++)
    {
        fread(readSlot, 1, header.SizeofSlot, fd);
        InsertSlot(result.Slots, *readSlot, slotIndexMask);
    }
    uint32_t soff = ftell(fd);
    assert(soff == header.OffsetStrings);

    result.StringMemory = (char*)malloc(result.StringMemoryCapacity);
    fread(result.StringMemory, 1, result.StringMemorySize, fd);

    uint32_t stringSizeLeft = result.StringMemorySize;
    uint32_t stringsProcessed = 0;

    for(char* prevNewline = result.StringMemory;
        stringSizeLeft > 0;)
    {
        char *nextNewline = (char*) memchr(prevNewline, '\n', stringSizeLeft);
        if (nextNewline == 0)
            break;

        (*nextNewline) = '\0';
        stringSizeLeft -= (nextNewline - prevNewline);
        prevNewline = nextNewline;
        stringsProcessed++;
    }

    assert(stringsProcessed == header.NumberOfSlots);
    result.SlotsUsed = header.NumberOfSlots;
    return result;
#undef slot_t
}

metac_identifier_table_slot_t* findFirstEntry(metac_identifier_table_t* table)
{
    for(metac_identifier_table_slot_t* slotP = table->Slots;
        slotP < table->Slots + (1 << table->SlotCount_Log2);
        slotP++)
    {
        if (slotP->HashKey)
        {
            return slotP;
        }
    }

    return 0;
}
void WriteTable(metac_identifier_table_t* table, const char* filename, uint32_t lengthShift, const char* comment)
{
    // printf("Writing Table with %u entires\n", table->SlotsUsed);
    void* newLinePosInStrings =
        memchr(table->StringMemory, '\n', table->StringMemorySize);

    if (newLinePosInStrings != 0)
    {
        fprintf(stderr, " --- Newline in strings detected ----\n");
        fprintf(stderr, " --- Table will not be serilaized ----\n");
        return ;
    }

    FILE* fd = fopen(filename, "wb");

    identifier_table_file_header_t header = {0};
    header.LengthShift = lengthShift;
    // file looks like
    //char chunk[4192];
    //uint32_t chunk_used = 0;
    fpos_t startPosition;
    (void)fgetpos(fd, &startPosition);

    header.HeaderCommentSize = (comment ? strlen(comment) : 0);
    uint32_t headerSize = sizeof(header) + ALIGN4(header.HeaderCommentSize);
    fseek(fd, headerSize, SEEK_CUR);

    header.OffsetSlots = headerSize;
    header.SizeofSlot = sizeof(metac_identifier_table_slot_t);
    header.StringMemorySize = table->StringMemorySize;
    // -------------- write out slots ---------------------------------
    const uint32_t maxSlots = (1 << table->SlotCount_Log2);
    for(uint32_t slotIndex = 0;
        slotIndex < maxSlots;
        slotIndex++)
    {
        metac_identifier_table_slot_t slot = table->Slots[slotIndex];
        if (slot.HashKey)
        {
            header.NumberOfSlots++;
            fwrite(&slot, 1, sizeof(slot), fd);
        }
    }
    // ----------------- write out strings ----------------------------
    header.OffsetStrings = header.OffsetSlots +
        (header.SizeofSlot * header.NumberOfSlots);

    const char* stringP = table->StringMemory;
    for(uint32_t i = 0;
        i < header.NumberOfSlots;
        i++)
    {
        uint32_t len = strlen(stringP);
        fwrite(stringP, 1, len, fd);
        fwrite("\n", 1, 1, fd);
        uint32_t padBy = ALIGN4(len + 1) - (len + 1);
        fwrite("@@@@", 1, padBy, fd);
        stringP += ALIGN4(len + 1);
    }
    assert((stringP - table->StringMemory) == table->StringMemorySize);

    // --------------- write header / end of file ----------------------
    fsetpos(fd, &startPosition);
    fwrite(&header, 1, sizeof(header), fd);
    fclose(fd);
}
#endif

#if 0

#ifndef REFCOUNT


            fprintf(fd, "Hash: %x Id: %s\n",
                                slot.HashKey,
                                       IdentifierPtrToCharPtr(table, slot.Ptr));

#else
            fprintf(fd, "Hash: %x Id: %s RefCount: %u Displacement: %u\n",
                               slot.HashKey,
                                      IdentifierPtrToCharPtr(table, slot.Ptr),
                                                   slot.RefCount,
                                                                    slot.Displacement);
#endif
#endif
