#ifndef _IDENTIFIER_TABLE_C_
#define _IDENTIFIER_TABLE_C_

#include "../os/compat.h"

#include <assert.h>
#include <stdlib.h>
#include "metac_lexer.h"
#include "metac_identifier_table.h"
#include <string.h>
#include <stdio.h>

#include "../os/metac_atomic.h"

#include "../3rd_party/tracy/TracyC.h"

#ifdef TRACY_ENABLE
#  define TRACY_COUNTER(COUNTER) static uint32_t COUNTER
#else
#  define TRACY_COUNTER(COUNTER)
#endif

const metac_identifier_ptr_t empty_identifier = {~0u};

static inline bool IsFilled(metac_identifier_table_slot_t slot)
{
    return slot.HashKey != 0;
}

const char* IdentifierPtrToCharPtr(const metac_identifier_table_t* table,
                                   metac_identifier_ptr_t ptr)
{
    assert(ptr.v);
    return table->StringMemory + (ptr.v - 4);
}

void IdentifierTable_Init(metac_identifier_table_t* table, uint32_t lengthShift, uint32_t slotCountLog2)
{
    table->SlotCount_Log2 = slotCountLog2;
    const uint32_t maxSlots = (1 << table->SlotCount_Log2);
    table->Slots = (metac_identifier_table_slot_t*) calloc(maxSlots, sizeof(metac_identifier_table_slot_t));
    table->StringMemory = (char*)malloc(maxSlots * 32);
    table->StringMemoryCapacity = maxSlots * 32;
    table->StringMemorySize = 0;
    table->SlotsUsed = 0;
    table->LengthShift = lengthShift;
    table->MaxDisplacement = 0;
}
void IdentifierTable_Free(metac_identifier_table_t* table)
{
    free(table->Slots);
    free(table->StringMemory);
    static const metac_identifier_table_t zeroTable = {0};
    (*table) = zeroTable;
}
#define ALIGN4(N) (((N) + 3) & ~3)
metac_identifier_ptr_t GetOrAddIdentifier(metac_identifier_table_t* table,
                                          uint32_t identifierKey,
                                          const char* identifier)
{
    TracyCZone(ctx, true);
    uint32_t length = (identifierKey >> table->LengthShift);

    metac_identifier_ptr_t result = {0};

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
            } while(_InterlockedCompareExchange(&table->StringMemorySize, newValue, expected)
                    != expected))
            // atomic compare exchange has been done.
            _InterlockedIncrement(&table->SlotsUsed);
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

/// returns slotIndex if key was found and -1 if it's not found
int32_t MetaCIdentifierTable_HasKey(metac_identifier_table_t* table,
                                    uint32_t key)
{
    int32_t result = -1;

    const uint32_t slotIndexMask = ((1 << table->SlotCount_Log2) - 1);
    const uint32_t initialSlotIndex = (key & slotIndexMask);
    assert(slotIndexMask);
    // if slotIndexMask is 0 most likely the table has not been initialized
    // TracyCPlot("TargetIndex", initialSlotIndex);

    for(
        uint32_t slotIndex = initialSlotIndex;
        (++slotIndex & slotIndexMask) != initialSlotIndex;
    )
    {
        int32_t idx = cast(int32_t)((slotIndex - 1) & slotIndexMask);
        metac_identifier_table_slot_t slot =
            table->Slots[idx];

        if (slot.HashKey == 0)
            break;
        else if (slot.HashKey == key)
        {
            result = idx;
            break;
        }
    }

    return result;
}

#include "../os/bsr.h"
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

// TODO: Rename to GetIdentifer
metac_identifier_ptr_t IsIdentifierInTable(metac_identifier_table_t* table,
                                           uint32_t key,
                                           const char* idChars)
{
    metac_identifier_ptr_t result = {0};

    const uint32_t slotIndexMask = ((1 << table->SlotCount_Log2) - 1);
    const uint32_t initialSlotIndex = (key & slotIndexMask);
    assert(slotIndexMask);
    // if slotIndexMask is 0 most likely the table has not been initialized
    // TracyCPlot("TargetIndex", initialSlotIndex);
    for(
        uint32_t slotIndex = initialSlotIndex;
        (++slotIndex & slotIndexMask) != initialSlotIndex;
    )
    {
        metac_identifier_table_slot_t slot =
            table->Slots[(slotIndex - 1) & slotIndexMask];

        if (slot.HashKey == 0)
            return result;
        if (slot.HashKey == key)
        {
            bool matches =
                !memcmp(IdentifierPtrToCharPtr(table, slot.Ptr), idChars,
                        LENGTH_FROM_IDENTIFIER_KEY(key));
            if (matches)
            {
                result = slot.Ptr;
                return result;
            }
        }
    }
    assert(0);

    return result;
}


metac_identifier_table_slot_t* IdentifierTableLookup(
            metac_identifier_table_t* table,
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
        uint32_t lookupIdx = (slotIndex - 1) & slotIndexMask;
        metac_identifier_table_slot_t slot =
            table->Slots[lookupIdx];

        if (slot.HashKey == 0)
            return 0;
        if (slot.HashKey == key && slot.Ptr.v == value.v)
            return table->Slots + lookupIdx;
    }
    assert(0);
    return 0;
}

bool IsInTable(metac_identifier_table_t* table,
               uint32_t key, metac_identifier_ptr_t value)
{
    return IdentifierTableLookup(table, key, value) != 0;
}

#ifdef WRITE_TABLE
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
#if __TINYC__
    char slotMem[slotMemorySize];
#else
#endif
    slot_t* readSlot = (slot_t*)malloc(slotMemorySize);
    memset(readSlot, 0, slotMemorySize);

    for(uint32_t i = 0; i < header.NumberOfSlots; i++)
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
