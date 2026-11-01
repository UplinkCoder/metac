#ifndef _IDENTIFIER_TABLE_C_
#define _IDENTIFIER_TABLE_C_

#include "../os/compat.h"

#include <assert.h>
#include <stdlib.h>
#include "metac_lexer.h"
#include "metac_identifier_table.h"
#include "../hash/crc32c.h"
#include "../os/metac_alloc.h"
#include <string.h>
#include <stdio.h>

#include "../os/metac_atomic.h"
#include "../os/bsr.h"

#include "../3rd_party/tracy/TracyC.h"

// Robin Hood probing for GetOrAddIdentifier. Enable with -DMETAC_ROBINHOOD=1.
#ifndef METAC_ROBINHOOD
#  define METAC_ROBINHOOD 0
#endif

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

void InsertSlot(metac_identifier_table_slot_t* slots, metac_identifier_table_slot_t slot,
                const uint32_t slotIndexMask);

const char* IdentifierPtrToCharPtr(const metac_identifier_table_t* table,
                                   metac_identifier_ptr_t ptr)
{
    assert(ptr.v);
    return table->StringMemory + (ptr.v - 4);
}

void IdentifierTable_Init_(metac_identifier_table_t* table, uint32_t lengthShift, uint32_t slotCountLog2, metac_alloc_t* alloc,
                           const char* file, uint32_t line)
{
    table->SlotCount_Log2 = slotCountLog2;
    const uint32_t maxSlots = (1 << table->SlotCount_Log2);
    table->Slots = cast(metac_identifier_table_slot_t*)
        Allocator_Calloc_(alloc, sizeof(metac_identifier_table_slot_t), maxSlots, file, line);
    table->StringMemory = cast(char*)
        Allocator_Calloc_(alloc, sizeof(char), maxSlots * 32, file, line);
    table->StringMemoryCapacity = maxSlots * 32;
    table->StringMemorySize = 0;
    table->SlotsUsed = 0;
    table->LengthShift = lengthShift;
    table->MaxDisplacement = 0;
    table->Alloc = alloc;
}

void IdentifierTable_Free(metac_identifier_table_t* table)
{
    // Arena-backed tables (Alloc != 0) are reclaimed with the parent
    // allocator; only malloc-backed tables (ReadTable) own their buffers.
    if (table->Alloc == 0)
    {
        free(table->Slots);
        free(table->StringMemory);
    }
    static const metac_identifier_table_t zeroTable = {0};
    (*table) = zeroTable;
}

/// Ceil(log2(n)) for n >= 1; 0 for n == 0.
static inline uint32_t MetaCIdentifierTable_CeilLog2(uint32_t n)
{
    uint32_t log2 = 0;
    while ((1u << log2) < n && log2 < 31)
        log2++;
    return log2;
}

bool MetaCIdentifierTable_Grow(metac_identifier_table_t* self, uint32_t nSlots)
{
    if (!self->Alloc)
        return false;

    const uint32_t newLog2 = MetaCIdentifierTable_CeilLog2(nSlots);
    if (newLog2 < self->SlotCount_Log2)
        return true; // already large enough

    const uint32_t newMaxSlots = 1u << newLog2;
    metac_identifier_table_slot_t* newSlots =
        cast(metac_identifier_table_slot_t*)
        Allocator_Calloc_(self->Alloc, sizeof(metac_identifier_table_slot_t),
                          newMaxSlots, __FILE__, __LINE__);
    if (!newSlots)
        return false;

    const uint32_t oldMaxSlots = 1u << self->SlotCount_Log2;
    const uint32_t newIndexMask = newMaxSlots - 1;
    for (uint32_t i = 0; i < oldMaxSlots; i++)
    {
        if (self->Slots[i].HashKey)
            InsertSlot(newSlots, self->Slots[i], newIndexMask);
    }

    // Scale string storage with the slot table when we own the allocator.
    const uint32_t newStrCapacity = newMaxSlots * 32;
    if (newStrCapacity > self->StringMemoryCapacity)
    {
        char* newStringMemory =
            cast(char*) Allocator_Calloc_(self->Alloc, sizeof(char),
                                          newStrCapacity, __FILE__, __LINE__);
        if (!newStringMemory)
        {
            // Slots already rehashed; keep old string buffer.
        }
        else
        {
            memcpy(newStringMemory, self->StringMemory, self->StringMemorySize);
            self->StringMemory = newStringMemory;
            self->StringMemoryCapacity = newStrCapacity;
        }
    }

    self->Slots = newSlots;
    self->SlotCount_Log2 = newLog2;
    return true;
}

static bool MetaCIdentifierTable_GrowStringMemory(metac_identifier_table_t* table,
                                                  uint32_t minCapacity)
{
    if (!table->Alloc)
        return false;

    uint32_t newCapacity = table->StringMemoryCapacity;
    if (newCapacity == 0)
        newCapacity = 64;
    while (newCapacity < minCapacity && newCapacity < (1u << 30))
        newCapacity *= 2;

    char* newMemory = cast(char*)
        Allocator_Calloc_(table->Alloc, sizeof(char), newCapacity,
                          __FILE__, __LINE__);
    if (!newMemory)
        return false;

    memcpy(newMemory, table->StringMemory, table->StringMemorySize);
    table->StringMemory = newMemory;
    table->StringMemoryCapacity = newCapacity;
    return true;
}

#define ALIGN4(N) (((N) + 3) & ~3)

/// Intern `identifier` (length bytes, not counting NUL) into table string memory.
static metac_identifier_ptr_t MetaCIdentifierTable_InternString(
    metac_identifier_table_t* table, const char* identifier, uint32_t length)
{
    metac_identifier_ptr_t result = {0};
    const uint32_t alignLen = ALIGN4(length + 1);

    if (table->StringMemorySize + alignLen >= table->StringMemoryCapacity)
    {
        if (!MetaCIdentifierTable_GrowStringMemory(
                table, table->StringMemorySize + alignLen + 1))
        {
            return result;
        }
    }

#ifdef ATOMIC
    {
        uint32_t expected = table->StringMemorySize;
        uint32_t newValue = expected + alignLen;
        if (_InterlockedCompareExchange(&table->StringMemorySize,
                                        newValue, expected) != expected)
        {
            return result; // caller may retry
        }
        result.v = expected + 4;
    }
    _InterlockedIncrement(&table->SlotsUsed);
#else
    result.v = table->StringMemorySize + 4;
    table->StringMemorySize += alignLen;
    ++table->SlotsUsed;
#endif

    char* dest = table->StringMemory + (result.v - 4);
    memcpy(dest, identifier, length);
    dest[length] = '\0';
    return result;
}

metac_identifier_ptr_t GetOrAddIdentifier(metac_identifier_table_t* table,
                                          uint32_t identifierKey,
                                          const char* identifier)
{
    TracyCZone(ctx, true);
    const uint32_t length = (identifierKey >> table->LengthShift);
    const uint32_t origKey = identifierKey;

    metac_identifier_ptr_t result = {0};

    // Grow when load factor exceeds ~50% so probe sequences stay short.
    // Skip for malloc-backed tables (no allocator) which cannot grow.
    if (table->Alloc &&
        table->SlotsUsed > ((1u << table->SlotCount_Log2) / 2))
    {
        const uint32_t growTo = 1u << (table->SlotCount_Log2 + 1);
        MetaCIdentifierTable_Grow(table, growTo);
    }

    const uint32_t slotIndexMask = ((1 << table->SlotCount_Log2) - 1);
    const uint32_t initialSlotIndex = (origKey & slotIndexMask);
    // TracyCPlot("TargetIndex", initialSlotIndex);
    uint32_t displacement = 0;

#if METAC_ROBINHOOD
    // Entry currently seeking a home. Starts as the caller's identifier;
    // after a Robin Hood steal it becomes the displaced occupant.
    uint32_t placeKey = origKey;
    metac_identifier_ptr_t placePtr = {0};
    bool placeIsOriginal = true;
#endif

    // Visit every slot exactly once (mask+1 probes), starting at initialSlotIndex.
    for(
        uint32_t probes = 0;
        probes <= slotIndexMask;
        probes++
    )
    {
        const uint32_t slotIdx = (initialSlotIndex + probes) & slotIndexMask;
        metac_identifier_table_slot_t* slot = &table->Slots[slotIdx];
        const char* stringEntry;

#if METAC_ROBINHOOD
        if (placeIsOriginal && slot->HashKey == origKey)
#else
        if (slot->HashKey == identifierKey)
#endif
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
            TracyCPlot("Displacement", displacement);

#if METAC_ROBINHOOD
            if (!placePtr.v)
            {
                placePtr = MetaCIdentifierTable_InternString(
                    table, identifier, length);
                if (!placePtr.v)
                {
                    result.v = 0;
                    break;
                }
            }
            slot->HashKey = placeKey;
            slot->Ptr = placePtr;
            if (placeIsOriginal)
                result = placePtr;
#else
            metac_identifier_ptr_t inserted =
                MetaCIdentifierTable_InternString(
                    table, identifier, length);
            if (!inserted.v)
            {
                result.v = 0;
                break;
            }
            slot->HashKey = identifierKey;
            slot->Ptr = inserted;
            result = inserted;
#endif
            TracyCPlot("LoadFactor", (float)table->SlotsUsed / (float)slotIndexMask);
            TracyCPlot("StringMemorySize", table->StringMemorySize);
            static uint32_t misses = 0;
            TracyCPlot("Misses", misses++);
#ifdef REFCOUNT
            slot->RefCount++;
            slot->Displacement = displacement;
#endif
            break;
        }
#if METAC_ROBINHOOD
        else
        {
            // Occupied by a different key: maybe steal if we are poorer.
            const uint32_t theirHome = slot->HashKey & slotIndexMask;
            const uint32_t theirDisp = (slotIdx - theirHome) & slotIndexMask;
            if (displacement > theirDisp)
            {
                if (placeIsOriginal && !placePtr.v)
                {
                    placePtr = MetaCIdentifierTable_InternString(
                        table, identifier, length);
                    if (!placePtr.v)
                    {
                        result.v = 0;
                        break;
                    }
                }

                const uint32_t nextKey = slot->HashKey;
                const metac_identifier_ptr_t nextPtr = slot->Ptr;

                slot->HashKey = placeKey;
                slot->Ptr = placePtr;
#ifdef REFCOUNT
                slot->RefCount++;
                slot->Displacement = theirDisp;
#endif
                if (placeIsOriginal)
                    result = placePtr;

                placeKey = nextKey;
                placePtr = nextPtr;
                placeIsOriginal = false;
                // Displaced entry continues one step past where it sat.
                displacement = theirDisp + 1;
                TracyCPlot("RobinHoodSteal", 1);
                continue;
            }
        }
#endif
        TracyCPlot("MaxDisplacement", table->MaxDisplacement);
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
        uint32_t probes = 0;
        probes <= slotIndexMask;
        probes++
    )
    {
        int32_t idx = cast(int32_t)((initialSlotIndex + probes) & slotIndexMask);
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

static inline metac_identifier_ptr_t
MetaCIdentifierTable_CopyIdentifier(const metac_identifier_table_t* srcTable,
                                    metac_identifier_table_t* dstTable,
                                    metac_identifier_ptr_t srcIdPtr)
{
    assert(srcIdPtr.v);
    const char* idChars =
        IdentifierPtrToCharPtr((metac_identifier_table_t*)srcTable, srcIdPtr);
    const uint32_t idLen = strlen(idChars);
    const uint32_t idHash = crc32c_nozero(~0, idChars, idLen);
    const uint32_t idKey = IDENTIFIER_KEY(idHash, idLen);

    metac_identifier_ptr_t newPtr =
        GetOrAddIdentifier(dstTable, idKey, idChars);
    return newPtr;
}


#define slot_t metac_identifier_table_slot_t

void InsertSlot(slot_t* slots, slot_t slot, const uint32_t slotIndexMask)
{
    const uint32_t initialSlotIndex = (slot.HashKey & slotIndexMask);
    // TracyCPlot("TargetIndex", initialSlotIndex);
    for(
        uint32_t probes = 0;
        probes <= slotIndexMask;
        probes++
    )
    {
        slot_t* dstSlot = slots + ((initialSlotIndex + probes) & slotIndexMask);
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
        uint32_t probes = 0;
        probes <= slotIndexMask;
        probes++
    )
    {
        metac_identifier_table_slot_t slot =
            table->Slots[(initialSlotIndex + probes) & slotIndexMask];

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
    // Not found after a full probe — miss is a normal outcome.
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
        uint32_t probes = 0;
        probes <= slotIndexMask;
        probes++
    )
    {
        uint32_t lookupIdx = (initialSlotIndex + probes) & slotIndexMask;
        metac_identifier_table_slot_t slot =
            table->Slots[lookupIdx];

        if (slot.HashKey == 0)
            return 0;
        if (slot.HashKey == key && slot.Ptr.v == value.v)
            return table->Slots + lookupIdx;
    }
    return 0;
}

bool IsInTable(metac_identifier_table_t* table,
               uint32_t key, metac_identifier_ptr_t value)
{
    return IdentifierTableLookup(table, key, value) != 0;
}

static inline float MetaCIdentifierTable_LoadFactor(
    metac_identifier_table_t* self)
{
    uint32_t slotCount = ((1 << self->SlotCount_Log2) - 1);
    return ((float)self->SlotsUsed / (float)slotCount); 
}

#ifdef WRITE_TABLE
#define CRT_SECURE_NO_WARNINGS
metac_identifier_table_t ReadTable(const char* filename)
{
    metac_identifier_table_t result = {0};

    identifier_table_file_header_t header;
    FILE* fd = fopen(filename, "rb");
    if (!fd)
    {
        fprintf(stderr, "ReadTable: cannot open '%s'\n", filename);
        return result;
    }

    if (fread(&header, 1, sizeof(header), fd) != sizeof(header))
    {
        fclose(fd);
        return result;
    }

    if (header.HeaderCommentSize)
    {
        char* comment = (char*)malloc(header.HeaderCommentSize + 1);
        if (comment)
        {
            if (fread(comment, 1, header.HeaderCommentSize, fd)
                    == header.HeaderCommentSize)
            {
                comment[header.HeaderCommentSize] = '\0';
                printf("Comment: %s\n", comment);
            }
            free(comment);
        }
    }

    // Header is followed by the comment padded to 4 bytes, then slots.
    if (fseek(fd, header.OffsetSlots, SEEK_SET) != 0)
    {
        fclose(fd);
        return result;
    }

    uint32_t stringMemoryCapacity = ((header.StringMemorySize + 64) & ~63);

    result.StringMemoryCapacity = stringMemoryCapacity;
    result.StringMemorySize = header.StringMemorySize;
    result.LengthShift = header.LengthShift;
    // Leave at least one empty slot so probes can terminate (load factor < 1).
    result.SlotCount_Log2 = MetaCIdentifierTable_CeilLog2(header.NumberOfSlots + 1);
    if (result.SlotCount_Log2 < 1)
        result.SlotCount_Log2 = 1;

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
        if (fread(readSlot, 1, header.SizeofSlot, fd) != header.SizeofSlot)
            break;
        InsertSlot(result.Slots, *readSlot, slotIndexMask);
    }
    free(readSlot);

    if (fseek(fd, header.OffsetStrings, SEEK_SET) != 0)
    {
        fclose(fd);
        free(result.Slots);
        result.Slots = 0;
        return result;
    }

    result.StringMemory = (char*)malloc(result.StringMemoryCapacity);
    if (result.StringMemory)
    {
        memset(result.StringMemory, 0, result.StringMemoryCapacity);
        fread(result.StringMemory, 1, result.StringMemorySize, fd);
    }

    uint32_t stringSizeLeft = result.StringMemorySize;
    uint32_t stringsProcessed = 0;

    if (result.StringMemory)
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

    // assert(stringsProcessed == header.NumberOfSlots);
    printf("stringProcessed: %d, header.NumberOfSlots: %d\n",
        stringsProcessed, header.NumberOfSlots);
    result.SlotsUsed = header.NumberOfSlots;
    fclose(fd);
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
        if (len)
        {
            stringP += ALIGN4(len + 1);
        }
    }

    if ((stringP - table->StringMemory) != table->StringMemorySize)
    {
        printf("StringLength: %d\n", cast(int)(stringP - table->StringMemory));
        printf("StringMemorySize: %d\n", table->StringMemorySize);
    }
    else
    {
    }

    // --------------- write header / end of file ----------------------
    fsetpos(fd, &startPosition);
    fwrite(&header, 1, sizeof(header), fd);
    if (header.HeaderCommentSize && comment)
    {
        fwrite(comment, 1, header.HeaderCommentSize, fd);
        // pad comment to ALIGN4
        uint32_t padBy = ALIGN4(header.HeaderCommentSize) - header.HeaderCommentSize;
        if (padBy)
            fwrite("@@@@", 1, padBy, fd);
    }
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
