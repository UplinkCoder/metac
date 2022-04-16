#ifndef _IDENTIFIER_TABLE_C_
#define _IDENTIFIER_TABLE_C_

#include "compat.h"
#include <assert.h>
#include <stdlib.h>
#include "metac_lexer.h"
#include "metac_identifier_table.h"
#include <string.h>

#include "3rd_party/tracy/TracyC.h"


#ifdef _MSC_VER
#  define __builtin_memcpy memcpy
#endif
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
    table->SlotCount_Log2 = 14;
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
                                          const char* identifier, uint32_t identifierKey)
{
    // TracyCZoneCtx ctx;
    TracyCZone(ctx, true);
    const uint32_t length = LENGTH_FROM_IDENTIFIER_KEY(identifierKey);
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
        metac_identifier_table_slot_t *slot = &table->Slots[slotIndex - 1];
        if (slot->HashKey == identifierKey)
        {
            const char* stringEntry = IdentifierPtrToCharPtr(table, slot->Ptr);
            if (__builtin_memcmp(identifier, stringEntry, length) == 0)
            {
                static uint32_t Hits = 0;
                
                TracyCPlot("Hits", ++Hits);
                
                result = slot->Ptr;
                break;
            }
            else
            {
                static uint32_t collisions = 0;
                
                TracyCPlot("Collisions", ++collisions);
            }
        }
        else if (slot->HashKey == 0)
        {
            uint32_t expected;
            uint32_t newValue;
            TracyCPlot("MaxDisplacement", table->MaxDisplacement);
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
            __builtin_memcpy(tableMem, identifier, length);
            tableMem[length] = '\0';
            static uint32_t misses = 0;
            TracyCPlot("Misses", misses++);
            slot->HashKey = identifierKey;
            slot->Ptr = result;
            break;
        }
#if 0
        else
        {
            const uint32_t TargetSlot = (slot->HashKey & slotIndexMask);
            int TargetDisplacement = (slotIndex - TargetSlot); 
            if (++displacement > TargetDisplacement)
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
            if (++displacement > table->MaxDisplacement)
            {
                table->MaxDisplacement = displacement;
            }
            TracyCPlot("MaxDisplacement", table->MaxDisplacement);
            TracyCPlot("LoadFactor", (float)table->SlotsUsed / (float)slotIndexMask);
            TracyCPlot("StringMemorySize", table->StringMemorySize);
        continue;
    }
    TracyCZoneEnd(ctx);

    return result;
}
#endif
