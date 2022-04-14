#ifndef _IDENTIFIER_TABLE_C_
#define _IDENTIFIER_TABLE_C_

#include "compat.h"
#include <assert.h>
#include <stdlib.h>
#include "metac_lexer.h"
#include "metac_identifier_table.h"

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
    table->SlotCount_Log2 = 8;
    table->Slots = table->inlineSlots;
    table->StringMemory = (char*)malloc(16384);
    table->StringMemoryCapacity = 16384;
    table->StringMemorySize = 0;
    for(int slotIdx = 0; slotIdx < 256; slotIdx++)
    {
        table->Slots[slotIdx].HashKey = 0;
    }
}

#define ALIGN4(N) (((N) + 3) & ~3)
metac_identifier_ptr_t GetOrAddIdentifier(metac_identifier_table_t* table,
                                          const char* identifier, uint32_t identifierKey)
{
    const uint32_t length = LENGTH_FROM_IDENTIFIER_KEY(identifierKey);
    metac_identifier_ptr_t result = {0};
    const uint32_t initialSlotIndex =
        (identifierKey & ((1 << table->SlotCount_Log2) - 1));
    for(
        uint32_t slotIndex = initialSlotIndex;
        ++slotIndex != initialSlotIndex;
    )
    {
        metac_identifier_table_slot_t *slot = &table->Slots[slotIndex - 1];
        if (slot->HashKey == identifierKey)
        {
            const char* stringEntry = IdentifierPtrToCharPtr(table, slot->Ptr);
            if (__builtin_memcmp(identifier, stringEntry, length) == 0)
            {
                result = slot->Ptr;
                break;
            }
        }
        else if (slot->HashKey == 0)
        {
            uint32_t expected;
            uint32_t newValue;
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
#else
            } while(!__atomic_compare_exchange(&table->StringMemorySize, &expected, &newValue,
                false, __ATOMIC_ACQUIRE, __ATOMIC_ACQUIRE));
            // atomic compare exchange has been done.
#endif
            char* tableMem = (table->StringMemory + (result.v - 4));
            __builtin_memcpy(tableMem, identifier, length);
            tableMem[length] = '\0';

            slot->HashKey = identifierKey;
            slot->Ptr = result;
            break;
        }
        continue;
    }

    return result;
}
#endif
