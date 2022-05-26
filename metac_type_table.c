#ifndef _METAC_TYPE_TABLE_C_
#define _METAC_TYPE_TABLE_C_
#include <assert.h>
#include "metac_type_table.h"
#include "metac_type.h"
#include <string.h>
#include <stdlib.h>

#define GET_OR_ADD_TYPE_DEF(TYPE_NAME, MEMBER_NAME) \
metac_type_index_t MetaCTypeTable_GetOrAdd ## MEMBER_NAME ## Type \
    (METAC_TYPE_TABLE_T(TYPE_NAME)* table, \
     METAC_TYPE_TABLE_KEY_T(TYPE_NAME)* key, \
     metac_type_##TYPE_NAME##_t* type) \
{ \
    metac_type_index_t result = \
        MetaCTypeTable_GetOrAddTypeImpl((metac_type_table_t*)table, (metac_type_t*) type, \
        (metac_type_table_slot_t*)key, (sizeof(*key) - sizeof(uint32_t))); \
    return result; \
}

void TypeTableInitImpl(metac_type_table_t* table, const uint32_t sizeof_slot, metac_type_index_kind_t kind)
{
    table->Kind = kind;
    table->SlotCount_Log2 = 12;
    const uint32_t maxSlots = (1 << table->SlotCount_Log2);
    table->Slots = (metac_type_table_slot_t*) calloc(maxSlots, sizeof_slot);
    table->SlotsUsed = 0;
    table->MaxDisplacement = 0;
}

#ifndef ALIGN4
#  define ALIGN4(N) (((N) + 3) & ~3)
#endif
metac_type_index_t MetaCTypeTable_GetOrAddTypeImpl(metac_type_table_t* table,
                                                   metac_type_t* type,
                                                   metac_type_table_slot_t* key,
                                                   const uint32_t keyTrailingSize)
{
    metac_type_index_t result = {0};

    const uint32_t hash = key->HashKey;
    const uint32_t keySize = keyTrailingSize + sizeof(hash);
    const uint32_t slotIndexMask = ((1 << table->SlotCount_Log2) - 1);
    const uint32_t initialSlotIndex = (hash & slotIndexMask);

    for(
        uint32_t slotIndex = initialSlotIndex;
        (++slotIndex & slotIndexMask) != initialSlotIndex;
    )
    {
        metac_type_table_slot_t* slot = (metac_type_table_slot_t*)
        (((char*)table->Slots) +
            ((slotIndex - 1) & slotIndexMask) * keySize);

        if (slot->HashKey == hash)
        {
            if (memcmp(((char*)&slot->HashKey) + sizeof(slot->HashKey),
                        ((char*)key) + sizeof(hash),
                        keyTrailingSize)
                == 0)
            {
                result = slot->TypeIndex;
                break;
            }
            else
            {
                // collision happend
            }
        }
        else if (slot->HashKey == 0)
        {
#ifdef ATOMIC
            uint32_t expected = 0;
            uint32_t newValue;
#endif
            do {
                //assert(table->KeyMemorySize + ALIGN4(keySize)
                //       < table->KeyMemoryCapacity);
                int32_t abs_index = ((char*)slot) - ((char*)table->Slots);
                result.v = TYPE_INDEX_V(table->Kind, abs_index / (keySize));
                // Compare xchange here
#ifdef ATOMIC
                if (expected != 0)
                {
                    // we tried to cmp_xchg and failed ...
                    // this can mean another thread beat us to it or it can mean
                    // that
                }
                newValue = hash;
            } while(!__atomic_compare_exchange(&slot->HashKey, &expected, &newValue,
                false, __ATOMIC_ACQUIRE, __ATOMIC_ACQUIRE));
            // atomic compare exchange has been done.
            __atomic_add_fetch(&table->SlotsUsed, 1, __ATOMIC_ACQUIRE)
#else
                slot->HashKey = hash;
                table->SlotsUsed = table->SlotsUsed + 1;
            } while (false);
#endif

            memcpy(((char*)slot) + sizeof(slot->HashKey), ((char*)key) + sizeof(hash), keyTrailingSize);
            break;
        }
        continue;
    }
    return result;
}

FOREACH_TABLE_MEMBER(GET_OR_ADD_TYPE_DEF);

uint32_t EntangleInts(uint32_t a, uint32_t b)
{
    uint32_t max_n = (a > b ? a : b);
    uint32_t result = 0;
    uint32_t srcPos = 0;
    uint32_t dstPos = 0;

    if (max_n > (1 << 15))
    {
        max_n = (1 << 15);
        result |= (1 << 31);
    }

    while(max_n)
    {
        {
            bool bit = (a & (1 << srcPos)) >> srcPos;
            result |= (bit << dstPos++);
        }

        {
            bool bit = (b & (1 << srcPos)) >> srcPos;
            result |= (bit << dstPos++);
        }
        srcPos++;
        max_n >>= 1;
    }

    return result;
}

uint32_t UntangleInts(uint32_t tangled)
{
    assert((tangled & (1 << 31)) == 0);
    uint16_t a = 0;
    uint16_t b = 0;
    uint32_t binpos = 0;

    while(tangled)
    {
        a |= ((tangled & 1) << binpos);
        tangled >>= 1;

        b |= ((tangled & 1) << binpos);
        tangled >>= 1;
        binpos++;
    }

    uint32_t result = a | (b << 16);

    return result;
}


#endif // _METAC_TYPE_TABLE_C_
