#ifndef _METAC_TYPE_TABLE_C_
#define _METAC_TYPE_TABLE_C_
#include <assert.h>
#include "metac_type_table.h"
#include "metac_type.h"
#include "metac_alloc_node.h"
#include <string.h>
#include <stdlib.h>

#define GET_OR_EMPTY_TYPE_DEF(TYPE_NAME, MEMBER_NAME, CMP_FUNC) \
metac_type_index_t MetaCTypeTable_GetOrEmpty ## MEMBER_NAME ## Type \
    (const METAC_TYPE_TABLE_T(TYPE_NAME)* table, \
     METAC_TYPE_TABLE_KEY_T(TYPE_NAME)* key) \
{ \
    metac_type_index_t result = \
        MetaCTypeTable_GetOrEmptyImpl((metac_type_table_t*)table, \
                                      (metac_type_table_slot_t*)key, \
                                      (sizeof(*key) - sizeof(metac_type_table_slot_t)), \
                                      CMP_FUNC \
        ); \
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
metac_type_index_t MetaCTypeTable_GetOrEmptyImpl(metac_type_table_t* table,
                                                 metac_type_table_slot_t* key,
                                                 const uint32_t keyTrailingSize,
                                                 const bool (*cmpSlot)(const metac_type_table_slot_t*,
                                                                       const metac_type_table_slot_t*))
{
    metac_type_index_t result = {0};

    const uint32_t hash = key->HashKey;
    const uint32_t keySize = keyTrailingSize + sizeof(metac_type_table_slot_t);
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
            if (cmpSlot(slot, key))
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
            break;
        }
        continue;
    }
    return result;
}

FOREACH_TABLE_MEMBER(GET_OR_EMPTY_TYPE_DEF);


#define ADD_TYPE_DEF(TYPE_NAME, MEMBER_NAME, CMP_FUNC) \
void MetaCTypeTable_Add ## MEMBER_NAME ## Type \
    (METAC_TYPE_TABLE_T(TYPE_NAME)* table, \
     const METAC_TYPE_TABLE_KEY_T(TYPE_NAME)* key) \
{ \
        MetaCTypeTable_AddImpl((metac_type_table_t*)table, \
        (metac_type_table_slot_t*)key, (sizeof(*key) - sizeof(metac_type_table_slot_t)), CMP_FUNC); \
}

void MetaCTypeTable_AddImpl(metac_type_table_t* self,
                            const metac_type_table_slot_t *entry,
                            const uint32_t trailingSize,
                            bool (*cmpSlot)(const metac_type_table_slot_t*,
                                            const metac_type_table_slot_t*))
{
    assert(entry->HashKey != 0 && entry->TypeIndex.v != 0 && entry->TypeIndex.v != -1);

    // first we find the slot
    const uint32_t hash = entry->HashKey;
    const uint32_t keySize = trailingSize + sizeof(metac_type_table_slot_t);

    const uint32_t slotIndexMask = ((1 << self->SlotCount_Log2) - 1);
    const uint32_t initialSlotIndex = (hash & slotIndexMask);

    for(
        uint32_t slotIndex = initialSlotIndex;
        (++slotIndex & slotIndexMask) != initialSlotIndex;
    )
    {
        metac_type_table_slot_t* slot = (metac_type_table_slot_t*)
        (((char*)self->Slots) +
            ((slotIndex - 1) & slotIndexMask) * keySize);

        if (slot->HashKey == 0)
        {
            // we've found the first empty space to put this entry
            memcpy(slot, entry, keySize);
            return ;
        }
        if (slot->HashKey == hash)
        {
            assert (memcmp(((char*)slot) + sizeof(metac_type_table_slot_t),
                        ((char*)entry)   + sizeof(metac_type_table_slot_t),
                        trailingSize)
                != 0);
            // this assert makes sure we don't have duplicates
        }
    }
    assert(0);
}

FOREACH_TABLE_MEMBER(ADD_TYPE_DEF);



#if 0 // Add type entry
        else if (slot->HashKey == 0)
        {
#ifdef ATOMIC
            uint32_t expected = 0;
            uint32_t newValue;
            do {
                // Compare xchange here
                if (expected != 0)
                {
                    // we tried to cmp_xchg and failed ...
                    // this can mean another thread beat us to it
                    assert(0);
                }
                newValue = hash;
            } while(!__atomic_compare_exchange(&slot->HashKey, &expected, &newValue,
                false, __ATOMIC_ACQUIRE, __ATOMIC_ACQUIRE));
            // atomic compare exchange has been done.
            __atomic_add_fetch(&table->SlotsUsed, 1, __ATOMIC_ACQUIRE)
#else
            do {
                assert(slot->HashKey == 0);
                slot->HashKey = hash;
                table->SlotsUsed = table->SlotsUsed + 1;
            } while (false);
#endif

            memcpy(&slot->TypeIndex + 1, ((char*)key) + sizeof(hash), keyTrailingSize);
            uint32_t idx = ~0;
            switch(table->Kind)
            {
                metac_declaration_kind_t declKind;

                case type_index_struct:
                { declKind = decl_type_struct; } goto Lagg;
                case type_index_union:
                { declKind = decl_type_union; } goto Lagg;
                Lagg:
                {

                } break;

                case type_index_typedef:
                {
                    metac_type_typedef_slot_t *typedef_=
                        (metac_type_typedef_slot_t*) key;
                    metac_type_typedef_t* semaTypedef
                        = AllocNewSemaTypedef(typedef_->ElementTypeIndex);
                    idx = TypedefIndex(semaTypedef);
                } break;

                case type_index_functiontype:
                {
                    metac_type_functiontype_slot_t* funcType =
                        (metac_type_functiontype_slot_t*) key;

                    metac_type_typedef_t* semaFunctype
                        = AllocNewSemaFunctionype(funcType->ReturnType,
                                                  funcType->ParameterTypes,
                                                  funcType->ParameterTypeCount);
                    idx = FunctiontypeIndex(semaFunctype);
                } break;
                case type_index_ptr:
                {
                    metac_type_ptr_slot_t* ptrType =
                        (metac_type_ptr_slot_t*) key;
                    metac_type_ptr_t* semaPtr = AllocNewSemaPtrType(ptrType->ElementTypeIndex);
                    idx = PtrTypeIndex(semaPtr);
                } break;
                default: assert(0);
            }
            slot->TypeIndex.v =
                TYPE_INDEX_V(table->Kind, idx);
            result = slot->TypeIndex;
            break;
        }
        continue;
#endif

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
