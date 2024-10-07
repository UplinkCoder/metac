#include "metac_value_table.h"

const metac_node_ptr_t empty_node = {~0u};

static inline bool IsFilled(metac_value_table_slot_t slot)
{
    return slot.HashKey != 0;
}

void ValueTable_Init_(metac_value_table_t* table, uint32_t slotCountLog2, metac_alloc_t* alloc,
                      const char* file, uint32_t line)
{
    table->SlotCount_Log2 = slotCountLog2;
    const uint32_t maxSlots = (1 << table->SlotCount_Log2);
    table->Slots = cast(metac_value_table_slot_t*)
        Allocator_Calloc_(alloc, sizeof(metac_value_table_slot_t), maxSlots, file, line);
    table->SlotsUsed = 0;
 

void ValueTable_Free(metac_value_table_t* table)
{
    static const metac_value_table_t zeroTable = {0};
    (*table) = zeroTable;
}

metac_node_ptr_t GetOrAddValue(metac_value_table_t* table, metac_node_ptr_t keyNode, metac_node_ptr_t valueNode)
{
    metac_node_ptr_t result = {0};

    const uint32_t slotIndexMask = ((1 << table->SlotCount_Log2) - 1);
    const uint32_t initialSlotIndex = (keyNodeHash & slotIndexMask);
    uint32_t displacement = 0;

    for (uint32_t slotIndex = initialSlotIndex; (++slotIndex & slotIndexMask) != initialSlotIndex;)
    {
        metac_value_table_slot_t* slot = &table->Slots[(slotIndex - 1) & slotIndexMask];

        if (slot->HashKey == keyNodeHash)
        {
            if (slot->KeyNode.v == keyNode.v)
            {
                result = slot->ValueNode;
#ifdef REFCOUNT
                slot->RefCount++;
#endif
                break;
            }
        }
        else if (slot->HashKey == 0)
        {
            result = valueNode;
            slot->HashKey = keyNodeHash;
            slot->KeyNode = keyNode;
            slot->ValueNode = valueNode;
#ifdef REFCOUNT
            slot->RefCount = 1;
            slot->Displacement = displacement;
#endif
            table->SlotsUsed++;
            break;
        }
        displacement++;
    }
    return result;
}

int32_t ValueTable_HasKey(metac_value_table_t* table, uint32_t keyNodeHash, metac_node_ptr_t keyNode)
{
    int32_t result = -1;
    const uint32_t slotIndexMask = ((1 << table->SlotCount_Log2) - 1);
    const uint32_t initialSlotIndex = (keyNodeHash & slotIndexMask);

    for (uint32_t slotIndex = initialSlotIndex; (++slotIndex & slotIndexMask) != initialSlotIndex;)
    {
        int32_t idx = (int32_t)((slotIndex - 1) & slotIndexMask);
        metac_value_table_slot_t slot = table->Slots[idx];

        if (slot.HashKey == 0)
            break;
        if (slot.HashKey == keyNodeHash && slot.KeyNode.v == keyNode.v)
        {
            result = idx;
            break;
        }
    }

    return result;
}

metac_node_ptr_t IsValueInTable(metac_value_table_t* table, uint32_t keyNodeHash, metac_node_ptr_t keyNode)
{
    metac_node_ptr_t result = {0};
    const uint32_t slotIndexMask = ((1 << table->SlotCount_Log2) - 1);
    const uint32_t initialSlotIndex = (keyNodeHash & slotIndexMask);

    for (uint32_t slotIndex = initialSlotIndex; (++slotIndex & slotIndexMask) != initialSlotIndex;)
    {
        metac_value_table_slot_t slot = table->Slots[(slotIndex - 1) & slotIndexMask];

        if (slot.HashKey == 0)
            return result;
        if (slot.HashKey == keyNodeHash && slot.KeyNode.v == keyNode.v)
        {
            result = slot.ValueNode;
            return result;
        }
    }

    return result;
}

metac_value_table_slot_t* ValueTableLookup(metac_value_table_t* table, uint32_t keyNodeHash, metac_node_ptr_t keyNode, metac_node_ptr_t valueNode)
{
    const uint32_t slotIndexMask = ((1 << table->SlotCount_Log2) - 1);
    const uint32_t initialSlotIndex = (keyNodeHash & slotIndexMask);

    for (uint32_t slotIndex = initialSlotIndex; (++slotIndex & slotIndexMask) != initialSlotIndex;)
    {
        uint32_t lookupIdx = (slotIndex - 1) & slotIndexMask;
        metac_value_table_slot_t slot = table->Slots[lookupIdx];

        if (slot.HashKey == 0)
            return 0;
        if (slot.HashKey == keyNodeHash && slot.KeyNode.v == keyNode.v && slot.ValueNode.v == valueNode.v)
            return table->Slots + lookupIdx;
    }

    return 0;
}

bool IsInValueTable(metac_value_table_t* table, uint32_t keyNodeHash, metac_node_ptr_t keyNode, metac_node_ptr_t valueNode)
{
    return ValueTableLookup(table, keyNodeHash, keyNode, valueNode) != 0;
}