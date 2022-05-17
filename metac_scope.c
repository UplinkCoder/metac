#include "metac_scope.h"
#undef SSE2

#ifdef SSE2
#  include <xmmintrin.h>
#endif

metac_scope_table_slot_t* MetaCScopeTable_Lookup(metac_scope_table_t* self,
                                                 uint32_t idHash,
                                                 metac_identifier_ptr_t idPtr)
{

}

void MetaCScope_RegisterIdentifier(metac_scope_t* self,
                                   uint32_t idKey,
                                   metac_identifier_ptr_t idPtr,
                                   metac_node_header_t* node)
{
    metac_scope_table_slot_t* slot =
        MetaCScopeTable_Lookup(&self->ScopeTable, idKey, idPtr);

    slot->Node = node;
}

#ifndef _emptyPointer
#define _emptyPointer 0x1
#define emptyNode (metac_node_header_t*) _emptyPointer
#endif

void MetaCScope_PushLRU(metac_scope_lru_t* lru, uint16_t lw15, metac_scope_table_slot_t* slot)
{
    uint64_t LRUContentHashes = lru->LRUContentHashes;

#if 0
    uint16_t oldContent[4];
    oldContent[0] = LRUContentHashs >> (15 * 0) & 0x7FFF;
    oldContent[1] = LRUContentHashs >> (15 * 1) & 0x7FFF;
    oldContent[2] = LRUContentHashs >> (15 * 2) & 0x7FFF;
    oldContent[3] = LRUContentHashs >> (15 * 3) & 0x7FFF;
#endif

    // maybe the OccupancyFlags don't matter right now
    // uint32_t OccupanyFlags = (LRUContentHashs >> 60);

    LRUContentHashes = (((LRUContentHashes << 15) | lw15) & ((1LLU << (15 * 4)) - 1));

#if 0
    uint16_t newContent[4];
    newContent[0] = LRUContentHashs >> (15 * 0) & 0x7FFF;
    newContent[1] = LRUContentHashs >> (15 * 1) & 0x7FFF;
    newContent[2] = LRUContentHashs >> (15 * 2) & 0x7FFF;
    newContent[3] = LRUContentHashs >> (15 * 3) & 0x7FFF;
#endif

    lru->LRUContentHashes = LRUContentHashes;
#if 0
    __builtin_memmove(&lru->Slots[1], &lru->Slots[0], sizeof(lru->Slots) - sizeof(lru->Slots[0]));
#else
    lru->Slots[3] = lru->Slots[2];
    lru->Slots[2] = lru->Slots[1];
    lru->Slots[1] = lru->Slots[0];
#endif
    lru->Slots[0] = (metac_scope_lru_slot_t){slot->Ptr, slot->Node};
}

/// Returns 0 to keep looking upwards
/// and a vaild pointer if it could be found
metac_node_header_t* MetaCScope_LookupIdentifier(metac_scope_t* self,
                                                 uint32_t identifierKey,
                                                 metac_identifier_ptr_t identifierPtr)
{
    metac_node_header_t* result = 0;
    // first do the LRU Lookup
    uint16_t lw15 = identifierKey & 0x7FFF;

#ifdef SSE2
    _m128i keyMask = _mm_set1_epi16(lw15);
    _m128i LRU_HASHES = _mm_set_epi64(self->LRU.LRUContentHashes, 0);
    _m128i cmp = _mm_cmpeq_epi16(keyMask, LRU_HASHES);
    _mm_shuffle_epi32()

#else
    for(int i = 0; i < 4; i++)
    {
        if (((self->LRU.LRUContentHashes >> (15 * i)) & 0x7FFF) == lw15)
        {
            if (self->LRU.Slots[i].Ptr.v == identifierPtr.v)
                return self->LRU.Slots[i].Node;
        }
    }
#endif

    metac_scope_table_slot_t * slot =
        MetaCScopeTable_Lookup(&self->ScopeTable, identifierKey, identifierPtr);
    if (slot != 0)
    {
        result = slot->Node;
        assert(result != 0);

        MetaCScope_PushLRU(&self->LRU, lw15, slot);
    }

    return result;
}

metac_scope_t* MetaCScope_PushScope(metac_scope_t *self, metac_scope_parent_t scopeOwner)
{
    metac_scope_t* result = 0;



    return result;
}
