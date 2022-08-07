#include "metac_scope.h"
#include "crc32c.h"
#include "metac_alloc_node.h"

#ifdef SSE2
#  include <xmmintrin.h>
#endif

#include "bsr.h"

void MetaCScopeTable_InitN(metac_scope_table_t* self, uint32_t nMembers, metac_alloc_t* alloc)
{
    // we need to avoid the table being 100% full
    // so allocate 25% more than needed.
    uint32_t extraMembers = (nMembers / 4);
    self->SlotCount_Log2 = LOG2(nMembers + extraMembers);
    const uint32_t maxSlots = (1 << self->SlotCount_Log2);
    self->Arena = AllocateArena(alloc, maxSlots * sizeof(metac_scope_table_slot_t));
    self->Slots = (metac_scope_table_slot_t*) self->Arena->Memory;
    self->SlotsUsed = 0;
}

void MetaCScopeTable_Init(metac_scope_table_t* self, metac_alloc_t* alloc)
{
    MetaCScopeTable_InitN(self, 32, alloc);
}

void MetaCScopeTable_Free(metac_scope_table_t* self)
{
    free(self->Slots);
    self->Slots = 0;
    self->SlotCount_Log2 = 0;
    self->SlotsUsed = 0;
}

metac_scope_table_slot_t* MetaCScopeTable_Lookup(metac_scope_table_t* self,
                                                 const uint32_t idPtrHash,
                                                 metac_identifier_ptr_t idPtr)
{
   TracyCZone(ctx, true);

    const uint32_t slotIndexMask = ((1 << self->SlotCount_Log2) - 1);
    const uint32_t initialSlotIndex = (idPtrHash & slotIndexMask);

    for(
        uint32_t slotIndex = initialSlotIndex;
        (++slotIndex & slotIndexMask) != initialSlotIndex;
    )
    {
        metac_scope_table_slot_t* slot =
            &self->Slots[(slotIndex - 1) & slotIndexMask];
        if (slot->Hash == idPtrHash && idPtr.v == slot->Ptr.v)
        {
            TracyCZoneEnd(ctx);
            return slot;
        }
        else if (slot->Hash == 0)
        {
            break;
        }
    }
    TracyCZoneEnd(ctx);
    return 0;
}
#define existsPointer ((metac_scope_table_slot_t*) 2)
#define fullPointer ((metac_scope_table_slot_t*) 1)

metac_scope_table_slot_t* MetaCScopeTable_Insert(metac_scope_table_t* self,
                                                 metac_identifier_ptr_t idPtr)
{
    uint32_t hash = crc32c_nozero(~0, &idPtr.v, sizeof(idPtr.v));
    const uint32_t slotIndexMask = ((1 << self->SlotCount_Log2) - 1);
    const uint32_t initialSlotIndex = (hash & slotIndexMask);


    for(
        uint32_t slotIndex = initialSlotIndex;
        (++slotIndex & slotIndexMask) != initialSlotIndex;
        //TODO change computation of next slot index
    )
    {
        metac_scope_table_slot_t* slot =
            &self->Slots[(slotIndex - 1) & slotIndexMask];
        if (slot->Hash == 0)
        {
            self->SlotsUsed++;
            slot->Hash = hash;
            slot->Ptr = idPtr;

            return slot;
        }
        else if (slot->Hash == hash && slot->Ptr.v == idPtr.v)
        {
            return existsPointer;
        }
    }

    return fullPointer;
}

/// See scope_insert_error for return values
scope_insert_error_t MetaCScope_RegisterIdentifier(metac_scope_t* self,
                                                   metac_identifier_ptr_t idPtr,
                                                   metac_node_header_t* node)
{
    metac_scope_table_slot_t* slot =
        MetaCScopeTable_Insert(&self->ScopeTable, idPtr);

    if (slot == existsPointer)
    {
        return identifier_exists_already;
    }
    else if (slot == fullPointer)
    {
        return table_full;
    }
    else
    {
        slot->Node = node;

        return success;
    }
}

#define CASE_(ERROR) \
    case ERROR : result = #ERROR; break;

const char* ScopeInsertError_toChars(scope_insert_error_t error)
{
    const char* result;

    switch(error)
    {
        FOREACH_SCOPE_INSERT_ERROR(CASE_)
    }

    return result;
}

#undef CASE_

#ifndef _emptyPointer
#define _emptyPointer 0x1
#define emptyNode (metac_node_header_t*) _emptyPointer
#endif
/*
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

    LRUContentHashes = (((LRUContentHashes << 15) | lw15) & ((1ULL << (15 * 4)) - 1));

#if 0
    uint16_t newContent[4];
    newContent[0] = LRUContentHashs >> (16 * 0) & 0x7FFF;
    newContent[1] = LRUContentHashs >> (16 * 1) & 0x7FFF;
    newContent[2] = LRUContentHashs >> (16 * 2) & 0x7FFF;
    newContent[3] = LRUContentHashs >> (16 * 3) & 0x7FFF;
#endif

    lru->LRUContentHashes = LRUContentHashes;
#if 0
    __builtin_memmove(&lru->Slots[1], &lru->Slots[0], sizeof(lru->Slots) - sizeof(lru->Slots[0]));
#else
    lru->Slots[3] = lru->Slots[2];
    lru->Slots[2] = lru->Slots[1];
    lru->Slots[1] = lru->Slots[0];
#endif
//    lru->Slots[0] = (metac_scope_lru_slot_t){slot->Ptr, slot->Node};
}
 */
/*
 * const char *ssechr(const char *s, char ch)
{
    __m128i zero = _mm_setzero_si128();
    __m128i cx16 = _mm_set1_epi8(ch); // (ch) replicated 16 times.
    while (1) {
        __m128i  x = _mm_loadu_si128((__m128i const *)s);
        unsigned u = _mm_movemask_epi8(_mm_cmpeq_epi8(zero, x));
        unsigned v = _mm_movemask_epi8(_mm_cmpeq_epi8(cx16, x))
                & ~u & (u - 1);
        if (v) return s + __builtin_ctz(v) - 1;
        if (u) return  NULL;
        s += 16;
    }
}
 *
 #ifdef _MSC_VER
#include <intrin.h>

uint32_t __inline ctz( uint32_t value )
{
    DWORD trailing_zero = 0;

    if ( _BitScanForward( &trailing_zero, value ) )
    {
        return trailing_zero;
    }
    else
    {
        // This is undefined, I better choose 32 than 0
        return 32;
    }
}
 */

/// Returns 0 to keep looking upwards
/// and a vaild pointer if it could be found
metac_node_header_t* MetaCScope_LookupIdentifier(metac_scope_t* self,
                                                 uint32_t idPtrHash,
                                                 metac_identifier_ptr_t identifierPtr)
{
    metac_node_header_t* result = 0;
    {
        metac_scope_table_slot_t * slot =
            MetaCScopeTable_Lookup(&self->ScopeTable, idPtrHash, identifierPtr);
        if (slot != 0)
        {
            result = slot->Node;
            assert(result != 0);
        }
    }
    return result;
}
