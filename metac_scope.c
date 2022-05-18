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

    LRUContentHashes = (((LRUContentHashes << 15) | lw15) & ((1LLU << (15 * 4)) - 1));

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
    lru->Slots[0] = (metac_scope_lru_slot_t){slot->Ptr, slot->Node};
}
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
                                                 uint32_t identifierKey,
                                                 metac_identifier_ptr_t identifierPtr)
{
    metac_node_header_t* result = 0;
    // first do the LRU Lookup
    uint16_t lw15 = identifierKey & 0x7FFF;

    uint32_t startSearch = 0;

#ifdef SSE2
    __m128i keyMask = _mm_set1_epi16(lw15);
    __m128i lruHashes = (__m128i)_mm_loadu_pd(&self->LRU.LRUContentHashes);
    __m128i cmp = _mm_cmpeq_epi16(keyMask, lruHashes);
    uint32_t searchResult = _mm_movemask_epi8(cmp);
    if (searchResult == 0)
        goto LtableLookup;
    startSearch = __builtin_ffs(searchResult);
#endif

    for(int i = startSearch; i < 4; i++)
    {
        if (((self->LRU.LRUContentHashes >> (16 * i)) & 0x7FFF) == lw15)
        {
            if (self->LRU.Slots[i].Ptr.v == identifierPtr.v)
                return self->LRU.Slots[i].Node;
        }
    }
LtableLookup:
    {
        metac_scope_table_slot_t * slot =
            MetaCScopeTable_Lookup(&self->ScopeTable, identifierKey, identifierPtr);
        if (slot != 0)
        {
            result = slot->Node;
            assert(result != 0);

            MetaCScope_PushLRU(&self->LRU, lw15, slot);
        }
    }
    return result;
}

metac_scope_t* MetaCScope_PushScope(metac_scope_t *self, metac_scope_parent_t scopeOwner)
{
    metac_scope_t* result = 0;



    return result;
}
#undef _emptyPointer
