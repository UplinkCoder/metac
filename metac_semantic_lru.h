#ifndef _METAC_SEMANTIC_LRU_H_
#define _METAC_SEMANTIC_LRU_H_

#include "compat.h"
#include "metac_simd.h"

#include "metac_identifier_table.h"
#include "metac_parsetree.h"

#define LRU_HASH_MASK        0xffc0 // (((1 << 10) -1) << 6)
#define LRU_SCOPE_LEVEL_MASK 0x003f // ((1 << 6) - 1)

#pragma pack(push, 1)
typedef struct metac_semantic_lru_slot_t
{
    metac_identifier_ptr_t Ptr;
    /// Node may be 0
    metac_node_header_t* Node;
} metac_semantic_lru_slot_t;
#pragma pack(pop)


typedef struct metac_semantic_lru_t
{
    int16x8_t LRUContentHashes;
    metac_semantic_lru_slot_t Slots[8];
} metac_semantic_lru_t ;

#endif
