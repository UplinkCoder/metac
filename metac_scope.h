#ifndef _METAC_SCOPE_H_
#define _METAC_SCOPE_H_
#include "compat.h"
#include "metac_identifier_table.h"
#include "metac_sematree.h"
typedef struct metac_scope_ptr_t
{
    uint32_t v;
} metac_scope_ptr_t;

#pragma pack(push, 1)
typedef struct metac_scope_lru_slot_t
{
    metac_identifier_ptr_t Ptr;
    /// Node may be 0
    struct metac_node_header_t* Node;
} metac_scope_lru_slot_t;
#pragma pack(pop)

typedef struct metac_scope_table_slot_t
{
    uint32_t Hash;
    metac_identifier_ptr_t Ptr;
    /// Node may be 0
    struct metac_node_header_t* Node;
} metac_scope_table_slot_t;


typedef struct metac_scope_table_t
{
    metac_scope_table_slot_t* Slots;
    uint32_t SlotCount_Log2;
    uint32_t SlotsUsed;   
} metac_scope_table_t;

typedef struct metac_scope_lru_t
{
    /// has the lower 15 bit of the id hashes
    /// contained in the LRU
    /// the upper 4 bit sigifiy if the Identifier came from the parent
    /// and therefore may be overwritten
    uint64_t LRUContentHashes;
    metac_scope_lru_slot_t Slots[4];
} metac_scope_lru_t;

typedef struct metac_scope_t
{
    metac_scope_ptr_t Parent;
    metac_identifier_table_t ScopeTable;
    
    metac_scope_lru_t LRU;
} metac_scope_t;

/// Returns 0 to keep looking upwards
/// and a vaild pointer if it could be found
metac_node_header_t* MetaCScope_LookupIdentifier(metac_scope_t* self,
                                                 uint32_t identifierKey,
                                                 metac_identifier_ptr_t identifierPtr);

metac_scope_t* MetaCScope_PushScope(metac_scope_t* self);
#endif // _METAC_SCOPE_H_