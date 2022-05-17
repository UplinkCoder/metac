#ifndef _METAC_SCOPE_H_
#define _METAC_SCOPE_H_
#include "compat.h"
#include "metac_identifier_table.h"

struct metac_sema_declaration_t;


typedef enum metac_scope_parent_kind_t
{
    scope_parent_unknown   = 0x0,

    /// unused for now
    scope_parent_module    = 0x1,

    scope_parent_function  = 0x2,
    scope_parent_aggregate = 0x3,
    scope_parent_stmt      = 0x4,

    scope_parent_extended  = 0x6,
    scope_parent_invalid  = 0x7

    // unused range 9-D 9, A, B, C, D
} metac_scope_parent_kind_t;


typedef struct metac_scope_parent_t
{
    union {
        uint32_t v;
        struct {
            uint32_t Index : 29;
            metac_scope_parent_kind_t Kind : 3;
        };
    };
} metac_scope_parent_t;

#include "metac_sematree.h"


#define SCOPE_PARENT_INDEX(PARENT_INDEX) \
    ((PARENT_INDEX).v & 0x1fffffff)

#define SCOPE_PARENT_KIND(PARENT_INDEX) \
    ((metac_sema_parent_kind_t)((PARENT_INDEX).v >> 29))

#define SCOPE_PARENT_V(KIND, INDEX) \
    ((uint32_t)(((KIND) << 29) | (INDEX)))

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
    uint32_t Serial;
    metac_scope_parent_t Parent;
    metac_scope_table_t ScopeTable;

    metac_scope_lru_t LRU;
} metac_scope_t;

/// Returns 0 to keep looking upwards
/// and a vaild pointer if it could be found
struct metac_node_header_t* MetaCScope_LookupIdentifier(metac_scope_t* self,
                                                        uint32_t identifierKey,
                                                        metac_identifier_ptr_t identifierPtr);

metac_scope_t* MetaCScope_PushScope(metac_scope_t* self, metac_scope_parent_t owner);
#endif // _METAC_SCOPE_H_
