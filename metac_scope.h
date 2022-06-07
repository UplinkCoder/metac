#ifndef _METAC_SCOPE_H_
#define _METAC_SCOPE_H_
#include "compat.h"
#include "metac_identifier_table.h"
#include "metac_node.h"

struct metac_sema_declaration_t;

typedef enum metac_scope_parent_kind_t
{
    scope_parent_unknown   = 0x0,

    /// unused for now
    scope_parent_module    = 0x1,

    scope_parent_function  = 0x2,
    scope_parent_struct    = 0x3,
    scope_parent_statement = 0x4,
    scope_parent_block     = 0x5,

    scope_parent_union     = 0x6,

    scope_parent_extended  = 0x7,

    // unused range 9-D 9, A, B, C, D
    scope_parent_invalid  = 0xF

} metac_scope_parent_kind_t;


typedef struct metac_scope_parent_t
{
    union {
        uint32_t v;
        struct {
            uint32_t Index : 28;
            metac_scope_parent_kind_t Kind : 4;
        };
    };
} metac_scope_parent_t;

#ifndef DEFINE_MEMBERS
#define DEFINE_MEMBERS(M) \
    M,
#endif

#define FOREACH_SCOPE_INSERT_ERROR(M) \
    M(success) \
    M(identifier_exists_already) \
    M(table_full) \
    M(no_scope)


typedef enum scope_insert_error_t
{
    FOREACH_SCOPE_INSERT_ERROR(DEFINE_MEMBERS)
} scope_insert_error_t;

#undef DEFINE_MEMBERS


#define SCOPE_PARENT_INDEX(PARENT_INDEX) \
    ((PARENT_INDEX).v & 0xfffffff)

#define SCOPE_PARENT_KIND(PARENT_INDEX) \
    ((metac_sema_parent_kind_t)((PARENT_INDEX).v >> 28))

#define SCOPE_PARENT_V(KIND, INDEX) \
    ((uint32_t)(((KIND) << 28) | (INDEX)))

typedef struct metac_scope_ptr_t
{
    uint32_t v;
} metac_scope_ptr_t;

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


typedef enum metac_scope_flags_t
{
    scope_flag_none,

    scope_flag_temporary = (1 << 0),
    /// mounted scopes are temporary scopes
    /// which get mounted over the current scope
    scope_flag_mounted   = (1 << 1),

    scope_flag_max = (1 << 2)
} metac_scope_flags_t;

typedef struct metac_scope_t
{
    metac_scope_flags_t scopeFlags;
    metac_scope_parent_t Owner;
    struct metac_scope_t* Parent;
    union {
        metac_scope_table_t ScopeTable;
        const struct metac_scope_t* MountedScope;
    } ;
    uint32_t Serial;
} metac_scope_t;

/// Returns 0 to keep looking upwards
/// and a vaild pointer if it could be found
metac_node_header_t* MetaCScope_LookupIdentifier(metac_scope_t* self,
                                                 uint32_t idPtrHash,
                                                 metac_identifier_ptr_t identifierPtr);

/// Returns true on insertion and false if the table was full
scope_insert_error_t MetaCScope_RegisterIdentifier(metac_scope_t* self,
                                                   metac_identifier_ptr_t idPtr,
                                                   metac_node_t node);

#endif // _METAC_SCOPE_H_
