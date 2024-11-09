#ifndef _METAC_SCOPE_H_
#define _METAC_SCOPE_H_
#include "../os/compat.h"
#include "../parser/metac_identifier_table.h"
#include "../parser/metac_node.h"
#include "../os/metac_alloc.h"

struct metac_sema_decl_t;

#define FOREACH_SCOPE_OWNER(M) \
    M(scope_owner_unknown   , 0x0) \
    /* unused for now */ \
    M(scope_owner_module    , 0x1) \
    M(scope_owner_function  , 0x2) \
    M(scope_owner_struct    , 0x3) \
    M(scope_owner_statement , 0x4) \
    M(scope_owner_block     , 0x5) \
    M(scope_owner_union     , 0x6) \
    M(scope_owner_extended  , 0x7) \
    M(scope_owner_enum      , 0x9) \
    M(scope_owner_template  , 0xA) \
    /* 0xF is for invalid reserved. */ \
    M(scope_owner_invalid  , 0xF)

#define DEF_MEMBER(NAME, VALUE) \
    NAME = VALUE,

typedef enum metac_scope_owner_kind_t
{
    FOREACH_SCOPE_OWNER(DEF_MEMBER)
} metac_scope_owner_kind_t;


typedef struct metac_scope_owner_t
{
    union {
        uint32_t v;
        struct {
            uint32_t Index : 28;
            metac_scope_owner_kind_t Kind : 4;
        };
    };
} metac_scope_owner_t;

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


#define SCOPE_OWNER_INDEX(PARENT_INDEX) \
    ((PARENT_INDEX).v & 0xfffffff)

#define SCOPE_OWNER_KIND(PARENT_INDEX) \
    ((metac_scope_owner_kind_t)((PARENT_INDEX).v >> 28))

#define SCOPE_OWNER_V(KIND, INDEX) \
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
    arena_ptr_t Arena;
    metac_alloc_t* Alloc;

    bool AllowOverride;
} metac_scope_table_t;

void MetaCScopeTable_InitN(metac_scope_table_t *self,
                           uint32_t nMembers,
                           metac_alloc_t *alloc);

void MetaCScopeTable_Init(metac_scope_table_t *self, metac_alloc_t *alloc);

void MetaCScopeTable_Free(metac_scope_table_t *self);

metac_scope_table_slot_t* MetaCScopeTable_Lookup(metac_scope_table_t* self,
                                                 const uint32_t idPtrHash,
                                                 metac_identifier_ptr_t idPtr);

metac_scope_table_slot_t* MetaCScopeTable_Insert(metac_scope_table_t* self,
                                                 metac_identifier_ptr_t idPtr);

typedef enum metac_scope_flags_t
{
    scope_flag_none,

    scope_flag_temporary = (1 << 0),
    /// mounted scopes are temporary scopes
    /// which get mounted over the current scope
    scope_flag_mounted   = (1 << 1),

    /// a closed scope cannot be added to anymore
    scope_flag_closed    = (1 << 2),

    scope_flag_max = (1 << 3)
} metac_scope_flags_t;

typedef struct metac_scope_t
{
    metac_scope_flags_t ScopeFlags;
    metac_scope_owner_t Owner;
    struct metac_scope_t* Parent;
    union {
        metac_scope_table_t ScopeTable;
        const struct metac_scope_t* MountedScope;
    } ;
    uint32_t Serial;
    /// when true the scope can no longer be added to
    bool Closed;
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
