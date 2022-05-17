#ifndef _METAC_TYPE_H_
#define _METAC_TYPE_H_

#include "compat.h"
#include "metac_identifier_table.h"

typedef enum metac_type_index_kind_t
{
    type_index_unknown  = 0,

    type_index_basic    = 1,

    type_index_enum     = 2,

    type_index_ptr      = 3,
    type_index_array    = 4,

    type_index_struct   = 5,
    type_index_union    = 6,
    type_index_class    = 7,

    type_index_map      = 8,

    // unused range 9-D 9, A, B, C, D

    type_index_extended = 0xE,
    type_index_invalid  = 0xF
} metac_type_index_kind_t;

typedef struct metac_type_index_t
{
    union {
        uint32_t v;
        struct {
            uint32_t Index : 28;
            metac_type_index_kind_t Kind : 4;
        };
    };
} metac_type_index_t;

#define TYPE_INDEX_INDEX(TYPE_INDEX) \
    ((TYPE_INDEX).v & 0xFFFFFfF)

#define TYPE_INDEX_KIND(TYPE_INDEX) \
    ((metac_type_index_kind_t)((TYPE_INDEX).v >> 28))

#define TYPE_INDEX_V(KIND, INDEX) \
    (((KIND) << 28) | (INDEX))

#define ERROR_TYPE_INDEX_V -1

typedef struct metac_type_aggregate_field_t
{
    uint32_t Hash;
    uint32_t Serial;

    metac_type_index_t Type;
    metac_identifier_ptr_t Identifier;

    uint32_t Offset;
    uint32_t AggregateIndex;
} metac_type_aggregate_field_t;

typedef struct metac_enum_member_t
{
    uint32_t Hash;
    uint32_t Serial;

    metac_type_index_t Type;
    metac_identifier_ptr_t Identifier;

    struct metac_sema_expression_t* Value;
} metac_enum_member_t;

#endif
