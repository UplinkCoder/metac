#ifndef _METAC_TYPE_H_
#define _METAC_TYPE_H_

#include "compat.h"
#include "metac_identifier_table.h"
typedef enum metac_type_index_kind_t
{
    type_index_unknown  = 0,

    type_index_struct   = 1,
    type_index_union    = 2,
    type_index_class    = 3,

    type_index_enum     = 4,
    type_index_basic    = 5,

    type_index_ptr      = 6,
    type_index_array    = 7,
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

typedef struct metac_type_t
{
} metac_type_t;

typedef struct metac_struct_field_t
{
    uint32_t Hash;

    metac_type_index_t Type;
    metac_identifier_ptr_t Identifier;

    uint32_t Offset;
} metac_struct_field_t;

typedef struct metac_type_array_t
{
    
} metac_type_array_t;

#endif
