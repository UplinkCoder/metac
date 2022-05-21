#ifndef _METAC_TYPE_H_
#define _METAC_TYPE_H_

#include "compat.h"
#include "metac_identifier_table.h"

typedef enum metac_type_index_kind_t
{
    type_index_unknown  = 0x0,

    type_index_basic    = 0x1,

    type_index_enum     = 0x2,

    type_index_ptr      = 0x3,
    type_index_array    = 0x4,

    type_index_struct   = 0x5,
    type_index_union    = 0x6,
    type_index_class    = 0x7,

    type_index_map      = 0x8,

    type_index_function = 0x9,

    // unused range A-D A, B, C, D

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


typedef enum metac_type_kind_t
{
    type_invalid,

    type_struct,
    type_union,
    type_class,
    type_enum,

    type_typedef,
    type_functiontype,

    type_auto,
// DoNT CHANGE THE ORDER FROM HERE
// XXX: Order needs to be in sync with the type tokens in metac_lexer.h
    type_void,
    type_bool,
    type_char,
    type_short,
    type_int,
    type_long,
    type_size_t,

    type_float,
    type_double,
//TO HERE
    type_long_long,
    type_long_double,
// ALSO DON"T CHANGE ANYTHIBG FROM HERE
    type_unsigned_char,
    type_unsigned_short,
    type_unsigned_int,
    type_unsigned_long,
//TO HERE
    type_unsigned_long_long,


    type_type,
    type_identifier,

    type_ptr,
    type_array,

    type_map,

    type_max
} metac_type_kind_t;
#endif
