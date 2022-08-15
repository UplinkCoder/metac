#ifndef _METAC_TYPE_H_
#define _METAC_TYPE_H_

#include "compat.h"
#include "metac_identifier_table.h"
#include "metac_scope.h"
#include "metac_node.h"

#ifndef AT
#define AT(...)
#endif

typedef enum metac_type_index_kind_t
{
    type_index_unknown       = 0x0,

    type_index_basic         = 0x1,

    type_index_enum          = 0x2,

    type_index_ptr           = 0x3,
    type_index_array         = 0x4,

    type_index_struct        = 0x5,
    type_index_union         = 0x6,
    type_index_class         = 0x7,

    type_index_map           = 0x8,

    type_index_functiontype  = 0x9,

    type_index_typedef       = 0xA,

    type_index_tuple         = 0xB,

    // unused range C-D C, D

    type_index_extended      = 0xE,
    type_index_invalid       = 0xF
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

#define METAC_TYPE_HEADER \
    metac_declaration_kind_t Kind; \
    uint32_t LocationIdx; \
    uint32_t Hash; \
    uint32_t Serial;

typedef struct metac_type_header_t
{
    METAC_TYPE_HEADER
} metac_type_header_t;

typedef metac_type_header_t* metac_type_t;

typedef struct metac_type_aggregate_field_t
{
    metac_type_header_t Header;

    metac_type_index_t Type;
    uint16_t Index;
    uint16_t Offset;

    metac_identifier_ptr_t Identifier;
    /// the aggregate this field is a part of
    metac_type_index_t AggregateType;
} metac_type_aggregate_field_t;

typedef struct metac_type_aggregate_t
{
    metac_type_header_t Header;

    metac_identifier_ptr_t Identifier;

    uint16_t FieldCount;
    uint16_t Alignment;

    uint32_t Size;

    AT(NO_SQL) metac_scope_t* Scope;

    AT(NO_SQL) metac_type_aggregate_field_t* Fields;
} metac_type_aggregate_t;

typedef struct metac_enum_member_t
{
    metac_type_header_t Header;

    metac_type_index_t Type;
    metac_identifier_ptr_t Identifier;

    struct metac_sema_expression_t* Value;
} metac_enum_member_t;

typedef struct metac_type_enum_t
{
    metac_type_header_t Header;

    metac_identifier_ptr_t Name;

    metac_enum_member_t* Members;

    uint32_t MemberCount;
} metac_type_enum_t;

typedef struct metac_type_array_t
{
    metac_type_header_t Header;

    metac_type_index_t ElementType;

    uint32_t Dim;
} metac_type_array_t;

typedef struct metac_type_ptr_t
{
    metac_type_header_t Header;

    metac_type_index_t ElementType;
} metac_type_ptr_t;

typedef struct metac_type_functiontype_t
{
    metac_type_header_t Header;

    metac_type_index_t ReturnType;

    metac_type_index_t* ParameterTypes;

    uint32_t ParameterTypeCount;

    metac_identifier_ptr_t* ParameterNames;
} metac_type_functiontype_t;

typedef struct metac_type_typedef_t
{
    metac_type_header_t Header;

    metac_type_index_t Type;

    metac_identifier_ptr_t Identifier;
} metac_type_typedef_t;

typedef struct metac_type_tuple_t
{
    metac_type_header_t Header;

    metac_type_index_t* typeIndicies;

    uint32_t typeCount;
} metac_type_tuple_t;

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
// DON'T CHANGE THE ORDER FROM HERE
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
// ALSO DON'T CHANGE ANYTHING FROM HERE
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

    type_tuple,

    type_modifiers,

    type_max
} metac_type_kind_t;
#endif
