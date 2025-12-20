#ifndef _METAC_TYPE_H_
#define _METAC_TYPE_H_

#include "../os/compat.h"
#include "../parser/metac_identifier_table.h"
#include "../semantic/metac_scope.h"
#include "../parser/metac_lexer.h" // for location
#include "../parser/metac_parsetree.h"
#include "../parser/metac_node.h"

#ifndef AT
#define AT(...)
#endif

#define FOREACH_TYPE_INDEX_KIND(M) \
    M(type_index_unknown       , 0x0) \
    M(type_index_basic         , 0x1) \
    M(type_index_enum          , 0x2) \
    M(type_index_ptr           , 0x3) \
    M(type_index_array         , 0x4) \
    M(type_index_struct        , 0x5) \
    M(type_index_union         , 0x6) \
    M(type_index_class         , 0x7) \
    M(type_index_map           , 0x8) \
    M(type_index_functiontype  , 0x9) \
    M(type_index_typedef       , 0xA) \
    M(type_index_tuple         , 0xB) \
    M(type_index_template      , 0xC) \
    \
    M(type_index_unresolved    , 0xD) \
    M(type_index_extended      , 0xE) \
    M(type_index_invalid       , 0xF)


#define TYPE_INDEX_MEMBER(KIND, VALUE) \
    KIND = VALUE,

typedef enum metac_type_index_kind_t
{
    FOREACH_TYPE_INDEX_KIND(TYPE_INDEX_MEMBER)
    dummy = 0xf
} metac_type_index_kind_t;

#undef TYPE_INDEX_MEMBER

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
    ((TYPE_INDEX).v & 0x0FFFFFFF)

#define TYPE_INDEX_KIND(TYPE_INDEX) \
    ((metac_type_index_kind_t)((TYPE_INDEX).v >> 28))

#define TYPE_INDEX_V(KIND, INDEX) \
    (((KIND) << 28) | (INDEX))

#define ERROR_TYPE_INDEX_V -1

#define METAC_TYPE_HEADER \
    metac_decl_kind_t Kind; \
    metac_location_ptr_t LocationIdx; \
    uint32_t Hash; \
    uint32_t Serial;

typedef struct metac_type_header_t
{
    METAC_TYPE_HEADER
    decl_type_t* Origin;
} metac_type_header_t;

typedef metac_type_header_t* metac_type_t;

typedef struct metac_type_aggregate_field_t
{
    metac_node_header_t Header;

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

    metac_type_index_t TypeIndex;

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

    struct metac_sema_expr_t* Value;
} metac_enum_member_t;

typedef struct metac_type_enum_t
{
    metac_type_header_t Header;

    metac_type_index_t TypeIndex;

    metac_identifier_ptr_t Identifier;

    metac_type_index_t BaseType;

    metac_enum_member_t* Members;

    uint32_t MemberCount;
} metac_type_enum_t;

typedef struct metac_type_basic_t
{
    metac_type_header_t Header;

    metac_type_index_t TypeIndex;
} metac_type_basic_t;

typedef struct metac_type_array_t
{
    metac_type_header_t Header;

    metac_type_index_t TypeIndex;

    metac_type_index_t ElementType;

    uint32_t Dim;
} metac_type_array_t;

typedef struct metac_type_ptr_t
{
    metac_type_header_t Header;

    metac_type_index_t TypeIndex;

    metac_type_index_t ElementType;
} metac_type_ptr_t;

typedef struct metac_type_functiontype_t
{
    metac_type_header_t Header;

    metac_type_index_t TypeIndex;

    metac_type_index_t YieldType;

    metac_type_index_t ReturnType;

    metac_type_index_t* ParameterTypes;

    metac_identifier_ptr_t* ParameterNames;

    uint32_t ParameterTypeCount;
} metac_type_functiontype_t;

typedef struct metac_type_typedef_t
{
    metac_type_header_t Header;

    metac_type_index_t TypeIndex;

    metac_type_index_t Type;

    metac_identifier_ptr_t Identifier;
} metac_type_typedef_t;

typedef struct metac_type_tuple_t
{
    metac_type_header_t Header;

    metac_type_index_t TypeIndex;

    metac_type_index_t* TypeIndicies;

    uint32_t TypeCount;
} metac_type_tuple_t;

typedef struct metac_type_template_t
{
    metac_type_header_t Header;

    metac_type_index_t TypeIndex;

    metac_decl_t* Symbol;

    metac_expr_t* Arguments;

    uint32_t ArgumentCount;
} metac_type_template_t;
#endif
