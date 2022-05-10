#ifndef _METAC_TYPE_TABLE_H_
#define _METAC_TYPE_TABLE_H_

#include "compat.h"

typedef enum type_index_kind_t
{
    type_index_unknown = 0,

    type_index_struct   = 1,
    type_index_union    = 2,
    type_index_class    = 3,
    
    type_index_enum     = 4,
    type_index_basic    = 5,
    
    type_index_ptr      = 6,
    type_index_array    = 7,
    type_index_map      = 8,
    
    // unused range 9-D 9, A, B, C, D
    
    type_index_extended = 0xD,
    type_index_invalid  = 0xF,
} type_index_kind_t;

typedef struct type_index_t
{
    union { 
        uint32_t v;
        struct {
            type_index_kind_t Kind : 4;
            uint32_t Index : 28;
        };
    };
} type_index_t;

#define TYPE_INDEX_KIND(TYPE_INDEX) \
    ((TYPE_INDEX).v & 0xF)

#define TYPE_INDEX_INDEX(TYPE_INDEX) \
    ((TYPE_INDEX).v >> 4)

typedef struct metac_type_table_slot_t
{
    uint32_t HashKey;
    type_index_t Index;
} metac_type_table_slot_t;

typedef struct metac_type_table_t
{
    metac_type_table_slot_t* Slots;
 
    uint32_t SlotCount_Log2;
    uint32_t SlotsUsed;

    uint32_t MaxDisplacement;
} metac_type_table_t;

type_index_t GetOrAddArrayType(metac_type_table_t* table, type_index_t elementType, uint32_t dimension);

#endif