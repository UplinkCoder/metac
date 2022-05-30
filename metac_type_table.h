#ifndef _METAC_TYPE_TABLE_H_
#define _METAC_TYPE_TABLE_H_

#include "compat.h"
#include "metac_type.h"
#include <assert.h>

uint32_t EntangleInts(uint32_t a, uint32_t b);
uint32_t UntangleInts(uint32_t tangled);

typedef struct metac_type_table_slot_t
{
    uint32_t HashKey;
    metac_type_index_t TypeIndex;
} metac_type_table_slot_t;

typedef struct metac_type_enum_slot_t
{
    uint32_t HashKey;
    metac_type_index_t TypeIndex;

    uint32_t MemberCount;
    metac_enum_member_t* Members;
} metac_type_enum_slot_t;

typedef struct metac_type_aggregate_slot_t
{
    uint32_t HashKey;
    metac_type_index_t TypeIndex;

    uint32_t FieldCount;
    metac_type_aggregate_field_t* Fields;
} metac_type_aggregate_slot_t;

typedef struct metac_type_functiontype_slot_t
{
    uint32_t HashKey;
    metac_type_index_t TypeIndex;

    metac_type_index_t ReturnType;

    metac_type_index_t* ParameterTypes;
    uint32_t ParameterTypeCount;
} metac_type_functiontype_slot_t;

typedef struct metac_type_ptr_slot_t
{
    uint32_t HashKey;
    metac_type_index_t TypeIndex;

    metac_type_index_t ElementTypeIndex;
} metac_type_ptr_slot_t;


typedef struct metac_type_array_slot_t
{
    uint32_t HashKey;
    metac_type_index_t TypeIndex;

    metac_type_index_t ElementTypeIndex;
    uint32_t Dimension;
} metac_type_array_slot_t;

typedef struct metac_type_typedef_slot_t
{
    uint32_t HashKey;
    metac_type_index_t TypeIndex;

    metac_identifier_ptr_t Identifier;
    metac_type_index_t ElementTypeIndex;
} metac_type_typedef_slot_t;

typedef struct  metac_type_table_t
{
    metac_type_table_slot_t* Slots;

    uint32_t SlotCount_Log2;
    uint32_t SlotsUsed;

    uint32_t MaxDisplacement;
    metac_type_index_kind_t Kind;
} metac_type_table_t;

#define Expression_IsEqual(A, B) \
    (A == B ? true : Expression_IsEqual_(A, B))

bool Expression_IsEqual_(struct metac_sema_expression_t* a,
                         struct metac_sema_expression_t* b);

static inline bool EnumSlotsEqual(const metac_type_table_slot_t* a,
                                  const metac_type_table_slot_t* b)
{
    bool result = true;
    metac_type_enum_slot_t* slotA = cast(metac_type_enum_slot_t*) a;
    metac_type_enum_slot_t* slotB = cast(metac_type_enum_slot_t*) b;
    if (slotA->MemberCount == slotB->MemberCount)
    {
        uint32_t count = slotA->MemberCount;
        for(int i = 0; i < count; i++)
        {
            if (!Expression_IsEqual(slotA->Members[i].Value, slotB->Members[i].Value))
            {
                result = false;
                break;
            }
        }
    }
    else
    {
        result = false;
    }

    return result;
}

static inline bool ArraySlotsEqual(const metac_type_table_slot_t* a,
                                   const metac_type_table_slot_t* b)
{
    metac_type_array_slot_t* slotA = cast(metac_type_enum_slot_t*) a;
    metac_type_array_slot_t* slotB = cast(metac_type_enum_slot_t*) b;
    return (slotA->ElementTypeIndex.v == slotB->ElementTypeIndex.v
         || slotA->Dimension == slotB->Dimension);
}

static inline bool AggregateSlotsEqual(const metac_type_table_slot_t* a,
                                       const metac_type_table_slot_t* b)
{
    bool result = true;
    metac_type_aggregate_slot_t* slotA = cast(metac_type_aggregate_slot_t*) a;
    metac_type_aggregate_slot_t* slotB = cast(metac_type_aggregate_slot_t*) b;
    if (slotA->FieldCount == slotB->FieldCount)
    {
        const uint32_t fieldCount = slotA->FieldCount;
        for(uint32_t i = 0; i < fieldCount; i++)
        {
            const metac_type_aggregate_field_t* fieldsA =
                slotA->Fields;
            const metac_type_aggregate_field_t* fieldsB =
                slotB->Fields;
            if (fieldsA[i].Identifier.v != fieldsB[i].Identifier.v
             || fieldsA[i].Type.v       != fieldsB[i].Type.v)
            {
                result = false;
                break;
            }
        }
    }
    else
    {
        result = false;
    }
    return result;
}

static inline bool PtrSlotsEqual(const metac_type_table_slot_t* a,
                                 const metac_type_table_slot_t* b)
{
    metac_type_ptr_slot_t* slotA = cast(metac_type_ptr_slot_t*) a;
    metac_type_ptr_slot_t* slotB = cast(metac_type_ptr_slot_t*) b;
    return (slotA->ElementTypeIndex.v == slotB->ElementTypeIndex.v);
}

static inline bool FunctiontypeSlotsEqual(const metac_type_table_slot_t* a,
                                          const metac_type_table_slot_t* b)
{
    bool result = true;
    metac_type_functiontype_slot_t* slotA =
        cast(metac_type_functiontype_slot_t*) a;
    metac_type_functiontype_slot_t* slotB =
        cast(metac_type_functiontype_slot_t*) b;


    if (slotA->ReturnType.v == slotB->ReturnType.v &&
        slotA->ParameterTypeCount == slotB->ParameterTypeCount)
    {
        const uint32_t parameterTypeCount = slotA->ParameterTypeCount;
        for(uint32_t i = 0; i < parameterTypeCount; i++)
        {
            const metac_type_index_t* parameterTypesA =
                slotA->ParameterTypes;
            const metac_type_index_t* parameterTypesB =
                slotB->ParameterTypes;
            if (parameterTypesA[i].v != parameterTypesB[i].v)
            {
                result = false;
                break;
            }
        }
    }
    else
    {
        result = false;
    }
    return result;
}

static inline bool TypedefSlotsEqual(const metac_type_table_slot_t* a,
                                     const metac_type_table_slot_t* b)
{
    metac_type_typedef_slot_t* slotA = cast(metac_type_typedef_slot_t*) a;
    metac_type_typedef_slot_t* slotB = cast(metac_type_typedef_slot_t*) b;
    return (slotA->Identifier.v == slotB->Identifier.v
        &&  slotA->ElementTypeIndex.v == slotB->ElementTypeIndex.v);
}


#define METAC_TYPE_TABLE_T(SLOT_TYPE) \
    metac_type_table_##SLOT_TYPE##_t

#define METAC_TYPE_TABLE_KEY_T(SLOT_TYPE) \
    metac_type_## SLOT_TYPE ##_slot_t

#define METAC_TYPE_TABLE_T_DEF(SLOT_TYPE) \
typedef struct  METAC_TYPE_TABLE_T(SLOT_TYPE) \
{ \
    metac_type_## SLOT_TYPE ##_slot_t* Slots; \
    \
    uint32_t SlotCount_Log2; \
    uint32_t SlotsUsed; \
    \
    uint32_t MaxDisplacement; \
    metac_type_index_kind_t Kind; \
} METAC_TYPE_TABLE_T(SLOT_TYPE);

#define FOREACH_TABLE_SLOT_TYPE(M) \
    M(enum) \
    M(array) \
    M(aggregate) \
    M(ptr) \
    M(functiontype) \
    M(typedef)

FOREACH_TABLE_SLOT_TYPE(METAC_TYPE_TABLE_T_DEF)


#define FOREACH_TABLE_MEMBER(M) \
    M(enum, Enum, EnumSlotsEqual) \
    M(array, Array, ArraySlotsEqual) \
    M(aggregate, Struct, AggregateSlotsEqual) \
    M(aggregate, Union,  AggregateSlotsEqual) \
    M(ptr, Ptr, PtrSlotsEqual) \
    M(functiontype, Function, FunctiontypeSlotsEqual) \
    M(typedef, Typedef, TypedefSlotsEqual)

#define DECLARE_ADD(TYPE_NAME, MEMBER_NAME, UNUSED_CMP) \
    void MetaCTypeTable_Add ## MEMBER_NAME ## Type \
    (METAC_TYPE_TABLE_T(TYPE_NAME)* table, \
     const METAC_TYPE_TABLE_KEY_T(TYPE_NAME)* key);

#define DECLARE_GET_OR_EMPTY(TYPE_NAME, MEMBER_NAME, UNUSED_CMP) \
    metac_type_index_t MetaCTypeTable_GetOrEmpty ## MEMBER_NAME ## Type \
    (const METAC_TYPE_TABLE_T(TYPE_NAME)* table, \
     METAC_TYPE_TABLE_KEY_T(TYPE_NAME)* key);

FOREACH_TABLE_MEMBER(DECLARE_GET_OR_EMPTY)

FOREACH_TABLE_MEMBER(DECLARE_ADD)
/*
metac_type_index_t MetaCTypeTable_GetOrAddArrayType(METAC_TYPE_TABLE_T(array)* table,
                                                    uint32_t hash,
                                                    metac_type_array_slot_t* key);
*/
//void MetaCTypeTable_

void TypeTableInitImpl(metac_type_table_t* table, const uint32_t sizeof_slot, metac_type_index_kind_t kind);
#endif
