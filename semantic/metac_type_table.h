#ifndef _METAC_TYPE_TABLE_H_
#define _METAC_TYPE_TABLE_H_

#include "../os/compat.h"
#include "metac_type.h"

uint32_t EntangleInts(uint32_t a, uint32_t b);
uint32_t UntangleInts(uint32_t tangled);

typedef struct metac_type_table_slot_t
{
    METAC_TYPE_HEADER
    decl_type_t* origin;
} metac_type_table_slot_t;

typedef struct  metac_type_table_t
{
    metac_type_table_slot_t* Slots;

    uint32_t SlotCount_Log2;
    uint32_t SlotsUsed;

    uint32_t MaxDisplacement;
    metac_type_index_kind_t Kind;
} metac_type_table_t;

#define Expr_IsEqual(A, B) \
    (A == B ? true : Expr_IsEqual_(A, B))

bool Expr_IsEqual_(const struct metac_sema_expr_t* a,
                         const struct metac_sema_expr_t* b);

static inline const bool EnumSlotsEqual(const metac_type_table_slot_t* a,
                                        const metac_type_table_slot_t* b)
{
    bool result = true;
    metac_type_enum_t* slotA = cast(metac_type_enum_t*) a;
    metac_type_enum_t* slotB = cast(metac_type_enum_t*) b;

    if (slotA->Identifier.v != slotB->Identifier.v)
    {
        result = false;
        goto Lret;
    }

    if (slotA->MemberCount == slotB->MemberCount)
    {
        uint32_t count = slotA->MemberCount;
        for(uint32_t i = 0; i < count; i++)
        {
            if (slotA->Members[i].Identifier.v != slotB->Members[i].Identifier.v
             || !Expr_IsEqual(slotA->Members[i].Value, slotB->Members[i].Value))
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
Lret:
    return result;
}

static inline const bool ArraySlotsEqual(const metac_type_table_slot_t* a,
                                         const metac_type_table_slot_t* b)
{
    metac_type_array_t* slotA = cast(metac_type_array_t*) a;
    metac_type_array_t* slotB = cast(metac_type_array_t*) b;
    return (slotA->ElementType.v == slotB->ElementType.v
         && slotA->Dim == slotB->Dim);
}

static inline const bool AggregateSlotsEqual(const metac_type_table_slot_t* a,
                                             const metac_type_table_slot_t* b)
{
    bool result = true;

    metac_type_aggregate_t* slotA =
        cast(metac_type_aggregate_t*) a;

    metac_type_aggregate_t* slotB =
        cast(metac_type_aggregate_t*) b;

    if (slotA->FieldCount == slotB->FieldCount)
    {
        const uint32_t fieldCount = slotA->FieldCount;
        for(uint32_t i = 0; i < fieldCount; i++)
        {
            const metac_type_aggregate_field_t* fieldsA =
                slotA->Fields + i;
            const metac_type_aggregate_field_t* fieldsB =
                slotB->Fields + i;

            if (fieldsA->Identifier.v != fieldsB->Identifier.v
             || fieldsA->Type.v       != fieldsB->Type.v)
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

static inline const bool PtrSlotsEqual(const metac_type_table_slot_t* a,
                                       const metac_type_table_slot_t* b)
{
    metac_type_ptr_t* slotA = cast(metac_type_ptr_t*) a;
    metac_type_ptr_t* slotB = cast(metac_type_ptr_t*) b;
    return (slotA->ElementType.v == slotB->ElementType.v);
}

static inline const bool FunctiontypeSlotsEqual(const metac_type_table_slot_t* a,
                                                const metac_type_table_slot_t* b)
{
    bool result = true;
    metac_type_functiontype_t* slotA =
        cast(metac_type_functiontype_t*) a;
    metac_type_functiontype_t* slotB =
        cast(metac_type_functiontype_t*) b;


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

static inline const bool TypedefSlotsEqual(const metac_type_table_slot_t* a,
                                           const metac_type_table_slot_t* b)
{
    metac_type_typedef_t* slotA = cast(metac_type_typedef_t*) a;
    metac_type_typedef_t* slotB = cast(metac_type_typedef_t*) b;
    return (slotA->Identifier.v == slotB->Identifier.v
        &&  slotA->Type.v == slotB->Type.v);
}


static inline const bool TupleSlotsEqual(const metac_type_table_slot_t* a,
                                         const metac_type_table_slot_t* b)
{
   metac_type_tuple_t* slotA = cast(metac_type_tuple_t*) a;
   metac_type_tuple_t* slotB = cast(metac_type_tuple_t*) b;
   bool result = false;

   if (slotA->TypeCount == slotB->TypeCount)
   {
       result = true;
       const uint32_t typeCount = slotA->TypeCount;
       for(uint32_t i = 0; i < typeCount; i++)
       {
           if (slotA->TypeIndicies[i].v != slotB->TypeIndicies[i].v)
           {
               result = false;
               break;
           }
       }
   }

   return result;
}

#define METAC_TYPE_TABLE_T(SLOT_TYPE) \
    metac_type_table_##SLOT_TYPE##_t

#define METAC_TYPE_TABLE_KEY_T(SLOT_TYPE) \
    metac_type_## SLOT_TYPE ##_t

#define METAC_TYPE_TABLE_T_DEF(SLOT_TYPE) \
typedef struct  METAC_TYPE_TABLE_T(SLOT_TYPE) \
{ \
    metac_type_## SLOT_TYPE ##_t* Slots; \
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
    M(typedef) \
    M(tuple)

FOREACH_TABLE_SLOT_TYPE(METAC_TYPE_TABLE_T_DEF)


#define FOREACH_TABLE_MEMBER(M) \
    M(enum, Enum, EnumSlotsEqual) \
    M(array, Array, ArraySlotsEqual) \
    M(aggregate, Struct, AggregateSlotsEqual) \
    M(aggregate, Union,  AggregateSlotsEqual) \
    M(ptr, Ptr, PtrSlotsEqual) \
    M(functiontype, Function, FunctiontypeSlotsEqual) \
    M(typedef, Typedef, TypedefSlotsEqual) \
    M(tuple, Tuple, TupleSlotsEqual)

#define DECLARE_ADD(TYPE_NAME, MEMBER_NAME, UNUSED_CMP) \
    metac_type_index_t MetaCTypeTable_Add ## MEMBER_NAME ## Type \
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

void TypeTableInitImpl(metac_type_table_t* table, const uint32_t sizeof_slot,
                       metac_type_index_kind_t kind, metac_alloc_t* alloc);
#endif
