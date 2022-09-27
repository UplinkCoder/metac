#ifndef _METAC_TYPE_SEMANITC_H_
#define _METAC_TYPE_SEMANITC_H_
typedef struct MetaCSemantic_doTypeSemantic_Fiber_t
{
    metac_semantic_state_t* Sema;
    decl_type_t* Type;
    metac_type_index_t Result;
} MetaCSemantic_doTypeSemantic_Fiber_t;

metac_type_index_t MetaCSemantic_GetTypeIndex(metac_semantic_state_t* state,
                                              metac_type_kind_t typeKind,
                                              decl_type_t* type);

metac_type_index_t  MetaCSemantic_CommonSubtype(metac_semantic_state_t* state,
                                                metac_type_index_t a, metac_type_index_t b);

metac_type_index_t MetaCSemantic_GetElementType(metac_semantic_state_t* self,
                                                metac_type_index_t typeIndex);

metac_type_index_t MetaCSemantic_GetArrayTypeOf(metac_semantic_state_t* state,
                                                metac_type_index_t elementTypeIndex,
                                                uint32_t dimension);

metac_type_index_t MetaCSemantic_GetPtrTypeOf(metac_semantic_state_t* self,
                                              metac_type_index_t elementTypeIndex);

uint32_t ComputeStructSize(metac_semantic_state_t* self, metac_type_index_t* typeBegin,
    uint32_t nTypes, metac_type_index_t * (*Next) (metac_type_index_t*));

bool MetaCSemantic_ComputeStructLayout(metac_semantic_state_t* self,
                                       decl_type_struct_t* agg,
                                       metac_type_aggregate_t* semaAgg);
#endif //_METAC_TYPE_SEMANTIC_H_
