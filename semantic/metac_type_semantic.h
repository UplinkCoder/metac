#ifndef _METAC_TYPE_SEMANITC_H_
#define _METAC_TYPE_SEMANITC_H_
typedef struct MetaCSemantic_doTypeSemantic_Fiber_t
{
    metac_sema_state_t* Sema;
    decl_type_t* Type;
    metac_type_index_t Result;
} MetaCSemantic_doTypeSemantic_Fiber_t;

typedef struct metac_size_computer_t
{
    /// CurrentSize.
    uint32_t CurrentSize;
    /// CurrentMaxAlignment
    uint32_t MaxAlignment;

#ifndef NDEBUG
    uint32_t Debug_FieldIndex;
#endif
} metac_size_computer_t;

void MetaCSizeComputer_Init(metac_size_computer_t* self);

void MetaCSizeComputer_BeginSizeOf(metac_size_computer_t* self);

uint32_t MetaCSizeComputer_MemberType(metac_size_computer_t* self,
                                      metac_sema_state_t* sema,
                                      metac_type_index_t memberType);

uint32_t MetaCSizeComputer_FinishSizeOf(metac_size_computer_t* self);

sema_decl_type_t* MetaCSemantic_GetTypeNode(metac_sema_state_t* self,
                                            metac_type_index_t typeIndex);

metac_type_index_t MetaCSemantic_GetTypeIndex(metac_sema_state_t* state,
                                              metac_type_kind_t typeKind,
                                              decl_type_t* type);

metac_type_index_t  MetaCSemantic_CommonSubtype(metac_sema_state_t* state,
                                                metac_type_index_t a, metac_type_index_t b);

metac_type_index_t MetaCSemantic_GetElementType(metac_sema_state_t* self,
                                                metac_type_index_t typeIndex);

metac_type_index_t MetaCSemantic_GetArrayTypeOf(metac_sema_state_t* state,
                                                metac_type_index_t elementTypeIndex,
                                                uint32_t dimension);

metac_type_index_t MetaCSemantic_GetPtrTypeOf(metac_sema_state_t* self,
                                              metac_type_index_t elementTypeIndex);

uint32_t ComputeStructSize(metac_sema_state_t* self, metac_type_index_t* typeBegin,
    uint32_t nTypes, metac_type_index_t * (*Next) (metac_type_index_t*));

bool MetaCSemantic_ComputeStructLayout(metac_sema_state_t* self,
                                       decl_type_struct_t* agg,
                                       metac_type_aggregate_t* semaAgg);

metac_type_index_t MetaCSemantic_TypeSemantic(metac_sema_state_t* self,
                                              decl_type_t* type);
#endif //_METAC_TYPE_SEMANTIC_H_
