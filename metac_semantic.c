#include "metac_semantic.h"
#include "compat.h"
#include <assert.h>

bool isBasicType(metac_type_kind_t typeKind)
{
    if (typeKind >= type_void &&  typeKind <= type_unsigned_long_long)
    {
        return true;
    }
    return false;
}

void MetaCSemantic_doDeclSemantic(metac_semantic_state_t* state, metac_declaration_t* decl)
{
    switch(decl->DeclKind)
    {
        case decl_function:
        {
            decl_function_t* f = cast(decl_function_t*) decl;
        } break;
        case decl_variable:
        {
            decl_variable_t* v = cast(decl_variable_t*) decl;
            
        } break;
    }
}

type_index_t MetaCSemantic_GetTypeIndex(metac_semantic_state_t* state,
                                        metac_type_kind_t typeKind,
                                        decl_type_t* type)
{
    if (isBasicType(typeKind))
    {
        return (type_index_t){(uint32_t) (typeKind + 1)};
    }
    
    return (type_index_t) {0};
}

type_index_t MetaCSemantic_GetArrayTypeOf(metac_semantic_state_t* state,
                                          type_index_t elementTypeIndex,
                                          uint32_t dimension)
{
    type_index_t result = 
        GetOrAddArrayType(state->TypeTable, elementTypeIndex, dimension);
    return result;
}

void MetaCSemantic_doExprSemantic(metac_semantic_state_t* state,
                                  metac_expression_t* expr)
{
    switch(expr->Kind)
    {
        case exp_invalid:
            assert(0);

        case exp_char :
            expr->TypeIndex = MetaCSemantic_GetTypeIndex(state, type_char, 0);
        break;
        case exp_string :
            expr->TypeIndex = MetaCSemantic_GetArrayTypeOf(state, 
                MetaCSemantic_GetTypeIndex(state, type_char, 0),
                LENGTH_FROM_STRING_KEY(expr->StringKey));
        break;
    }
}