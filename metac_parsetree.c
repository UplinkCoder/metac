#include "metac_parsetree.h"
#include "crc32c.h"


int MetaCDeclaration_Walk_Debug(metac_declaration_t* decl, const char* fn_name, walker_function_t walker_fn, void* ctx)
{
    // make sure the context confusion cookie is set
    assert((*(uint32_t*) ctx) == crc32c(~0, fn_name, strlen(fn_name)));

    return MetaCDeclaration_Walk_Real(decl, walker_fn, ctx);
}

int MetaCDeclaration_Walk_Real(metac_declaration_t* decl, walker_function_t walker_fn, void* ctx)
{
#define walker_fn(DECL, CTX) \
    walker_fn((metac_node_t*) DECL, CTX)

    int result = walker_fn((metac_node_t*)decl, ctx);

    if (result)
        return result;

    switch(decl->DeclKind)
    {
        //default 0:
        case decl_min:
        case decl_max:
            assert(0);

        case decl_variable:
        {
            decl_variable_t* variable = (decl_variable_t*) decl;
            result = walker_fn(variable->VarType, ctx);
            if (result)
                return result;
        } break;
        case decl_field:
        {
            decl_field_t* field = (decl_field_t*) decl;
            while(field != _emptyPointer)
            {
                result = walker_fn(field->Field, ctx);
                if (result)
                    return result;
                field = field->Next;
            }
        } break;
        case decl_parameter:
        {
            decl_parameter_t* parameter = (decl_parameter_t*) decl;
            while(parameter != _emptyPointer)
            {
                result = walker_fn(parameter->Parameter, ctx);
                if (!result)
                    return result;
                parameter = parameter->Next;
            }
        } break;
        case decl_enum_member:
        {
            decl_enum_member_t* enum_member = (decl_enum_member_t*) decl;
        } break;
        case decl_type:
        {
            decl_type_t* type = (decl_type_t*) decl;
        } break;
        case decl_type_struct:
        {
            decl_type_struct_t* type_struct = (decl_type_struct_t*) decl;
        } break;
        case decl_type_union:
        {
            decl_type_union_t* type_union = (decl_type_union_t*) decl;
        } break;
        case decl_type_enum:
        {
            decl_type_enum_t* type_enum = (decl_type_enum_t*) decl;
        } break;
        case decl_type_array:
        {
            decl_type_array_t* type_array = (decl_type_array_t*) decl;
        } break;
        case decl_type_ptr:
        {
            decl_type_ptr_t* type_ptr = (decl_type_ptr_t*) decl;
        } break;
        case decl_type_functiontype:
        {
            decl_type_functiontype_t* type_functiontype = (decl_type_functiontype_t*) decl;
        } break;
        case decl_typedef:
        {
            decl_typedef_t* typedef_ = (decl_typedef_t*) decl;
        } break;
        case decl_function:
        {
            decl_function_t* function = (decl_function_t*) decl;
        } break;
    }
}
