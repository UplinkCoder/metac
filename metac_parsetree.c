#include "metac_parsetree.h"
#include "crc32c.h"


int MetaCDeclaration_Walk_Debug(metac_declaration_t* decl, const char* fn_name, walker_function_t walker_fn, void* ctx)
{
    // make sure the context confusion cookie is set
    assert((*(uint32_t*) ctx) == crc32c_nozero(~0, fn_name, strlen(fn_name)));

    return MetaCDeclaration_Walk_Real(decl, walker_fn, ctx);
}

int MetaCDeclaration_Walk_Real(metac_declaration_t* decl, walker_function_t walker_fn, void* ctx)
{
#define walker_fn(DECL, CTX) \
    walker_fn((metac_node_t) DECL, CTX)

#define MetaCDeclaration_Walk_Real(DECL, FN, CTX) \
    MetaCDeclaration_Walk_Real((metac_declaration_t*) DECL, FN, CTX)

#define emptyNode \
    ((metac_node_t) 0x1)

    if(((metac_node_t)decl) == emptyNode)
        return 0;

    int result = walker_fn(decl, ctx);

    if (result)
        return result;

    switch(decl->DeclKind)
    {
        //default 0:
        case decl_type_tuple:
        // decl_type_tuple doesn't really exist
        case decl_min:
        case decl_max:
            assert(0);

        case decl_variable:
        {
            decl_variable_t* variable = (decl_variable_t*) decl;
            result = MetaCDeclaration_Walk_Real(variable->VarType, walker_fn, ctx);
            if (result)
                return result;
        } break;
        case decl_field:
        {
            decl_field_t* field = (decl_field_t*) decl;
            while(((metac_node_t)field) != emptyNode)
            {
                result = MetaCDeclaration_Walk_Real(field->Field, walker_fn, ctx);
                if (result)
                    return result;
                field = field->Next;
            }
        } break;
        case decl_parameter:
        {
            decl_parameter_t* parameter = (decl_parameter_t*) decl;
            while(((metac_node_t)parameter) != emptyNode)
            {
                result = MetaCDeclaration_Walk_Real(parameter->Parameter, walker_fn, ctx);
                if (result)
                    return result;
                parameter = parameter->Next;
            }
        } break;
        case decl_enum_member:
        {
            decl_enum_member_t* enum_member = (decl_enum_member_t*) decl;
            while(((metac_node_t)enum_member) != emptyNode)
            {
                result = walker_fn(enum_member, ctx);
                if (result)
                    return result;
                enum_member = enum_member->Next;
            }
        } break;
        case decl_type:
        {
            decl_type_t* type = (decl_type_t*) decl;
            if (result)
                return result;
        } break;
        case decl_type_struct:
        {
            decl_type_struct_t* type_struct = (decl_type_struct_t*) decl;
            result = MetaCDeclaration_Walk_Real(type_struct->Fields, walker_fn, ctx);
            if (result)
                return result;
        } break;
        case decl_type_union:
        {
            decl_type_union_t* type_union = (decl_type_union_t*) decl;
            result = MetaCDeclaration_Walk_Real(type_union->Fields, walker_fn, ctx);
            if (result)
                return result;
        } break;
        case decl_type_enum:
        {
            decl_type_enum_t* type_enum = (decl_type_enum_t*) decl;
            result = MetaCDeclaration_Walk_Real(type_enum->Members, walker_fn, ctx);
            if (result)
                return result;
        } break;
        case decl_type_array:
        {
            decl_type_array_t* type_array = (decl_type_array_t*) decl;
            result = MetaCDeclaration_Walk_Real(type_array->ElementType, walker_fn, ctx);
            if (result)
                return result;
            result = walker_fn(type_array->Dim, ctx);
            if (result)
                return result;
        } break;
        case decl_type_ptr:
        {
            decl_type_ptr_t* type_ptr = (decl_type_ptr_t*) decl;
            result = MetaCDeclaration_Walk_Real(type_ptr->ElementType, walker_fn, ctx);
            if (result)
                return result;
        } break;
        case decl_type_functiontype:
        {
            decl_type_functiontype_t* type_functiontype = (decl_type_functiontype_t*) decl;
            result = MetaCDeclaration_Walk_Real(type_functiontype->ReturnType, walker_fn, ctx);
            if (result)
                return result;
            result = MetaCDeclaration_Walk_Real(type_functiontype->Parameters, walker_fn, ctx);
            if (result)
                return result;
        } break;
        case decl_type_typedef:
        {
            decl_type_typedef_t* typedef_ = (decl_type_typedef_t*) decl;
            result = MetaCDeclaration_Walk_Real(typedef_->Type, walker_fn, ctx);
            if (result)
                return result;
        } break;
        case decl_function:
        {
            decl_function_t* function_ = (decl_function_t*) decl;
            result = MetaCDeclaration_Walk_Real(function_->ReturnType, walker_fn, ctx);
            if (result)
                return result;
            result = MetaCDeclaration_Walk_Real(function_->Parameters, walker_fn, ctx);
            if (result)
                return result;
            result = walker_fn(function_->FunctionBody, ctx);
            if (result)
                return result;
        } break;

        case decl_label:
        {
            assert(0);
            // not implemented at the moment
        } break;
        case decl_comment: {} break; // no children to visit
    }

    return 0;
}
#undef MetaCDeclaration_Walk_Real
#undef walker_fn
#undef emptyNode
