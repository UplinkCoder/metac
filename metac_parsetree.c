#include "metac_parsetree.h"
#include "crc32c.h"

#include "metac_node.c"

#define emptyNode \
    ((metac_node_t) 0x1)


int MetaCNode_TreeWalk_Real(metac_node_t node, walker_function_t walker_fn, void* ctx)
{
    int result;
#define walker_fn(NODE, CTX) \
	walker_fn((metac_node_t)NODE, CTX)
#define MetaCNode_TreeWalk_Real(NODE, FN, CTX) \
	MetaCNode_TreeWalk_Real((metac_node_t)NODE, FN, CTX)

    if (node == emptyNode)
    {
        return -1;
    }

    if (result = walker_fn(node, ctx))
    {
        return result;
    }

    switch(node->Kind)
    {
        default: assert(0);

        //default 0:
        case node_decl_type_tuple:
        // decl_type_tuple doesn't really exist
            assert(0);

        case node_decl_variable:
        {
            decl_variable_t* decl_variable = cast(decl_variable_t*) node;
            if ((metac_node_t)decl_variable->VarType != emptyNode)
                result = walker_fn(decl_variable->VarType, ctx);
            if(result)
                 return result;
            if ((metac_node_t)decl_variable->VarInitExpression != emptyNode)
                result = walker_fn(decl_variable->VarInitExpression, ctx);
            if(result)
                 return result;
        } break;

        case node_decl_field:
        {
            decl_field_t* field = (decl_field_t*) node;
            while(((metac_node_t)field) != emptyNode)
            {
                result = MetaCNode_TreeWalk_Real(field->Field, walker_fn, ctx);
                if (result)
                    return result;
                field = field->Next;
            }
        } break;

        case node_decl_parameter:
        {
            decl_parameter_t* parameter = (decl_parameter_t*) node;
            result = MetaCNode_TreeWalk_Real(parameter->Parameter, walker_fn, ctx);
            if (result)
                return result;

            while(((metac_node_t)parameter->Next) != emptyNode)
            {
                result = MetaCNode_TreeWalk_Real(parameter->Next->Parameter, walker_fn, ctx);
                if (result)
                    return result;
                parameter = parameter->Next;
            }
        } break;

        case node_decl_enum_member:
        {
            decl_enum_member_t* enum_member = (decl_enum_member_t*) node;
            while((metac_node_t)(enum_member->Next) != emptyNode)
            {
                result = walker_fn(enum_member->Next, ctx);
                if (result)
                    return result;
                enum_member = enum_member->Next;
            }
        } break;
        case node_decl_type:
        {
            decl_type_t* type = (decl_type_t*) node;
            if (result)
                return result;
        } break;
        case node_decl_type_struct:
        {
            decl_type_struct_t* type_struct = (decl_type_struct_t*) node;
            if (type_struct->Fields && (metac_node_t)type_struct->Fields != emptyNode)
                result = MetaCNode_TreeWalk_Real(type_struct->Fields, walker_fn, ctx);
            if (result)
                return result;
        } break;
        case node_decl_type_union:
        {
            decl_type_union_t* type_union = (decl_type_union_t*) node;
            result = MetaCNode_TreeWalk_Real(type_union->Fields, walker_fn, ctx);
            if (result)
                return result;
        } break;
        case node_decl_type_enum:
        {
            decl_type_enum_t* type_enum = (decl_type_enum_t*) node;
            result = MetaCNode_TreeWalk_Real(type_enum->Members, walker_fn, ctx);
            if (result)
                return result;
        } break;
        case node_decl_type_array:
        {
            decl_type_array_t* type_array = (decl_type_array_t*) node;
            result = MetaCNode_TreeWalk_Real(type_array->ElementType, walker_fn, ctx);
            if (result)
                return result;
            result = walker_fn(type_array->Dim, ctx);
            if (result)
                return result;
        } break;
        case node_decl_type_ptr:
        {
            decl_type_ptr_t* type_ptr = (decl_type_ptr_t*) node;
            result = MetaCNode_TreeWalk_Real(type_ptr->ElementType, walker_fn, ctx);
            if (result)
                return result;
        } break;
        case node_decl_type_functiontype:
        {
            decl_type_functiontype_t* type_functiontype = (decl_type_functiontype_t*) node;
            result = MetaCNode_TreeWalk_Real(type_functiontype->ReturnType, walker_fn, ctx);
            if (result)
                return result;
            result = MetaCNode_TreeWalk_Real(type_functiontype->Parameters, walker_fn, ctx);
            if (result)
                return result;
        } break;
        case node_decl_type_typedef:
        {
            decl_type_typedef_t* typedef_ = (decl_type_typedef_t*) node;
            result = MetaCNode_TreeWalk_Real(typedef_->Type, walker_fn, ctx);
            if (result)
                return result;
        } break;
        case node_decl_function:
        {
            decl_function_t* decl_function = cast(decl_function_t*) node;
            if ((metac_node_t)decl_function->ReturnType != emptyNode)
                result = walker_fn(decl_function->ReturnType, ctx);
            if(result)
                 return result;
            if ((metac_node_t)decl_function->Parameters != emptyNode)
                result = walker_fn(decl_function->Parameters, ctx);
            if(result)
                 return result;
            if ((metac_node_t)decl_function->FunctionBody != emptyNode)
                result = walker_fn(decl_function->FunctionBody, ctx);
            if(result)
                 return result;
        } break;

        case node_decl_label:
        case node_decl_comment:
            break; // no children to visit

        case node_stmt_block:
        {
            stmt_block_t* stmt_block = cast(stmt_block_t*) node;
            const uint32_t statementCount = stmt_block->StatementCount;
            metac_statement_t* firstStatement = stmt_block->Body;
            for(uint32_t i = 0; i < statementCount; i++)
            {
                metac_statement_t* stmt = firstStatement + i;
                result = walker_fn(stmt, ctx);
                if (result)
                    break;
            }
            if (result)
                return result;
        }
        break;

        case node_stmt_break:
        case node_stmt_continue:
                break;

        case node_stmt_yield:
        {
            stmt_yield_t* stmt_yield = cast(stmt_yield_t*) node;
            if ((metac_node_t)stmt_yield->YieldExp != emptyNode)
                result = walker_fn(stmt_yield->YieldExp, ctx);
            if(result)
                 return result;
        } break;

        case node_stmt_scope:
            assert(0); // Not implemented currently.

        case node_stmt_for:
        {
            stmt_for_t* stmt_for = cast(stmt_for_t*) node;
            if (stmt_for->ForInit != emptyNode)
                result = walker_fn(stmt_for->ForInit, ctx);
            if (result)
                return result;

            if ((metac_node_t)stmt_for->ForCond != emptyNode)
                result = walker_fn(stmt_for->ForCond, ctx);
            if(result)
                 return result;

            if ((metac_node_t)stmt_for->ForPostLoop != emptyNode)
                result = walker_fn(stmt_for->ForPostLoop, ctx);
            if(result)
                 return result;
        } break;

        case node_stmt_while:
        {
            stmt_while_t* stmt_while = cast(stmt_while_t*) node;
            if ((metac_node_t)stmt_while->WhileExp != emptyNode)
                result = walker_fn(stmt_while->WhileExp, ctx);
            if(result)
                 return result;

            if ((metac_node_t)stmt_while->WhileBody != emptyNode)
                result = MetaCNode_TreeWalk_Real(stmt_while->WhileBody, walker_fn, ctx);
            if (result)
                return result;
        } break;

        case node_stmt_case:
        {
            stmt_case_t* stmt_case = cast(stmt_case_t*) node;
            if ((metac_node_t)stmt_case->CaseExp != emptyNode)
                result = walker_fn(stmt_case->CaseExp, ctx);
            if(result)
                 return result;
            if ((metac_node_t)stmt_case->CaseBody != emptyNode)
                result = MetaCNode_TreeWalk_Real(stmt_case->CaseBody, walker_fn, ctx);
            if (result)
                return result;
        } break;

        case node_stmt_goto:
            break;

        case node_stmt_exp:
        {
            stmt_exp_t* stmt_exp = cast(stmt_exp_t*) node;
            if ((metac_node_t)stmt_exp->Expression != emptyNode)
                result = walker_fn(stmt_exp->Expression, ctx);
            if(result)
                 return result;
        } break;

        case node_stmt_decl:
        {
            stmt_decl_t* stmt_decl = cast(stmt_decl_t*) node;
        } break;

        case node_stmt_if:
        {
            stmt_if_t* stmt_if = cast(stmt_if_t*) node;
        } break;

        case node_stmt_label:
            break;

        case node_stmt_return:
        {
            stmt_return_t* stmt_return = cast(stmt_return_t*) node;
            if ((metac_node_t)stmt_return->ReturnExp != emptyNode)
                result = walker_fn(stmt_return->ReturnExp, ctx);
            if(result)
                 return result;
        } break;

        case node_stmt_switch:
        {
            stmt_switch_t* stmt_switch = cast(stmt_switch_t*) node;
            if ((metac_node_t)stmt_switch->SwitchExp != emptyNode)
                result = walker_fn(stmt_switch->SwitchExp, ctx);
            if(result)
                 return result;
            if ((metac_node_t)stmt_switch->SwitchBody != emptyNode)
                result = MetaCNode_TreeWalk_Real(stmt_switch->SwitchBody, walker_fn, ctx);
            if (result)
                return result;
        } break;

        case node_stmt_do_while:
        {
            stmt_do_while_t* stmt_do_while = cast(stmt_do_while_t*) node;
            if ((metac_node_t)stmt_do_while->DoWhileExp != emptyNode)
                result = walker_fn(stmt_do_while->DoWhileExp, ctx);
            if(result)
                 return result;
        } break;

        case node_stmt_comment:
            break;
    }
}
#undef MetaCNode_TreeWalk_Real
#undef walker_fn
#undef emptyNode

int MetaCTree_Walk_Debug(metac_node_t node, const char* fn_name, walker_function_t walker_fn, void* ctx)
{
    // make sure the context confusion cookie is set
    assert((*(uint32_t*) ctx) == crc32c_nozero(~0, fn_name, strlen(fn_name)));

    return MetaCNode_TreeWalk_Real(node, walker_fn, ctx);
}
