#include "metac_parsetree.h"
#include "crc32c.h"

#include "metac_node.c"
int MetaCDeclaration_Walk_Debug(metac_declaration_t* decl, const char* fn_name, walker_function_t walker_fn, void* ctx)
{
    // make sure the context confusion cookie is set
    assert((*(uint32_t*) ctx) == crc32c_nozero(~0, fn_name, strlen(fn_name)));

    return MetaCDeclaration_TreeWalk_Real(decl, walker_fn, ctx);
}

int MetaCDeclaration_TreeWalk_Real(metac_declaration_t* decl, walker_function_t walker_fn, void* ctx)
{
#define walker_fn(DECL, CTX) \
    walker_fn((metac_node_t) DECL, CTX)

#define MetaCDeclaration_TreeWalk_Real(DECL, FN, CTX) \
    MetaCDeclaration_TreeWalk_Real((metac_declaration_t*) DECL, FN, CTX)

#define emptyNode \
    ((metac_node_t) 0x1)

    if(((metac_node_t)decl) == emptyNode)
        return -1;

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
            result = MetaCDeclaration_TreeWalk_Real(variable->VarType, walker_fn, ctx);
            if (result)
                return result;
        } break;
        case decl_field:
        {
            decl_field_t* field = (decl_field_t*) decl;
            while(((metac_node_t)field) != emptyNode)
            {
                result = MetaCDeclaration_TreeWalk_Real(field->Field, walker_fn, ctx);
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
                result = MetaCDeclaration_TreeWalk_Real(parameter->Parameter, walker_fn, ctx);
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
            if (type_struct->Fields && type_struct->Fields != emptyNode)
                result = MetaCDeclaration_TreeWalk_Real(type_struct->Fields, walker_fn, ctx);
            if (result)
                return result;
        } break;
        case decl_type_union:
        {
            decl_type_union_t* type_union = (decl_type_union_t*) decl;
            result = MetaCDeclaration_TreeWalk_Real(type_union->Fields, walker_fn, ctx);
            if (result)
                return result;
        } break;
        case decl_type_enum:
        {
            decl_type_enum_t* type_enum = (decl_type_enum_t*) decl;
            result = MetaCDeclaration_TreeWalk_Real(type_enum->Members, walker_fn, ctx);
            if (result)
                return result;
        } break;
        case decl_type_array:
        {
            decl_type_array_t* type_array = (decl_type_array_t*) decl;
            result = MetaCDeclaration_TreeWalk_Real(type_array->ElementType, walker_fn, ctx);
            if (result)
                return result;
            result = walker_fn(type_array->Dim, ctx);
            if (result)
                return result;
        } break;
        case decl_type_ptr:
        {
            decl_type_ptr_t* type_ptr = (decl_type_ptr_t*) decl;
            result = MetaCDeclaration_TreeWalk_Real(type_ptr->ElementType, walker_fn, ctx);
            if (result)
                return result;
        } break;
        case decl_type_functiontype:
        {
            decl_type_functiontype_t* type_functiontype = (decl_type_functiontype_t*) decl;
            result = MetaCDeclaration_TreeWalk_Real(type_functiontype->ReturnType, walker_fn, ctx);
            if (result)
                return result;
            result = MetaCDeclaration_TreeWalk_Real(type_functiontype->Parameters, walker_fn, ctx);
            if (result)
                return result;
        } break;
        case decl_type_typedef:
        {
            decl_type_typedef_t* typedef_ = (decl_type_typedef_t*) decl;
            result = MetaCDeclaration_TreeWalk_Real(typedef_->Type, walker_fn, ctx);
            if (result)
                return result;
        } break;
        case decl_function:
        {
            decl_function_t* function_ = (decl_function_t*) decl;
            result = MetaCDeclaration_TreeWalk_Real(function_->ReturnType, walker_fn, ctx);
            if (result)
                return result;
            result = MetaCDeclaration_TreeWalk_Real(function_->Parameters, walker_fn, ctx);
            if (result)
                return result;
            if (function_->FunctionBody != emptyNode)
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
/*
 * stmt_block_t* stmt_block = cast(stmt_block_t*) stmt;
 */

int MetaCStatement_TreeWalk_Real(metac_statement_t* stmt, walker_function_t walker_fn, void* ctx)
{
#define MetaCStatement_TreeWalk_Real(STMT, FN, CTX) \
    MetaCStatement_TreeWalk_Real(((metac_statement_t*) STMT), FN, CTX)

    int result;

    if (stmt == (metac_statement_t*) emptyNode)
    {
        return -1;
    }

    if (result = walker_fn(stmt, ctx))
    {
        return result;
    }

    switch(stmt->StmtKind)
    {
        case stmt_block:
        {
            stmt_block_t* stmt_block = cast(stmt_block_t*) stmt;
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
/*
        case stmt_if:
        {
            stmt_if_t* stmt_if = cast(stmt_if_t*) stmt;
            result = MetaCExpression_Walk_Real(stmt_if->IfCond, walker_fn, ctx);
            if (result)
                return result;
            result = MetaCS
            if (result)
                return result;
        }
        break;

        case stmt_switch:
        {
            stmt_switch_t* stmt_switch = cast(stmt_switch_t*) stmt;
            result = MetaCStatement_Walk_Real(stmt_switch->field, walker_fn, ctx);

            if (result)
                return result;
        }
        break;

        case stmt_while:
        {
            stmt_while_t* stmt_while = cast(stmt_while_t*) stmt;
            result = MetaCStatement_Walk_Real(stmt_while->field, walker_fn, ctx);

            if (result)
                return result;
        }
        break;

        case stmt_for:
        {
            stmt_for_t* stmt_for = cast(stmt_for_t*) stmt;
            result = MetaCStatement_Walk_Real(stmt_for->field, walker_fn, ctx);

            if (result)
                return result;
        }
        break;

        case stmt_do_while:
        {
            stmt_do_while_t* stmt_do_while = cast(stmt_do_while_t*) stmt;
            result = MetaCStatement_Walk_Real(stmt_do_while->field, walker_fn, ctx);

            if (result)
                return result;
        }
        break;

        case stmt_label:
        {
            stmt_label_t* stmt_label = cast(stmt_label_t*) stmt;
            result = MetaCStatement_Walk_Real(stmt_label->field, walker_fn, ctx);

            if (result)
                return result;
        }
        break;

        case stmt_case:
        {
            stmt_case_t* stmt_case = cast(stmt_case_t*) stmt;
            result = MetaCStatement_Walk_Real(stmt_case->field, walker_fn, ctx);

            if (result)
                return result;
        }
        break;

        case stmt_break:
        {
            stmt_break_t* stmt_break = cast(stmt_break_t*) stmt;
            result = MetaCStatement_Walk_Real(stmt_break->field, walker_fn, ctx);

            if (result)
                return result;
        }
        break;

        case stmt_yield:
        {
            stmt_yield_t* stmt_yield = cast(stmt_yield_t*) stmt;
            result = MetaCStatement_Walk_Real(stmt_yield->field, walker_fn, ctx);

            if (result)
                return result;
        }
        break;

        case stmt_scope:
        {
            stmt_scope_t* stmt_scope = cast(stmt_scope_t*) stmt;
            result = MetaCStatement_Walk_Real(stmt_scope->field, walker_fn, ctx);

            if (result)
                return result;
        }
        break;

        case stmt_continue:
        {
            stmt_continue_t* stmt_continue = cast(stmt_continue_t*) stmt;
            result = MetaCStatement_Walk_Real(stmt_continue->field, walker_fn, ctx);

            if (result)
                return result;
        }
        break;

        case stmt_goto:
        {
            stmt_goto_t* stmt_goto = cast(stmt_goto_t*) stmt;
            result = MetaCStatement_Walk_Real(stmt_goto->field, walker_fn, ctx);

            if (result)
                return result;
        }
        break;

        case stmt_return:
        {
            stmt_return_t* stmt_return = cast(stmt_return_t*) stmt;
            result = MetaCStatement_Walk_Real(stmt_return->field, walker_fn, ctx);

            if (result)
                return result;
        }
        break;

        case stmt_exp:
        {
            stmt_exp_t* stmt_exp = cast(stmt_exp_t*) stmt;
            result = MetaCStatement_Walk_Real(stmt_exp->field, walker_fn, ctx);

            if (result)
                return result;
        }
        break;

        case stmt_decl:
        {
            stmt_decl_t* stmt_decl = cast(stmt_decl_t*) stmt;
            result = MetaCStatement_Walk_Real(stmt_decl->field, walker_fn, ctx);

            if (result)
                return result;
        }
        break;

        case stmt_comment:
        {
            stmt_comment_t* stmt_comment = cast(stmt_comment_t*) stmt;
            result = MetaCStatement_Walk_Real(stmt_comment->field, walker_fn, ctx);

            if (result)
                return result;
        }
        break;
*/
    }
}

int MetaCNode_TreeWalk_Real(metac_node_t root, walker_function_t walker_fn, void* ctx)
{
    if (MetaCNode_IsExpression(root))
    {
        MetaCDeclaration_TreeWalk_Real((metac_expression_t*)root, walker_fn, ctx);
    }
    else if (MetaCNode_IsStatement(root))
    {
        MetaCStatement_TreeWalk_Real((metac_statement_t*)root, walker_fn, ctx);
    }
    else if (MetaCNode_IsDeclaration(root))
    {
        MetaCDeclaration_TreeWalk_Real((metac_declaration_t*)root, walker_fn, ctx);
    }
    else
    {
        printf("Node type %s is not classified???\n"
               "- It should be classfied as either statement declaration or expression\n"
            , MetaCNodeKind_toChars(root->Kind));
        assert(0);
    }
}
#undef MetaCDeclaration_TreeWalk_Real
#undef walker_fn
#undef emptyNode
