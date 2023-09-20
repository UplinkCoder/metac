// metac-printer.c
#include "metac_printer.h"
#include "../parser/metac_lexer.h"
#include "../parser/metac_parser.h"
#include "../utils/int_to_str.c"
#include "../3rd_party/fpconv/src/fpconv.h"

#ifndef FPCONV_C
#  include "../3rd_party/fpconv/src/fpconv.c"
#endif

#include <assert.h>
#include <string.h>

static inline void PrintExpr(metac_printer_t* self, metac_expr_t* exp);
#ifndef NO_SEMANTIC
static inline void PrintSemaExpr(metac_printer_t* self, metac_sema_state_t* sema, metac_sema_expr_t* exp);
#endif

static inline void PrintSpace(metac_printer_t* self)
{
    self->StringMemorySize++;
    self->CurrentColumn++;
}

static inline void PrintChar(metac_printer_t* self, char c)
{
    assert(c != ' ' && c != '\n');
    self->StringMemory[self->StringMemorySize++] = c;
}

static inline void PrintNewline(metac_printer_t* self)
{
    self->StringMemory[self->StringMemorySize++] = '\n';
    self->CurrentColumn = 0;
}

void static inline PrintIndent(metac_printer_t* self)
{
    const uint32_t indent = self->IndentLevel;

    self->StringMemorySize += 4 * indent;
    self->CurrentColumn += 4 * indent;
    // assert(self->CurrentColumn == 4 * indent);
    if (self->StartColumn > self->CurrentColumn)
    {
        int32_t columnAdvance =
            self->StartColumn - self->CurrentColumn;
        self->StringMemorySize += columnAdvance;
        self->CurrentColumn    += columnAdvance;
    }
}

static inline void CheckAndRellocIfNeeded(metac_printer_t* self,
                                          uint32_t length)
{
    int32_t underflow =
        self->StringMemoryCapacity -
        (self->StringMemorySize + length + 1024);
    if (underflow < 0)
    {
        uint32_t newCapa = cast(uint32_t)(self->StringMemoryCapacity * 1.3);
        newCapa = ((newCapa + 4095) & ~4095);
        self->StringMemory = (char*)realloc(self->StringMemory, newCapa);
        if (self->StringMemory == 0)
        {
            assert(0);
        }
    }
}

static inline void PrintString(metac_printer_t* self,
                 const char* string, uint32_t length)
{
    char c;
    uint32_t pos = 0;

    while((c = *string++) && (pos++ < length))
    {
        assert(c != '\n');
        self->StringMemory[self->StringMemorySize++] = c;
    }
    self->CurrentColumn += length;
}

static inline void PrintStringWithNewline(metac_printer_t* self,
                 const char* string, uint32_t length)
{
    char c;
    uint32_t pos = 0;

    while((c = *string++) && (pos++ < length))
    {
        if (c == '\n' || c == '\r')
            self->CurrentColumn = 0;
        self->CurrentColumn++;
        self->StringMemory[self->StringMemorySize++] = c;
    }
}

static inline void PrintIdentifier(metac_printer_t* self,
                                   metac_identifier_ptr_t idPtr)
{
    if (idPtr.v == empty_identifier.v)
        assert(0); // One is not supposed to print the empty identifier
    const char* ident = IdentifierPtrToCharPtr(self->IdentifierTable, idPtr);

    PrintString(self, ident, (uint32_t)strlen(ident));
}

static void PrintKeyword(metac_printer_t* self,
                         metac_token_enum_t keyword)
{
    const char * str =
        MetaCTokenEnum_toChars(keyword) + sizeof("tok_kw");

    PrintString(self, str, (uint32_t)strlen(str));
}

static inline void PrintToken(metac_printer_t* self,
                              metac_token_enum_t tokenType)
{
#define CASE_(KW) \
    case KW :

    switch(tokenType)
    {
        default:
            assert(0);

        case tok_semicolon:
            PrintChar(self, ';');
        break;
        case tok_lBrace :
            PrintChar(self, '{');
        break;

        case tok_rBrace :
            PrintChar(self, '}');
        break;

        case tok_lParen:
            PrintChar(self, '(');
        break;

        case tok_rParen:
            PrintChar(self, ')');
        break;

        case tok_lBracket:
            PrintChar(self, '[');
        break;
        case tok_rBracket:
            PrintChar(self, ']');
        break;

        case tok_assign:
            PrintChar(self, '=');
        break;
    }
}
#undef CASE_

static inline void PrintU64(metac_printer_t* self, uint64_t value)
{
    char u64Buffer[21];

    const char* result = u64tostr(value, u64Buffer);
    int32_t length = (u64Buffer + 20) - result;
    // assert((length > 0) && (length <= 20));
    PrintString(self, result, length);
}

static inline void PrintI64(metac_printer_t* self, int64_t value)
{
    char i64Buffer[22];

    const char* result = i64tostr(value, i64Buffer);
    int32_t length = (i64Buffer + 20) - result;
    // assert((length > 0) && (length <= 20));
    PrintString(self, result, length);
}

static inline void PrintF23(metac_printer_t* self, float value)
{
    char f23Buffer[25];

    int len = fpconv_dtoa(cast(double) value, f23Buffer);
    f23Buffer[len] = 'f';
    PrintString(self, f23Buffer, len + 1);
}


static inline void PrintType(metac_printer_t* self, decl_type_t* type);
static inline void PrintParameterList(metac_printer_t* self,
                                     decl_parameter_t* Parameters);

static inline void PrintFunctionTypeWithIdentifier(metac_printer_t* self,
                                                   decl_type_t* type,
                                                   metac_identifier_ptr_t identifier)
{
    decl_type_functiontype_t *funcType =
        (decl_type_functiontype_t*) type;

    assert(type->Kind == decl_type_functiontype);

    PrintType(self, funcType->ReturnType);
    PrintSpace(self);
    PrintChar(self, '(');
    PrintChar(self, '*');
    if (identifier.v != empty_identifier.v)
    {
        PrintIdentifier(self, identifier);
    }
    PrintChar(self, ')');
    PrintSpace(self);
    PrintParameterList(self, funcType->Parameters);
}


static inline void PrintVariable(metac_printer_t* self,
                                 decl_variable_t* variable)
{
    if (variable->VarType->Kind == decl_type_functiontype)
    {
        PrintFunctionTypeWithIdentifier(self,
                                        variable->VarType,
                                        variable->VarIdentifier);
    }
    else
    {
        PrintType(self, variable->VarType);
        PrintSpace(self);
        PrintIdentifier(self, variable->VarIdentifier);
    }

    if (variable->VarInitExpr != emptyPointer)
    {
        PrintSpace(self);
        PrintToken(self, tok_assign);
        PrintSpace(self);
        PrintExpr(self, variable->VarInitExpr);
    }
}

static inline void PrintDecl(metac_printer_t* self,
                             metac_decl_t* decl,
                             uint32_t level);

static inline void PrintField(metac_printer_t* self,
                              decl_variable_t* field)
{
    if (field->VarIdentifier.v != empty_identifier.v)
    {
        PrintVariable(self, field);
    }
    else
    {
        self->ForAnonymousField++;
        PrintDecl(self, cast(metac_decl_t*)field->VarType, 0);
        self->ForAnonymousField--;
    }
}

static inline void PrintParameterList(metac_printer_t* self,
                                      decl_parameter_t* Parameters)
{
    PrintToken(self, tok_lParen);

    for(decl_parameter_t* param = Parameters;
        param != emptyPointer;
        param = param->Next)
    {
        if (param->Parameter->VarIdentifier.v != empty_identifier.v)
        {
            PrintVariable(self, param->Parameter);
        }
        else
        {
            PrintType(self, param->Parameter->VarType);
        }
        if (param->Next != emptyPointer)
                PrintString(self, ", ", 2);
    }

    PrintToken(self, tok_rParen);
}


static inline void PrintType(metac_printer_t* self, decl_type_t* type)
{
    switch(type->Kind)
    {
        case decl_type_typeof:
        {
            decl_type_typeof_t *typeofType = (decl_type_typeof_t*) type;
            PrintString(self, "typeof (", sizeof("typeof (") - 1);
            PrintExpr(self, typeofType->Expr);
            PrintChar(self, ')');
        } break;
        case decl_type_array:
        {
            decl_type_array_t *arrayType = (decl_type_array_t*) type;
            // PrintTypeName(self, )
            PrintType(self, arrayType->ElementType);
            PrintChar(self, '[');
            if (METAC_NODE(arrayType->Dim) != emptyNode)
                PrintExpr(self, arrayType->Dim);
            PrintChar(self, ']');
        } break;

        case decl_type_ptr:
        {
            decl_type_ptr_t *ptrType = (decl_type_ptr_t*) type;
            // PrintTypeName(self, )
            PrintType(self, ptrType->ElementType);
            PrintChar(self, '*');
        } break;

        case decl_type:
        {
            // printf("TypeKind: %d\n", (int) type->Kind);
            if (type->TypeModifiers)
            {
                uint32_t modifiers = type->TypeModifiers;
                if (modifiers & typemod_const)
                {
                    PrintString(self, "const ", sizeof("const"));
                }
                if (modifiers & typemod_unsigned)
                {
                    PrintString(self, "unsigned ", sizeof("unsigned"));
                }
                if (modifiers & typemod_signed)
                {
                    PrintString(self, "signed ", sizeof("signed"));
                }
            }
            if (type->TypeKind >= type_auto && type->TypeKind <= type_double)
            {
                metac_token_enum_t tok = (metac_token_enum_t)
                    ((type->TypeKind - type_auto) + tok_kw_auto);
                PrintKeyword(self, tok);
            }
            else if (type->TypeKind == type_unsigned_char)
            {
                PrintString(self, "unsigned char", sizeof("unsigned char") - 1);
            }
            else if (type->TypeKind == type_unsigned_short)
            {
                PrintString(self, "unsigned short", sizeof("unsigned short") - 1);
            }
            else if (type->TypeKind == type_unsigned_int)
            {
                PrintString(self, "unsigned int", sizeof("unsigned int") - 1);
            }
            else if (type->TypeKind == type_unsigned_long)
            {
                PrintString(self, "unsigned long", sizeof("unsigned long") - 1);
            }
            else if (type->TypeKind == type_unsigned_long_long)
            {
                PrintString(self, "unsigned long long", sizeof("unsigned long long") - 1);
            }
            else if (type->TypeKind == type_long_long)
            {
                PrintString(self, "long long", sizeof("long long") - 1);
            }
            else if (type->TypeKind == type_long_double)
            {
                PrintString(self, "long double", sizeof("long double") - 1);
            }
            else if (type->TypeKind == type_type)
            {
                PrintString(self, "type", sizeof("type") - 1);
            }
            else if (type->TypeKind == type_identifier)
            {
                PrintIdentifier(self, type->TypeIdentifier);

                //printf("type_identifier: %d\n", type->Identifier.v);
            }
            else if (type->TypeKind == type_modifiers)
            {
                // we already printed the modifiers
            } else if (type->TypeKind == type_tuple)
            {
                goto LprintTuple;
            } else
                assert(0);
        } break;
        case decl_type_tuple :
    LprintTuple:
        {
            decl_type_tuple_t* typeTuple = cast(decl_type_tuple_t*) type;
            const int32_t typeCount = cast(int32_t)typeTuple->TypeCount;
            PrintString(self, "{", 1);
            for(int32_t i = 0; i < typeCount - 1; i++)
            {
                PrintType(self, typeTuple->Types[i]);
                PrintString(self, ", ", sizeof(", ") - 1);
            }
            if (typeCount)
            {
                PrintType(self, typeTuple->Types[typeCount - 1]);
            }
            PrintString(self, "}", 1);
        } break;

        case decl_type_struct :
        {
            decl_type_struct_t* structType = (decl_type_struct_t*) type;
            if (structType->Identifier.v != empty_identifier.v)
            {
                PrintIdentifier(self, structType->Identifier);
            }
            else
            {
                PrintKeyword(self, tok_kw_struct);
            }
        }
        break;
        case decl_type_union :
        {
            decl_type_union_t* unionType = (decl_type_union_t*) type;
            if (unionType->Identifier.v != empty_identifier.v)
            {
                PrintIdentifier(self, unionType->Identifier);
            }
            else
            {
                PrintKeyword(self, tok_kw_union);
            }
        }
        break;
        case decl_type_functiontype:
        {
            PrintFunctionTypeWithIdentifier(self, type, empty_identifier);
        } break;
        case decl_type_enum :
        {
            decl_type_enum_t* enumType = (decl_type_enum_t*) type;
            if (enumType->Identifier.v != empty_identifier.v)
            {
                PrintIdentifier(self, enumType->Identifier);
            }
            else
            {
                PrintKeyword(self, tok_kw_enum);
            }
        }
        break;
        default : assert(0);
    }
}

#define CASE_MACRO(EXPR_TYPE) \
    case EXPR_TYPE : {result = #EXPR_TYPE;} break;

const char* StmtKind_toChars(metac_stmt_kind_t kind)
{
    const char* result = 0;

    switch(kind)
    {
        FOREACH_STMT_KIND(CASE_MACRO)
    }

    return result;
}

#undef CASE_MACRO

static inline void PrintComment(metac_printer_t* self,
                                const char* Text, uint32_t Length)
{
    // bool isMultiline = (memchr(Text, '\n', Length) != 0);
    PrintString(self, "/*", 2);
    PrintStringWithNewline(self, Text, Length);
    PrintString(self, "*/", 2);

}
static inline void PrintStmt(metac_printer_t* self, metac_stmt_t* stmt)
{
    // printf("Kind: %s\n", StmtKind_toChars(stmt->Kind));
    switch(stmt->Kind)
    {
        case stmt_return :
        {
            stmt_return_t* stmt_return = cast(stmt_return_t*) stmt;

            PrintKeyword(self, tok_kw_return);
            PrintSpace(self);
            if (stmt_return->ReturnExp != emptyPointer)
                PrintExpr(self, stmt_return->ReturnExp);
            PrintToken(self, tok_semicolon);
        } break;
        case stmt_yield :
        {
            stmt_yield_t* stmt_yield = cast(stmt_yield_t*) stmt;

            PrintKeyword(self, tok_kw__yield);
            PrintSpace(self);
            if (stmt_yield->YieldExp != emptyPointer)
                PrintExpr(self, stmt_yield->YieldExp);
            PrintToken(self, tok_semicolon);
        } break;
        case stmt_block :
        {
            stmt_block_t* stmt_block = cast(stmt_block_t*) stmt;
            metac_stmt_t* nextStmt = stmt_block->Body;

            PrintToken(self, tok_lBrace);
            ++self->IndentLevel;

            if (nextStmt != emptyPointer)
            {
                PrintNewline(self);
                PrintIndent(self);
            }

            for(;
                nextStmt != emptyPointer;
                nextStmt = nextStmt->Next)
            {
                PrintStmt(self, nextStmt);
                if(nextStmt->Next != emptyPointer)
                {
                    PrintNewline(self);
                    PrintIndent(self);
                }
            }

            --self->IndentLevel;
            PrintNewline(self);
            PrintIndent(self);
            PrintToken(self, tok_rBrace);
        } break;
        case stmt_if :
        {
            stmt_if_t* stmt_if_ = cast(stmt_if_t*) stmt;

            PrintKeyword(self, tok_kw_if);
            PrintSpace(self);
            PrintChar(self, '(');
            PrintExpr(self, stmt_if_->IfCond);
            PrintChar(self, ')');

            if (stmt_if_->IfBody->Kind != stmt_block)
            {
                ++self->IndentLevel;
            }

            PrintNewline(self);
            PrintIndent(self);
            PrintStmt(self, stmt_if_->IfBody);
            if (stmt_if_->IfBody->Kind != stmt_block)
            {
                --self->IndentLevel;
            }

            PrintNewline(self);
            PrintIndent(self);

            if (stmt_if_->ElseBody != emptyPointer)
            {
                PrintKeyword(self, tok_kw_else);
                if (stmt_if_->ElseBody->Kind == stmt_if)
                {
                    PrintSpace(self);
                }
                else
                {
                    if (stmt_if_->ElseBody->Kind != stmt_block)
                    {
                        ++self->IndentLevel;
                    }

                    PrintNewline(self);
                    PrintIndent(self);
                }
                PrintStmt(self, stmt_if_->ElseBody);

                if (stmt_if_->ElseBody->Kind != stmt_if)
                {
                    if (stmt_if_->ElseBody->Kind != stmt_block)
                    {
                        --self->IndentLevel;
                    }
                }

                PrintNewline(self);
                PrintIndent(self);
            }
        } break;
        case stmt_expr :
        {
            stmt_expr_t* expr_stmt = cast(stmt_expr_t*) stmt;
            PrintExpr(self, expr_stmt->Expr);
            PrintToken(self, tok_semicolon);
        } break;
        case stmt_decl:
        {
            stmt_decl_t* decl_stmt = cast(stmt_decl_t*) stmt;
            PrintDecl(self, decl_stmt->Decl, 0);
        } break;
        case stmt_for:
        {
            stmt_for_t* stmt_for = cast(stmt_for_t*) stmt;
            PrintKeyword(self, tok_kw_for);
            PrintChar(self, '(');

            if (stmt_for->ForInit != cast(metac_node_t) emptyPointer)
            {
                if (IsExprNode(stmt_for->ForInit->Kind))
                {
                    PrintExpr(self, (metac_expr_t*)stmt_for->ForInit);
                    PrintToken(self, tok_semicolon);
                    PrintSpace(self);
                }
                else
                {
                    self->SuppressNewlineAfterDecl = true;
                    PrintDecl(self, (metac_decl_t*)stmt_for->ForInit, 0);
                    self->SuppressNewlineAfterDecl = false;
                    PrintSpace(self);
                }
            }
            else
            {
                PrintToken(self, tok_semicolon);
            }

            if (stmt_for->ForCond != cast(metac_expr_t*) emptyPointer)
            {
                PrintExpr(self, stmt_for->ForCond);
            }

            PrintToken(self, tok_semicolon);
            if (stmt_for->ForCond != cast(metac_expr_t*) emptyPointer)
            {
                PrintSpace(self);
            }

            if (stmt_for->ForPostLoop != cast(metac_expr_t*) emptyPointer)
            {
                PrintExpr(self, stmt_for->ForPostLoop);
            }
            PrintChar(self, ')');
            PrintNewline(self);
            PrintIndent(self);
            if (stmt_for->ForBody != (metac_stmt_t*) emptyPointer)
            {
                PrintStmt(self, stmt_for->ForBody);
            }
        } break;
        case stmt_break:
        {
            stmt_break_t* stmt_break = cast(stmt_break_t*) stmt;
            PrintKeyword(self, tok_kw_break);
            PrintToken(self, tok_semicolon);
        } break;
        case stmt_continue:
        {
            stmt_continue_t* stmt_continue = cast(stmt_continue_t*) stmt;
            PrintKeyword(self, tok_kw_continue);
            PrintToken(self, tok_semicolon);
        } break;
        case stmt_case:
        {
            stmt_case_t* caseStmt = cast(stmt_case_t*) stmt;
            if (caseStmt->CaseExp == (metac_expr_t*) emptyPointer)
            {
                PrintKeyword(self, tok_kw_default);
            }
            else
            {
                PrintKeyword(self, tok_kw_case);
                PrintSpace(self);
                PrintExpr(self, caseStmt->CaseExp);
            }
            PrintChar(self, ':');
            if (caseStmt->CaseBody != cast(metac_stmt_t*) emptyPointer)
            {
                if (caseStmt->CaseBody->Kind == stmt_block)
                {
                    PrintStmt(self, caseStmt->CaseBody);
                }
                else if (caseStmt->CaseBody->Kind == stmt_case)
                {
                    PrintNewline(self);
                    PrintIndent(self);
                    PrintStmt(self, caseStmt->CaseBody);
                }
                else
                {
                    ++self->IndentLevel;
                    PrintNewline(self);
                    PrintIndent(self);
                    metac_stmt_t* stmt = caseStmt->CaseBody;
                    while(stmt && stmt != emptyPointer)
                    {
                        PrintStmt(self, stmt);
                        if (stmt->Next)
                        {
                            if (stmt->Kind != stmt_decl)
                            {
                                PrintNewline(self);
                            }
                            PrintIndent(self);
                            stmt = stmt->Next;
                        }
                    }
                    --self->IndentLevel;
                }
            }
        } break;
        case stmt_label:
        {
            stmt_label_t* stmt_label = cast(stmt_label_t*) stmt;
            PrintIdentifier(self, stmt_label->Label);
            PrintChar(self, ':');
        } break;
        case stmt_goto:
        {
            stmt_goto_t* stmt_goto = cast(stmt_goto_t*) stmt;
            PrintKeyword(self, tok_kw_goto);
            PrintSpace(self);
            PrintIdentifier(self, stmt_goto->GotoLabel);
            PrintToken(self, tok_semicolon);
        } break;
        case stmt_switch:
        {
            stmt_switch_t* stmt_switch = cast(stmt_switch_t*) stmt;
            PrintKeyword(self, tok_kw_switch);
            PrintSpace(self);
            PrintChar(self, '(');
            PrintExpr(self, stmt_switch->SwitchExp);
            PrintChar(self, ')');
            PrintNewline(self);
            PrintIndent(self);
            PrintStmt(self, cast(metac_stmt_t*)stmt_switch->SwitchBody);
        } break;
        case stmt_run:
        {
            stmt_run_t* stmt_run = (stmt_run_t*)stmt;
            PrintString(self, "@run", sizeof("@run")-1);
            PrintSpace(self);
            PrintStmt(self, stmt_run->RunBody);
        } break;
        case stmt_while:
        {
            stmt_while_t* stmt_while = (stmt_while_t*)stmt;
            PrintKeyword(self, tok_kw_while);
            PrintSpace(self);
            PrintChar(self, '(');
            PrintExpr(self, stmt_while->WhileExp);
            PrintChar(self, ')');
            PrintNewline(self);
            PrintIndent(self);
            PrintStmt(self, stmt_while->WhileBody);
        } break;
        case stmt_do_while:
        {
            stmt_do_while_t* stmt_while = (stmt_do_while_t*)stmt;
            PrintKeyword(self, tok_kw_do);
            PrintNewline(self);
            PrintIndent(self);
            PrintStmt(self, stmt_while->DoWhileBody);
            PrintKeyword(self, tok_kw_while);
            PrintSpace(self);
            PrintChar(self, '(');
            PrintExpr(self, stmt_while->DoWhileExp);
            PrintChar(self, ')');
        } break;
        case stmt_comment:
        {
            stmt_comment_t* comment = (stmt_comment_t*)stmt;

            PrintString(self, "/*", 2);
            PrintStringWithNewline(self, comment->Text, comment->Length);
            PrintString(self, "*/", 2);
        } break;
        case stmt_empty:
        {
        } break;

        default : {
            fprintf(stderr,
                "Statement Kind: not handled by printer %s\n",
                    StmtKind_toChars(stmt->Kind));
            assert(0);
        }
    }
}

static inline metac_token_enum_t AggToken(metac_decl_kind_t declKind)
{
    metac_token_enum_t result;

    if (declKind == decl_type_struct)
    {
        result = tok_kw_struct;
    } else if (declKind == decl_type_union)
    {
        result = tok_kw_union;
    }/* else if (declKind == decl_class)
    //{

    }*/
    else
    {
        assert(0);
    }

    return result;
}

static inline void PrintDecl(metac_printer_t* self,
                                    metac_decl_t* decl,
                                    uint32_t level)
{
    bool printSemicolon = true;
    bool printingTypedef = false;
    // remove the for-typedef flag at the top
    // since we don't want it to propagate to members
    if (self->ForTypedef)
    {
        self->ForTypedef = false;
        printingTypedef = true;
    }

    switch (decl->Kind)
    {
        case decl_type_enum:
        {
            decl_type_enum_t* enum_ = (decl_type_enum_t*) decl;
            PrintString(self, "enum", sizeof("enum") - 1);
            PrintSpace(self);
            if (enum_->Identifier.v != empty_identifier.v)
            {
                PrintIdentifier(self, enum_->Identifier);
                PrintSpace(self);
            }
            PrintSpace(self);
            PrintToken(self, tok_lBrace);
            ++level;
            ++self->IndentLevel;
            decl_enum_member_t* member = enum_->Members;
            if (enum_->MemberCount)
            {
                PrintNewline(self);
                PrintIndent(self);
            }
            else
            {
                PrintSpace(self);
            }
            for(uint32_t memberIndex = 0;
                memberIndex < enum_->MemberCount;
                memberIndex++)
            {
                PrintIdentifier(self, member->Name);
                if (member->Value != emptyPointer)
                {
                    PrintSpace(self);
                    PrintChar(self, '=');
                    PrintSpace(self);
                    PrintExpr(self, member->Value);
                }
                if (member->Next != emptyPointer)
                {
                    PrintChar(self, ',');
                    PrintNewline(self);
                    PrintIndent(self);
                }
                member = member->Next;
            }
            --self->IndentLevel;
            --level;
            PrintIndent(self);
            PrintNewline(self);
            PrintToken(self, tok_rBrace);
        } break;

        case decl_type_typedef:
        {
            decl_type_typedef_t* typdef = (decl_type_typedef_t*) decl;
            PrintString(self, "typedef ", sizeof("typedef ") - 1);
            level++;
            self->ForTypedef = true;
            if (typdef->Type->Kind == decl_type_functiontype)
            {
                PrintFunctionTypeWithIdentifier(self, typdef->Type, typdef->Identifier);
            }
            else
            {
                PrintDecl(self, (metac_decl_t*)typdef->Type, level);
                if (typdef->Identifier.v != empty_identifier.v)
                {
                    PrintSpace(self);
                    PrintIdentifier(self, typdef->Identifier);
                }
            }
            level--;
        } break;

        case decl_type:
        case decl_type_array:
        {
            PrintType(self, (decl_type_t*) decl);
        }  break;

        case decl_type_union :
        case decl_type_struct :
        {
            decl_type_struct_t* struct_ = (decl_type_struct_t*) decl;
            decl_field_t* f = struct_->Fields;
            PrintKeyword(self, AggToken(decl->Kind));
            if (struct_->Identifier.v != empty_identifier.v)
            {
                PrintSpace(self);
                PrintIdentifier(self, struct_->Identifier);
            }

            if (METAC_NODE(f) == emptyNode)
            {
                break;
            }

            PrintSpace(self);
            PrintToken(self, tok_lBrace);
            ++level;
            ++self->IndentLevel;
            PrintNewline(self);
            PrintIndent(self);
            for(uint32_t memberIndex = 0;
                memberIndex < struct_->FieldCount;
                memberIndex++)
            {
                PrintDecl(self, (metac_decl_t*)f, level);
                //PrintChar(self, ';');
                if (f->Next && f->Next != emptyPointer)
                    PrintIndent(self);
                f = f->Next;
            }
            --self->IndentLevel;
            --level;
            //PrintNewline(self);
            PrintIndent(self);
            PrintToken(self, tok_rBrace);
            if (self->IndentLevel && (self->ForAnonymousField == 0))
                PrintNewline(self);
            else
                PrintSpace(self);

            if (self->ForAnonymousField > 0)
            {
                self->SuppressNewlineAfterDecl = true;
                printSemicolon = false;
            }

        } break;

        case decl_type_functiontype:
        {
            // printing function type for a typedef should have been done already.
            assert(!printingTypedef);
            PrintFunctionTypeWithIdentifier(self, (decl_type_t*)decl, empty_identifier);
        } break;

        case decl_field :
        {
            decl_field_t* field = (decl_field_t*) decl;
            PrintField(self, field->Field);
        } break;
        case decl_variable:
        {
            decl_variable_t* variable = (decl_variable_t*) decl;
            PrintVariable(self, variable);
        } break;
        case decl_function:
        {
            decl_function_t* function_ = (decl_function_t*) decl;
            PrintType(self, function_->ReturnType);
            PrintSpace(self);
            PrintIdentifier(self, function_->Identifier);
            PrintSpace(self);
            PrintParameterList(self, function_->Parameters);

            if (function_->FunctionBody != emptyPointer)
            {
                PrintNewline(self);
                PrintIndent(self);
                PrintStmt(self, (metac_stmt_t*)function_->FunctionBody);
                printSemicolon = false;
            }
        } break;

        case decl_comment:
        {
            decl_comment_t* comment = (decl_comment_t*) decl;
            PrintComment(self, comment->Text, comment->Length);
            printSemicolon = false;
        } break;
        case decl_label:
        {
            decl_label_t* label = (decl_label_t*) decl;
            PrintIdentifier(self, label->Identifier);
            PrintChar(self, ':');
        } break;
    }

    if ((!!printSemicolon) & (!printingTypedef))
    {
        PrintToken(self, tok_semicolon);
    }
    if (self->SuppressNewlineAfterDecl || printingTypedef)
    {
        self->SuppressNewlineAfterDecl = false;
    }
    else
    {
        PrintNewline(self);
    }

    self->ForTypedef = false;
}

void MetaCPrinter_PrintForHeader(metac_printer_t* self, metac_decl_t* decl)
{
    if (!decl)
        return;

    switch (decl->Kind)
    {
        case decl_function:
        {
            decl_function_t func =
                *cast(decl_function_t*)decl;

            METAC_NODE(func.FunctionBody) = emptyNode;
            PrintDecl(self, (metac_decl_t*)&func, 0);
        } break;
        default:
            PrintDecl(self, decl, 0);

    }
}

static inline void PrintExpr(metac_printer_t* self, metac_expr_t* expr)
{
    if (expr->Kind == expr_paren)
    {
        if (!IsBinaryExp(expr->E1->Kind))
            PrintChar(self, '(');

        PrintExpr(self, expr->E1);

        if (!IsBinaryExp(expr->E1->Kind))
            PrintChar(self, ')');
    }
    else if (expr->Kind == expr_ternary)
    {
        PrintChar(self, '(');
        PrintExpr(self, expr->Econd);
        PrintSpace(self);
        PrintChar(self, '?');
        PrintSpace(self);
        PrintExpr(self, expr->E1);
        PrintSpace(self);
        PrintChar(self, ':');
        PrintSpace(self);
        PrintExpr(self, expr->E2);
        PrintChar(self, ')');
    }
    else if (expr->Kind == expr_tuple)
    {
        PrintChar(self, '{');
        expr_tuple_t* tupleElement =
            expr->TupleExprList;
        for(uint32_t i = 0;
            i < expr->TupleExprCount;
            i++)
        {
            PrintExpr(self, tupleElement->Expr);
            if (i != (expr->TupleExprCount - 1))
            {
                PrintChar(self, ',');
                PrintSpace(self);
            }
            tupleElement = tupleElement->Next;
        }
        PrintChar(self, '}');
    }
    else if (expr->Kind == expr_type)
    {
        // PrintChar(self, '(');
        PrintType(self, expr->TypeExp);
        // PrintChar(self, ')');
    }
    else if (expr->Kind == expr_identifier)
    {
        PrintIdentifier(self, expr->IdentifierPtr);
    }
/*
    else if (expr->Kind == expr_run)
    {
        PrintString(self, "@run", 4);
        PrintSpace(self);
        if (expr->RunStmt != emptyPointer)
            PrintStmt(self, expr->RunStmt);
    }
*/
    else if (expr->Kind == expr_string)
    {
        uint32_t stringLength = LENGTH_FROM_STRING_KEY(expr->StringKey);
        PrintChar(self, '"');
        PrintString(self,
            IdentifierPtrToCharPtr(self->StringTable, expr->StringPtr),
            stringLength);
        PrintChar(self, '"');
    }
    else if (expr->Kind == expr_signed_integer)
    {
        PrintI64(self, expr->ValueI64);
    }
    else if (expr->Kind == expr_float)
    {
        PrintF23(self, expr->ValueF23);
    }
    else if (expr->Kind == expr_char)
    {
        PrintChar(self, '\'');
        PrintString(self, expr->Chars, LENGTH_FROM_CHAR_KEY(expr->CharKey));
        PrintChar(self, '\'');
    }
    else if (expr->Kind == expr_index)
    {
        PrintExpr(self, expr->E1);
        PrintToken(self, tok_lBracket);
        PrintExpr(self, expr->E2);
        PrintToken(self, tok_rBracket);
    }
    else if (expr->Kind == expr_call)
    {
        PrintExpr(self, expr->E1);
        if (METAC_NODE(expr->E2) != emptyPointer)
        {
            PrintExpr(self, expr->E2);
        }
        else
        {
            PrintString(self, "()", 2);
        }
    }
    else if (IsBinaryExp(expr->Kind))
    {
        const char* op = BinExpTypeToChars((metac_binary_expr_kind_t)expr->Kind);

        //PrintChar(self, '(');
        PrintExpr(self, expr->E1);

        if (expr->Kind != expr_dot)
            PrintSpace(self);

        PrintString(self, op, (uint32_t)strlen(op));

        if (expr->Kind != expr_dot)
            PrintSpace(self);

        PrintExpr(self, expr->E2);
        //PrintChar(self, ')');
    }
    else if (expr->Kind == expr_cast)
    {
        PrintString(self, "cast", 4);
        PrintChar(self, '(');
        PrintType(self, expr->CastType);
        PrintChar(self, ')');

        PrintExpr(self, expr->CastExp);
    }
    else if (expr->Kind == expr_template_instance)
    {
        PrintExpr(self, expr->E1);
        PrintChar(self, '!');
        PrintExpr(self, expr->E2);
    }
    else if (expr->Kind == expr_argument)
    {
        PrintChar(self, '(');
        for(expr_argument_t* arg = (expr_argument_t*)expr;
            arg != emptyPointer;
            arg = arg->Next)
        {
            PrintExpr(self, arg->Expr);
            if (arg->Next != emptyPointer)
                PrintString(self, ", ", 2);
        }
        PrintChar(self, ')');
    }
    else if (expr->Kind == expr_sizeof)
    {
        PrintKeyword(self, tok_kw_sizeof);
        PrintToken(self, tok_lParen);
        PrintExpr(self, expr->E1);
        PrintToken(self, tok_rParen);
    }
    else if (expr->Kind == expr_addr || expr->Kind == expr_deref
          || expr->Kind == expr_not  || expr->Kind == expr_compl
          || expr->Kind == expr_umin)
    {
        {
            const char* op = 0;
            if (expr->Kind == expr_addr)
                op = "&";
            else if (expr->Kind == expr_deref)
                op = "*";
            else if (expr->Kind == expr_not)
                op = "!";
            else if (expr->Kind == expr_compl)
                op = "~";
            else if (expr->Kind == expr_umin)
                op = "-";

            PrintString(self, op, (uint32_t)strlen(op));
        }
/*
        if (!IsBinaryExp(expr->E1->Kind))
            PrintChar(self, '(');
*/
        PrintExpr(self, expr->E1);
/*
        if (!IsBinaryExp(expr->E1->Kind))
            PrintChar(self, ')');
*/
    }
    else if (expr->Kind == expr_outer)
    {
        PrintChar(self, '$');
        PrintChar(self, '(');
        PrintExpr(self, expr->E1);
        PrintChar(self, ')');
    }
    else if (expr->Kind == expr_stringize)
    {
        PrintChar(self, '#');
        PrintExpr(self, expr->E1);
    }
    else if (expr->Kind == expr_increment || expr->Kind == expr_decrement)
    {
        const char* op = 0;
        if (expr->Kind == expr_increment)
            op = "++";
        else if (expr->Kind == expr_decrement)
            op = "--";

        assert(op);

        PrintString(self, op, (uint32_t)strlen(op));
/*
        if (!IsBinaryExp(expr->E1->Kind))
            PrintChar(self, '(');
*/
        PrintExpr(self, expr->E1);
/*
        if (!IsBinaryExp(expr->E1->Kind))
            PrintChar(self, ')');
*/
    }
    else if (expr->Kind == expr_post_increment || expr->Kind == expr_post_decrement)
    {
        const char* op = 0;
        if (expr->Kind == expr_post_increment)
            op = "++";
        else if (expr->Kind == expr_post_decrement)
            op = "--";

        assert(op);

        if (!IsBinaryExp(expr->E1->Kind))
            PrintChar(self, '(');

        PrintExpr(self, expr->E1);

        if (!IsBinaryExp(expr->E1->Kind))
            PrintChar(self, ')');

        PrintString(self, op, (uint32_t)strlen(op));
    }
    else if (expr->Kind == expr_inject || expr->Kind == expr_eject
          || expr->Kind == expr_typeof || expr->Kind == expr_assert
          || expr->Kind == expr_unary_dot)
    {
        {
            const char* op = 0;
            if (expr->Kind == expr_inject)
                op = "inject";
            else if (expr->Kind == expr_eject)
                op = "eject";
            else if (expr->Kind == expr_typeof)
                op = "typeof";
            else if (expr->Kind == expr_assert)
                op = "assert";
            else if (expr->Kind == expr_unary_dot)
                op = ".";

            assert(op);

            PrintString(self, op, (uint32_t)strlen(op));
        }

        if (!IsBinaryExp(expr->E1->Kind))
        {
           PrintChar(self, '(');
        }

        PrintExpr(self, expr->E1);

        if (!IsBinaryExp(expr->E1->Kind))
        {
            PrintChar(self, ')');
        }
    }
    else
    {
        printf("don't know how to print %s\n", (MetaCExprKind_toChars(expr->Kind)));
    }
}

#ifndef NO_SEMANTIC

#include "../semantic/metac_semantic.h"

static inline void PrintSemaStmt(metac_printer_t* self,
                                      metac_sema_state_t* sema,
                                      metac_sema_stmt_t* stmt);

static inline void PrintSemaType(metac_printer_t* self,
                                 metac_sema_state_t* sema,
                                 metac_type_index_t typeIndex);

static inline void PrintSemaFunctionType(metac_printer_t* self,
                                         metac_sema_state_t* sema,
                                         metac_type_functiontype_t* funcType,
                                         metac_identifier_ptr_t optId)
{
    PrintSemaType(self, sema, funcType->ReturnType);
    PrintSpace(self);
    PrintChar(self, '(');
    PrintChar(self, '*');
    if (optId.v != 0)
    {
        PrintIdentifier(self, optId);
    }
    PrintChar(self, ')');
    PrintSpace(self);
    PrintChar(self, '(');

    const uint32_t paramCount = funcType->ParameterTypeCount;
    for(uint32_t i = 0;
        i < paramCount;
        i++)
    {
        PrintSemaType(self, sema, funcType->ParameterTypes[i]);
        if (i != paramCount - 1)
        {
            PrintChar(self, ',');
            PrintSpace(self);
        }
    }
    PrintChar(self, ')');
}

void TypeToCharsP(metac_sema_state_t* sema,
                  metac_printer_t* printer,
                  metac_type_index_t typeIndex)
{
    PrintSemaType(printer, sema, typeIndex);
}

static inline void PrintSemaType(metac_printer_t* self,
                                 metac_sema_state_t* sema,
                                 metac_type_index_t typeIndex)
{
    switch(TYPE_INDEX_KIND(typeIndex))
    {
        case type_index_basic:
        {
            decl_type_t basicType = {(metac_decl_kind_t)0};
            basicType.Kind = decl_type;
            metac_type_kind_t Kind =
                cast(metac_type_kind_t) TYPE_INDEX_INDEX(typeIndex);
            basicType.TypeKind = Kind;
            PrintType(self, &basicType);
        } break;
        case type_index_struct:
        {
            uint32_t structIdx = TYPE_INDEX_INDEX(typeIndex);
            metac_identifier_ptr_t structName =
                StructPtr(sema, structIdx)->Identifier;
            PrintString(self, "struct ", sizeof("struct"));
            if (structName.v != empty_identifier.v)
            {
                PrintIdentifier(self, structName);
            }
        } break;
        case type_index_union:
        {
            uint32_t unionIdx = TYPE_INDEX_INDEX(typeIndex);
            metac_identifier_ptr_t unionName =
                UnionPtr(sema, unionIdx)->Identifier;
            PrintString(self, "union ", sizeof("union"));
            if (unionName.v != empty_identifier.v)
            {
                PrintIdentifier(self, unionName);
            }
        } break;
        case type_index_tuple:
        {
            uint32_t tupleIdx = TYPE_INDEX_INDEX(typeIndex);
            PrintString(self, "{", sizeof("{") - 1);
            metac_type_tuple_t* tupleType = TupleTypePtr(sema, tupleIdx);
            const int32_t typeCount = cast(int32_t)tupleType->TypeCount;
            for(int32_t i = 0; i < typeCount - 1; i++)
            {
                PrintSemaType(self, sema, tupleType->TypeIndicies[i]);
                PrintString(self, ", ", sizeof(", ") - 1);
            }
            if (typeCount)
            {
                PrintSemaType(self, sema, tupleType->TypeIndicies[typeCount - 1]);
            }
            PrintString(self, "}", 1);
        } break;
        case type_index_enum:
        {
            uint32_t enumIdx = TYPE_INDEX_INDEX(typeIndex);
            metac_identifier_ptr_t enumName =
                EnumTypePtr(sema, enumIdx)->Name;
            PrintString(self, "enum ", sizeof("enum"));
            if (enumName.v != empty_identifier.v)
            {
                PrintIdentifier(self, enumName);
            }
        } break;
        case type_index_ptr:
        {
            uint32_t ptrIdx = TYPE_INDEX_INDEX(typeIndex);
            metac_type_index_t elementType =
                PtrTypePtr(sema, ptrIdx)->ElementType;
            PrintSemaType(self, sema, elementType);
            PrintString(self, "*", sizeof("*") - 1);
        } break;
        case type_index_functiontype:
        {
            uint32_t funcTypeIdx = TYPE_INDEX_INDEX(typeIndex);
            metac_type_functiontype_t* fnType =
                FunctiontypePtr(sema, funcTypeIdx);
            metac_identifier_ptr_t nullIdPtr = {};
            PrintSemaFunctionType(self, sema, fnType, nullIdPtr);
        } break;
        case type_index_array:
        {
            uint32_t arrayTypeIdx = TYPE_INDEX_INDEX(typeIndex);
            metac_type_array_t* arrayType = ArrayTypePtr(sema, arrayTypeIdx);
            PrintSemaType(self, sema, arrayType->ElementType);
            PrintChar(self, '[');
            if (arrayType->Dim != -1)
            {
                PrintI64(self, arrayType->Dim);
            }
            PrintChar(self, ']');
        } break;
        case 0: // HACK case 0 is not supposed to be here, it should not be emiited
                // however for now I am not going to track this issue down
        case type_index_invalid:
        {
            PrintString(self, "__error_type ", sizeof("__error_type ") - 1);
        } break;
        default:
        {
            fprintf(stderr, "Invalid type_index_kind: %x\n", TYPE_INDEX_KIND(typeIndex));
            assert(0);
        }
    }
}

static inline void PrintSemaVariable(metac_printer_t* self,
                                     metac_sema_state_t* sema,
                                     sema_decl_variable_t* variable)
{
    if (TYPE_INDEX_KIND(variable->TypeIndex) == type_index_functiontype)
    {
        metac_type_functiontype_t* funcType =
            FunctiontypePtr(sema, TYPE_INDEX_INDEX(variable->TypeIndex));
        PrintSemaFunctionType(self, sema, funcType, variable->VarIdentifier);
    }
    else
    {
        PrintSemaType(self, sema, variable->TypeIndex);
        PrintSpace(self);
        PrintIdentifier(self, variable->VarIdentifier);

       if (variable->VarInitExpr != emptyPointer)
        {
            assert(variable->VarInitExpr);

            PrintSpace(self);
            PrintToken(self, tok_assign);
            PrintSpace(self);
            PrintSemaExpr(self, sema, variable->VarInitExpr);
        }
    }


}

static inline void PrintSemaDecl(metac_printer_t* self,
                                        metac_sema_state_t* sema,
                                        metac_sema_decl_t* semaDecl,
                                        uint32_t level)
{
    bool printSemicolon = true;

    switch (semaDecl->Kind)
    {
        case decl_type_enum:
        {
            PrintIdentifier(self, semaDecl->sema_decl_type_enum.Name);
        } break;
        case decl_type_typedef:
            assert(0);
        case decl_type:
        {
            sema_decl_type_t* semaType = cast(sema_decl_type_t*) semaDecl;
            PrintSemaType(self, sema, semaType->typeIndex);
        } break;

        case decl_type_union :
        case decl_type_struct :
        {
            metac_type_aggregate_t* struct_ = (metac_type_aggregate_t*) semaDecl;
            PrintKeyword(self, AggToken(semaDecl->Kind));
            if (struct_->Identifier.v != empty_identifier.v)
            {
                PrintSpace(self);
                PrintIdentifier(self, struct_->Identifier);
            }
            PrintSpace(self);
            PrintToken(self, tok_lBrace);
            ++level;
            ++self->IndentLevel;
            PrintNewline(self);
            PrintIndent(self);
            metac_type_aggregate_field_t* f = struct_->Fields;
            for(uint32_t memberIndex = 0;
                memberIndex < struct_->FieldCount;
                memberIndex++)
            {
                //PrintSemaDecl(self, sema, f + memberIndex, level);
                sema_decl_variable_t synVar = {};
                synVar.TypeIndex = (f + memberIndex)->Type;
                synVar.VarIdentifier = (f + memberIndex)->Identifier;
                METAC_NODE(synVar.VarInitExpr) = emptyNode;
                PrintSemaVariable(self, sema, &synVar);
                PrintChar(self, ';');
                PrintNewline(self);
                if (memberIndex && memberIndex != (struct_->FieldCount - 1))
                    PrintIndent(self);

            }
            --self->IndentLevel;
            --level;
            //PrintNewline(self);
            PrintIndent(self);
            PrintToken(self, tok_rBrace);
            if (self->IndentLevel)
                PrintNewline(self);
            else
                PrintSpace(self);
        } break;
        case decl_type_array:
        {
            //PrintType(self, (decl_type_t*)decl);
            assert(0);
        } break;
        case decl_field :
        {
            metac_type_aggregate_field_t* field =
                cast(metac_type_aggregate_field_t*) semaDecl;
            PrintIdentifier(self, field->Identifier);
            //PrintVariable(self, field->Field);
            // assert(0);
        } break;
        case decl_variable:
        {
            sema_decl_variable_t* variable = (sema_decl_variable_t*) semaDecl;
            PrintSemaVariable(self, sema, variable);
        } break;
        case decl_function:
        {
            sema_decl_function_t* function_ = (sema_decl_function_t*) semaDecl;
            metac_type_functiontype_t* functionType =
                FunctiontypePtr(sema, TYPE_INDEX_INDEX(function_->TypeIndex));

            PrintSemaType(self, sema, functionType->ReturnType);
            PrintSpace(self);
            PrintIdentifier(self, function_->Identifier);

            PrintChar(self, '(');
            const uint32_t paramCount = functionType->ParameterTypeCount;
            for(uint32_t i = 0;
                i < paramCount;
                i++
            )
            {
                PrintSemaVariable(self, sema, function_->Parameters + i);
                if (i != (paramCount - 1))
                {
                    PrintChar(self, ',');
                    PrintSpace(self);
                }
            }
            PrintChar(self, ')');
            PrintSpace(self);
            if (function_->FunctionBody != emptyPointer)
            {
                PrintSemaStmt(self, sema, (metac_sema_stmt_t*)function_->FunctionBody);
                printSemicolon = false;
            }
        } break;
    }

    if (!self->AsType)
    {
        if (!!printSemicolon) PrintToken(self, tok_semicolon);
        if (!self->SuppressNewlineAfterDecl) PrintNewline(self);
    }
}

static inline void PrintSemaExpr(metac_printer_t* self,
                                 metac_sema_state_t* sema,
                                 metac_sema_expr_t* semaExpr)
{
    if (semaExpr->Kind == expr_paren)
    {
        if (!IsBinaryExp(semaExpr->E1->Kind))
            PrintChar(self, '(');

        PrintSemaExpr(self, sema,  semaExpr->E1);

        if (!IsBinaryExp(semaExpr->E1->Kind))
            PrintChar(self, ')');
    }
    else if (semaExpr->Kind == expr_variable)
    {
        // printf("Don't know how to print expr_variable\n");
        PrintIdentifier(self, semaExpr->Variable->VarIdentifier);
    }
    else if (semaExpr->Kind == expr_tuple)
    {
        PrintChar(self, '{');
        metac_sema_expr_t** tupleElement =
            semaExpr->TupleExprs;
        for(uint32_t i = 0;
            i < semaExpr->TupleExprCount;
            i++)
        {
            PrintSemaExpr(self, sema,  tupleElement[i]);
            if (i != (semaExpr->TupleExprCount - 1))
            {
                PrintChar(self, ',');
                PrintSpace(self);
            }
        }
        PrintChar(self, '}');
    }
    else if (semaExpr->Kind == expr_type)
    {
        PrintChar(self, '(');
        PrintSemaType(self, sema, semaExpr->TypeExp);
        PrintChar(self, ')');
    }
    else if (semaExpr->Kind == expr_identifier || semaExpr->Kind == expr_variable)
    {
        PrintIdentifier(self, semaExpr->IdentifierPtr);
    }
    else if (semaExpr->Kind == expr_string)
    {
        uint32_t stringLength = LENGTH_FROM_STRING_KEY(semaExpr->StringKey);
        PrintChar(self, '"');
        PrintString(self,
            IdentifierPtrToCharPtr(self->StringTable, semaExpr->StringPtr),
            stringLength);
        PrintChar(self, '"');
    }
    else if (semaExpr->Kind == expr_signed_integer)
    {
        PrintI64(self, semaExpr->ValueI64);
    }
    else if (semaExpr->Kind == expr_float)
    {
        PrintF23(self, semaExpr->ValueF23);
    }
    else if (semaExpr->Kind == expr_char)
    {
        PrintChar(self, '\'');
        PrintString(self, semaExpr->Chars, LENGTH_FROM_CHAR_KEY(semaExpr->CharKey));
        PrintChar(self, '\'');
    }
    else if (IsBinaryExp(semaExpr->Kind) && semaExpr->Kind != expr_index)
    {
        PrintChar(self, '(');
        PrintSemaExpr(self, sema,  semaExpr->E1);

        PrintSpace(self);
        const char* op = BinExpTypeToChars((metac_binary_expr_kind_t)semaExpr->Kind);
        PrintString(self, op, (uint32_t)strlen(op));
        PrintSpace(self);

        PrintSemaExpr(self, sema,  semaExpr->E2);
        PrintChar(self, ')');
    }
    else if (semaExpr->Kind == expr_cast)
    {
        PrintString(self, "cast", 4);
        PrintChar(self, '(');
        PrintSemaType(self, sema,semaExpr->CastType);
        PrintChar(self, ')');

        PrintSemaExpr(self, sema,  semaExpr->CastExp);
    }
    else if (semaExpr->Kind == expr_call)
    {
        PrintSemaExpr(self, sema,  semaExpr->E1);
        PrintChar(self, '(');
        sema_exp_argument_list_t* argList =
            semaExpr->E2->ArgumentList;
        metac_sema_expr_t** onePastLastArg =
            cast(metac_sema_expr_t**)
            argList->Arguments + argList->ArgumentCount;
        for(metac_sema_expr_t** arg = argList->Arguments;
            arg < onePastLastArg;
            arg++)
        {
            PrintSemaExpr(self, sema,  *arg);
            if (arg != (onePastLastArg - 1))
                PrintString(self, ", ", 2);
        }
        PrintChar(self, ')');
    }
    else if (semaExpr->Kind == expr_index)
    {
        PrintSemaExpr(self, sema,  semaExpr->E1);
        PrintToken(self, tok_lBracket);
        PrintSemaExpr(self, sema,  semaExpr->E2);
        PrintToken(self, tok_rBracket);
    }
    else if (semaExpr->Kind == expr_sizeof)
    {
        PrintKeyword(self, tok_kw_sizeof);
        PrintToken(self, tok_lParen);
        PrintSemaExpr(self, sema,  semaExpr->E1);
        PrintToken(self, tok_rParen);
    }
    else if (semaExpr->Kind == expr_addr || semaExpr->Kind == expr_deref
          || semaExpr->Kind == expr_not  || semaExpr->Kind == expr_compl
          || semaExpr->Kind == expr_umin)
    {
        {
            const char* op = 0;
            if (semaExpr->Kind == expr_addr)
                op = "&";
            else if (semaExpr->Kind == expr_deref)
                op = "*";
            else if (semaExpr->Kind == expr_not)
                op = "!";
            else if (semaExpr->Kind == expr_compl)
                op = "~";
            else if (semaExpr->Kind == expr_umin)
                op = "-";

            PrintString(self, op, (uint32_t)strlen(op));
        }

        if (!IsBinaryExp(semaExpr->E1->Kind))
            PrintChar(self, '(');

        PrintSemaExpr(self, sema,  semaExpr->E1);

        if (!IsBinaryExp(semaExpr->E1->Kind))
            PrintChar(self, ')');
    }
    else if (semaExpr->Kind == expr_post_increment || semaExpr->Kind == expr_post_decrement)
    {
        const char* op = 0;
        if (semaExpr->Kind == expr_post_increment)
            op = "++";
        else if (semaExpr->Kind == expr_post_decrement)
            op = "--";

        assert(op);

        if (!IsBinaryExp(semaExpr->E1->Kind))
            PrintChar(self, '(');

        PrintSemaExpr(self, sema,  semaExpr->E1);

        if (!IsBinaryExp(semaExpr->E1->Kind))
            PrintChar(self, ')');

        PrintString(self, op, (uint32_t)strlen(op));
    }
    else if (semaExpr->Kind == expr_inject || semaExpr->Kind == expr_eject
          || semaExpr->Kind == expr_typeof || semaExpr->Kind == expr_assert
          || semaExpr->Kind == expr_unary_dot)
    {
        {
            const char* op = 0;
            if (semaExpr->Kind == expr_inject)
                op = "inject";
            else if (semaExpr->Kind == expr_eject)
                op = "eject";
            else if (semaExpr->Kind == expr_typeof)
                op = "typeof";
            else if (semaExpr->Kind == expr_assert)
                op = "assert";
            else if (semaExpr->Kind == expr_unary_dot)
                op = ".";

            assert(op);

            PrintString(self, op, (uint32_t)strlen(op));
        }

        if (!IsBinaryExp(semaExpr->E1->Kind))
           PrintChar(self, '(');

        PrintSemaExpr(self, sema,  semaExpr->E1);

        if (!IsBinaryExp(semaExpr->E1->Kind))
            PrintChar(self, ')');
    }
    else if (semaExpr->Kind == expr_outer)
    {
        PrintChar(self, '$');
        PrintChar(self, '(');
        PrintSemaExpr(self, sema, semaExpr->E1);
        PrintChar(self, ')');
    }
    else if (semaExpr->Kind == expr_stringize)
    {
        PrintChar(self, '#');
        if (!IsBinaryExp(semaExpr->E1->Kind))
           PrintChar(self, '(');

        PrintSemaExpr(self, sema, semaExpr->E1);

        if (!IsBinaryExp(semaExpr->E1->Kind))
            PrintChar(self, ')');
    }
    else if ((cast(metac_node_kind_t)semaExpr->Kind) == node_decl_enum_member)
    {
        metac_enum_member_t* enumMember = cast(metac_enum_member_t*) semaExpr;
        //PrintSemaDecl(self, sema, enumMember, self->IndentLevel);
        PrintIdentifier(self, enumMember->Identifier);
    }
    else
    {
        printf("don't know how to print %s\n", (MetaCExprKind_toChars(semaExpr->Kind)));
    }
}


static inline void PrintSemaStmt(metac_printer_t* self, metac_sema_state_t* sema, metac_sema_stmt_t* stmt)
{
    // printf("Kind: %s\n", StmtKind_toChars(stmt->Kind));
    switch(stmt->Kind)
    {
        case stmt_casebody :
        {
            sema_stmt_casebody_t* stmtCasebody = cast(sema_stmt_casebody_t*) stmt;
            const uint32_t stmtCount = stmtCasebody->StmtCount;
            for(uint32_t i = 0;
                i < stmtCount;
                i++)
            {
                PrintSemaStmt(self, sema, stmtCasebody->Stmts[i]);

                if (i < (stmtCount - 1))
                {
                    PrintNewline(self);
                    PrintIndent(self);
                }
            }
        } break;
        case stmt_return :
        {
            sema_stmt_return_t* stmt_return = cast(sema_stmt_return_t*) stmt;

            PrintKeyword(self, tok_kw_return);
            PrintSpace(self);
            if (stmt_return->ReturnExp != emptyPointer)
                PrintSemaExpr(self, sema, stmt_return->ReturnExp);
            PrintToken(self, tok_semicolon);
        } break;
        case stmt_yield :
        {
            sema_stmt_yield_t* stmt_yield = cast(sema_stmt_yield_t*) stmt;

            PrintKeyword(self, tok_kw__yield);
            PrintSpace(self);
            if (stmt_yield->YieldExp != emptyPointer)
                PrintSemaExpr(self, sema, stmt_yield->YieldExp);
            PrintToken(self, tok_semicolon);
        } break;
        case stmt_block :
        {
            sema_stmt_block_t* blockStmt
                = cast(sema_stmt_block_t*) stmt;

            PrintToken(self, tok_lBrace);
            ++self->IndentLevel;
            PrintNewline(self);
            PrintIndent(self);


            const uint32_t stmtCount = blockStmt->StmtCount;
            for(uint32_t i = 0;
                i < stmtCount;
                i++)
            {
                PrintSemaStmt(self, sema, blockStmt->Body[i]);
                if(i < (stmtCount - 1))
                {
                    PrintNewline(self);
                    PrintIndent(self);
                }
            }

            --self->IndentLevel;
            PrintNewline(self);
            PrintIndent(self);
            PrintToken(self, tok_rBrace);
        } break;
        case stmt_if :
        {
            sema_stmt_if_t* stmt_if_ = cast(sema_stmt_if_t*) stmt;

            PrintKeyword(self, tok_kw_if);
            PrintSpace(self);
            PrintChar(self, '(');
            PrintSemaExpr(self, sema, stmt_if_->IfCond);
            PrintChar(self, ')');

            if (stmt_if_->IfBody->Kind != stmt_block)
                ++self->IndentLevel;
            PrintNewline(self);
            PrintIndent(self);
            PrintSemaStmt(self, sema, stmt_if_->IfBody);
            if (stmt_if_->IfBody->Kind != stmt_block)
            {

                --self->IndentLevel;
            }
            PrintNewline(self);
            PrintIndent(self);

            if (stmt_if_->ElseBody != emptyPointer)
            {
                PrintKeyword(self, tok_kw_else);
                if (stmt_if_->ElseBody->Kind == stmt_if)
                {
                    PrintSpace(self);
                }
                else
                {
                    if (stmt_if_->ElseBody->Kind != stmt_block)
                        ++self->IndentLevel;

                    PrintNewline(self);
                    PrintIndent(self);
                }
                PrintSemaStmt(self, sema, stmt_if_->ElseBody);

                if (stmt_if_->ElseBody->Kind != stmt_if)
                {
                    if (stmt_if_->ElseBody->Kind != stmt_block)
                        --self->IndentLevel;
                }

                PrintNewline(self);
                PrintIndent(self);
            }
        } break;
        case stmt_expr :
        {
            sema_stmt_expr_t* expr_stmt = cast(sema_stmt_expr_t*) stmt;
            PrintSemaExpr(self, sema, expr_stmt->Expr);
            PrintToken(self, tok_semicolon);
        } break;
        case stmt_decl:
        {
            sema_stmt_decl_t* decl_stmt = cast(sema_stmt_decl_t*) stmt;
            PrintSemaDecl(self, sema, decl_stmt->Decl, 0);
        } break;
        case stmt_for:
        {
            sema_stmt_for_t* stmt_for = cast(sema_stmt_for_t*) stmt;
            PrintKeyword(self, tok_kw_for);
            PrintChar(self, '(');
            if (stmt_for->ForInit != emptyPointer)
            {
                if (IsExprNode(stmt_for->ForInit->Kind))
                {
                    PrintSemaExpr(self, sema, (cast(metac_sema_expr_t*)stmt_for->ForInit));
                    PrintToken(self, tok_semicolon);
                    PrintSpace(self);
                }
                else
                {
                    //TODO use a stack for printer options
                    self->SuppressNewlineAfterDecl = true;
                    PrintSemaDecl(self, sema, (cast(metac_sema_decl_t*)stmt_for->ForInit), self->IndentLevel);
                    self->SuppressNewlineAfterDecl = false;
                    assert(!self->SuppressNewlineAfterDecl);
                }
            }
            else
            {
                PrintToken(self, tok_semicolon);
            }

            if (stmt_for->ForCond != cast(metac_sema_expr_t*) emptyPointer)
            {
                PrintSemaExpr(self, sema, stmt_for->ForCond);
            }

            PrintToken(self, tok_semicolon);
            if (stmt_for->ForCond != cast(metac_sema_expr_t*) emptyPointer)
            {
                PrintSpace(self);
            }

            if (stmt_for->ForPostLoop != cast(metac_sema_expr_t*) emptyPointer)
            {
                PrintSemaExpr(self, sema, stmt_for->ForPostLoop);
            }
            PrintChar(self, ')');
            PrintNewline(self);
            PrintIndent(self);
            PrintSemaStmt(self, sema, stmt_for->ForBody);
        } break;
        case stmt_break:
        {
            sema_stmt_break_t* stmt_break = cast(sema_stmt_break_t*) stmt;
            PrintKeyword(self, tok_kw_break);
            PrintToken(self, tok_semicolon);
        } break;
        case stmt_continue:
        {
            sema_stmt_continue_t* stmt_continue = cast(sema_stmt_continue_t*) stmt;
            PrintKeyword(self, tok_kw_continue);
            PrintToken(self, tok_semicolon);
        } break;
        case stmt_case:
        {
            sema_stmt_case_t* caseStmt = cast(sema_stmt_case_t*) stmt;
        LprintCase:
            if (caseStmt->CaseExp == (metac_sema_expr_t*) emptyPointer)
            {
                PrintKeyword(self, tok_kw_default);
            }
            else
            {
                PrintKeyword(self, tok_kw_case);
                PrintSpace(self);
                PrintSemaExpr(self, sema, caseStmt->CaseExp);
            }
            PrintChar(self, ':');
            if (caseStmt->CaseBody != cast(sema_stmt_casebody_t*) emptyPointer)
            {
                if (caseStmt->CaseBody->Kind == stmt_case)
                {
                    caseStmt = cast(sema_stmt_case_t*)
                        caseStmt->CaseBody;
                    PrintNewline(self);
                    PrintIndent(self);
                    goto LprintCase;
                }
                else if (caseStmt->CaseBody->Kind == stmt_casebody)
                {
                    sema_stmt_casebody_t* caseBody = caseStmt->CaseBody;
                    const uint32_t stmtCount = caseBody->StmtCount;
                    assert(stmtCount);

                    ++self->IndentLevel;
                    PrintNewline(self);
                    PrintIndent(self);
                    for(uint32_t i = 0;
                        i < stmtCount;
                        i++)
                    {
                        metac_sema_stmt_t* s = caseBody->Stmts[i];
                        PrintSemaStmt(self, sema, s);
                        if (i < (stmtCount - 1))
                        {
                            PrintIndent(self);
                        }
                    }
                    --self->IndentLevel;
                }
                else
                {
                    if (caseStmt->CaseBody->Kind != stmt_block)
                    {
                        ++self->IndentLevel;
                        PrintNewline(self);
                        PrintIndent(self);
                    }
                    PrintSemaStmt(self, sema, cast(metac_sema_stmt_t*)caseStmt->CaseBody);
                    if (caseStmt->CaseBody->Kind != stmt_block)
                        --self->IndentLevel;

                    PrintNewline(self);
                    PrintIndent(self);
                }
            }
        } break;
        case stmt_label:
        {
            sema_stmt_label_t* stmt_label = cast(sema_stmt_label_t*) stmt;
            PrintIdentifier(self, stmt_label->Label);
            PrintChar(self, ':');
        } break;
        case stmt_goto:
        {
            sema_stmt_goto_t* stmt_goto = cast(sema_stmt_goto_t*) stmt;
            PrintKeyword(self, tok_kw_goto);
            PrintSpace(self);
            PrintIdentifier(self, stmt_goto->GotoLabel);
            PrintToken(self, tok_semicolon);
        } break;
        case stmt_switch:
        {
            sema_stmt_switch_t* stmt_switch = cast(sema_stmt_switch_t*) stmt;
            PrintKeyword(self, tok_kw_switch);
            PrintSpace(self);
            PrintChar(self, '(');
            PrintSemaExpr(self, sema, stmt_switch->SwitchExp);
            PrintChar(self, ')');
            PrintNewline(self);
            PrintIndent(self);
            PrintSemaStmt(self, sema, cast(metac_sema_stmt_t*)stmt_switch->SwitchBody);
        } break;
        case stmt_while:
        {
            sema_stmt_while_t* stmt_while = (sema_stmt_while_t*)stmt;
            PrintKeyword(self, tok_kw_while);
            PrintSpace(self);
            PrintChar(self, '(');
            PrintSemaExpr(self, sema, stmt_while->WhileExp);
            PrintChar(self, ')');
            PrintNewline(self);
            PrintIndent(self);
            PrintSemaStmt(self, sema, stmt_while->WhileBody);
        } break;
        case stmt_comment:
        {
            stmt_comment_t* comment = (stmt_comment_t*)stmt;

            PrintString(self, "/*", 2);
            PrintStringWithNewline(self, comment->Text, comment->Length);
            PrintString(self, "*/", 2);
        } break;

        default : {
            fprintf(stderr,
                "Statement Kind: not handled by printer %s\n",
                    StmtKind_toChars(stmt->Kind));
            assert(0);
        }
    }
}


const char* MetaCPrinter_PrintSemaNode(metac_printer_t* self,
                                       metac_sema_state_t* sema,
                                       metac_node_t node)
{
    const char* result = self->StringMemory + self->StringMemorySize;
    uint32_t posBegin = self->StringMemorySize;

    if (node->Kind > node_expr_invalid && node->Kind < node_expr_max)
    {
        PrintSemaExpr(self, sema,  (metac_sema_expr_t*) node);
    }
    else if (node->Kind > node_stmt_min && node->Kind < node_stmt_max)
    {
        PrintSemaStmt(self, sema, (metac_sema_stmt_t*) node);
    }
    else if (node->Kind > node_decl_min && node->Kind < node_decl_max)
    {
        PrintSemaDecl(self, sema, (metac_sema_decl_t*) node, 0);
    }
    else
        assert(0);

    int advancement = self->StringMemorySize - posBegin;
    self->StringMemory[self->StringMemorySize++] = '\0';
    return result;
}

#endif // NO_SEMANTIC

void MetaCPrinter_Reset(metac_printer_t* self)
{
    memset(self->StringMemory, ' ', self->StringMemorySize);

    self->StringMemorySize = 0;
    self->IndentLevel = 0;
    self->CurrentColumn = 0;
    self->StartColumn = 0;
}

void MetaCPrinter_Init(metac_printer_t* self,
                       metac_identifier_table_t* identifierTable,
                       metac_identifier_table_t* stringTable)
{
    MetaCPrinter_InitSz(self,
                        identifierTable,
                        stringTable,
                        INITIAL_SIZE);
}

void MetaCPrinter_InitSz(metac_printer_t* self,
                         metac_identifier_table_t* identifierTable,
                         metac_identifier_table_t* stringTable,
                         uint32_t initialSize)
{
    self->StringMemoryCapacity = initialSize;
    self->StringMemory = (char*)malloc(self->StringMemoryCapacity);
    self->StringMemorySize = self->StringMemoryCapacity;
    self->SuppressNewlineAfterDecl = false;
    self->AsType = false;
    self->ForTypedef = false;
    MetaCPrinter_Reset(self);

    self->IdentifierTable = identifierTable;
    self->StringTable = stringTable;
}

void MetaCPrinter_Free(metac_printer_t* self)
{
    if (self->StringMemory)
    {
        free(self->StringMemory);
        self->StringMemory = 0;
        self->StringMemoryCapacity = self->StringMemorySize = 0;
    }
}

void MetacPrinter_PrintStringLiteral(metac_printer_t* self, const char* str)
{
    PrintString(self, str, (uint32_t)strlen(str));
}

void MetacPrinter_PrintI64(metac_printer_t* self, int64_t val)
{
    PrintI64(self, val);
}

const char* MetaCPrinter_PrintExpr(metac_printer_t* self, metac_expr_t* exp)
{
    const char* result = self->StringMemory + self->StringMemorySize;

    PrintExpr(self, exp);

    self->StringMemory[self->StringMemorySize++] = '\0';

    return result;
}

const char* MetaCPrinter_PrintDecl(metac_printer_t* self, metac_decl_t* decl)
{
    const char* result = self->StringMemory + self->StringMemorySize;

    PrintDecl(self, decl, 0);

    self->StringMemory[self->StringMemorySize++] = '\0';

    return result;
}

const char* MetaCPrinter_PrintStmt(metac_printer_t* self, metac_stmt_t* exp)
{
    const char* result = self->StringMemory + self->StringMemorySize;
    uint32_t posBegin = self->StringMemorySize;
    PrintStmt(self, exp);
    int advancement = self->StringMemorySize - posBegin;

    self->StringMemory[self->StringMemorySize++] = '\0';

    return result;
}

const char* MetaCPrinter_PrintNode(metac_printer_t* self, metac_node_t node, uint32_t level)
{
    const char* result = self->StringMemory + self->StringMemorySize;
    uint32_t posBegin = self->StringMemorySize;

    if (node->Kind > node_expr_invalid && node->Kind < node_expr_max)
    {
        PrintExpr(self, (metac_expr_t*) node);
    }
    else if ((cast(metac_stmt_kind_t)node->Kind) > stmt_min && (cast(metac_stmt_kind_t) node->Kind) < stmt_max)
    {
        PrintStmt(self, (metac_stmt_t*) node);
    }
    else if ((cast(metac_decl_kind_t)node->Kind) > decl_min && (cast(metac_decl_kind_t)node->Kind) < decl_max)
    {
        PrintDecl(self, (metac_decl_t*) node, level);
    }
    else
        assert(0);

    int advancement = self->StringMemorySize - posBegin;
    self->StringMemory[self->StringMemorySize++] = '\0';
    return result;
}

