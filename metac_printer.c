// metac-printer.c
#include "metac_printer.h"
#include "metac_lexer.h"
#include "metac_parser.h"
#include "int_to_str.c"
#include <assert.h>
#include <string.h>

static inline void PrintExpression(metac_printer_t* self, metac_expression_t* exp);
static inline void PrintSemaExpression(metac_printer_t* self, metac_semantic_state_t* sema, metac_sema_expression_t* exp);

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

    while((c = *string++))
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

    while((c = *string++))
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

    PrintString(self, ident, strlen(ident));
}

static void PrintKeyword(metac_printer_t* self,
                         metac_token_enum_t keyword)
{
    const char * str =
        MetaCTokenEnum_toChars(keyword) + sizeof("tok_kw");

    PrintString(self, str, strlen(str));
}

static inline void PrintToken(metac_printer_t* self,
                              metac_token_enum_t tokenType)
{
#define CASE_(KW) \
    case KW :

    switch(tokenType)
    {
        FOREACH_KEYWORD_TOKEN(CASE_)
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

static inline void PrintType(metac_printer_t* self, decl_type_t* type);
static inline void PrintParameterList(metac_printer_t* self,
                                     decl_parameter_t* Parameters);

static inline void PrintVariable(metac_printer_t* self,
                                 decl_variable_t* variable)
{
    if (variable->VarType->Kind == decl_type_functiontype)
    {
        decl_type_functiontype_t *funcType =
            (decl_type_functiontype_t*) variable->VarType;

        PrintType(self, funcType->ReturnType);
        PrintSpace(self);
        PrintChar(self, '(');
        PrintChar(self, '*');
        PrintIdentifier(self, variable->VarIdentifier);
        PrintChar(self, ')');
        PrintSpace(self);
        PrintParameterList(self, funcType->Parameters);
    }
    else
    {
        PrintType(self, variable->VarType);
        PrintSpace(self);
        PrintIdentifier(self, variable->VarIdentifier);
    }

    if (variable->VarInitExpression != emptyPointer)
    {
        PrintSpace(self);
        PrintToken(self, tok_assign);
        PrintSpace(self);
        PrintExpression(self, variable->VarInitExpression);
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
            PrintExpression(self, typeofType->Exp);
        } break;
        case decl_type_array:
        {
            decl_type_array_t *arrayType = (decl_type_array_t*) type;
            // PrintTypeName(self, )
            PrintType(self, arrayType->ElementType);
            PrintChar(self, '[');
            if (METAC_NODE(arrayType->Dim) != emptyNode)
                PrintExpression(self, arrayType->Dim);
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
                PrintString(self, "long long", sizeof("unsigned long long") - 1);
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
            }
            else
                assert(0);
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
        case decl_type_functiontype:
        {
            decl_type_functiontype_t *funcType = (decl_type_functiontype_t*) type;
            PrintType(self, funcType->ReturnType);
            PrintSpace(self);
            PrintChar(self, '(');
            PrintChar(self, '*');
            PrintString(self, "function) ", sizeof("function) ") - 1);
            PrintParameterList(self, funcType->Parameters);
        } break;
        default : assert(0);
    }
}

static inline void PrintDeclaration(metac_printer_t* self,
                                    metac_declaration_t* decl,
                                    uint32_t level);

#define CASE_MACRO(EXP_TYPE) \
    case EXP_TYPE : {result = #EXP_TYPE;} break;

const char* StatementKind_toChars(metac_statement_kind_t kind)
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
static inline void PrintStatement(metac_printer_t* self, metac_statement_t* stmt)
{
    // printf("Kind: %s\n", StatementKind_toChars(stmt->Kind));
    switch(stmt->Kind)
    {
        case stmt_return :
        {
            stmt_return_t* stmt_return = cast(stmt_return_t*) stmt;

            PrintKeyword(self, tok_kw_return);
            PrintSpace(self);
            if (stmt_return->ReturnExp != emptyPointer)
                PrintExpression(self, stmt_return->ReturnExp);
            PrintToken(self, tok_semicolon);
        } break;
        case stmt_yield :
        {
            stmt_yield_t* stmt_yield = cast(stmt_yield_t*) stmt;

            PrintKeyword(self, tok_kw_yield);
            PrintSpace(self);
            if (stmt_yield->YieldExp != emptyPointer)
                PrintExpression(self, stmt_yield->YieldExp);
            PrintToken(self, tok_semicolon);
        } break;
        case stmt_block :
        {
            stmt_block_t* stmt_block = cast(stmt_block_t*) stmt;

            PrintToken(self, tok_lBrace);
            ++self->IndentLevel;
            PrintNewline(self);
            PrintIndent(self);

            for(metac_statement_t* nextStmt = stmt_block->Body;
                nextStmt != emptyPointer;
                nextStmt = nextStmt->Next)
            {
                PrintStatement(self, nextStmt);
                if(nextStmt->Next)
                {
                    PrintNewline(self);
                    PrintIndent(self);
                }
            }

            --self->IndentLevel;
            if (stmt->Next)
            {
                PrintNewline(self);
                PrintIndent(self);
            }
            PrintToken(self, tok_rBrace);
        } break;
        case stmt_if :
        {
            stmt_if_t* stmt_if_ = cast(stmt_if_t*) stmt;

            PrintKeyword(self, tok_kw_if);
            PrintSpace(self);
            PrintChar(self, '(');
            PrintExpression(self, stmt_if_->IfCond);
            PrintChar(self, ')');

            if (stmt_if_->IfBody->Kind != stmt_block)
                ++self->IndentLevel;

            PrintNewline(self);
            PrintIndent(self);
            PrintStatement(self, stmt_if_->IfBody);
            if (stmt_if_->IfBody->Kind != stmt_block)
                --self->IndentLevel;

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
                PrintStatement(self, stmt_if_->ElseBody);

                if (stmt_if_->ElseBody->Kind != stmt_if)
                {
                    if (stmt_if_->ElseBody->Kind != stmt_block)
                        --self->IndentLevel;
                }

                PrintNewline(self);
                PrintIndent(self);
            }
        } break;
        case stmt_exp :
        {
            stmt_exp_t* exp_stmt = cast(stmt_exp_t*) stmt;
            PrintExpression(self, exp_stmt->Expression);
            PrintToken(self, tok_semicolon);
        } break;
        case stmt_decl:
        {
            stmt_decl_t* decl_stmt = cast(stmt_decl_t*) stmt;
            PrintDeclaration(self, decl_stmt->Declaration, 0);
        } break;
        case stmt_for:
        {
            stmt_for_t* stmt_for = cast(stmt_for_t*) stmt;
            PrintKeyword(self, tok_kw_for);
            PrintChar(self, '(');
            if (stmt_for->ForInit != cast(metac_node_t) emptyPointer)
            {
                if (IsExpressionNode(stmt_for->ForInit->Kind))
                {
                    PrintExpression(self, (metac_expression_t*)stmt_for->ForInit);
                }
                else
                {
                    self->SupressNewlineAfterDeclaration = true;
                    PrintDeclaration(self, (metac_declaration_t*)stmt_for->ForInit, 0);
                    PrintSpace(self);
                }
            }
            else
            {
                PrintToken(self, tok_semicolon);
            }

            if (stmt_for->ForCond != cast(metac_expression_t*) emptyPointer)
            {
                PrintExpression(self, stmt_for->ForCond);
            }
            PrintToken(self, tok_semicolon);
            if (stmt_for->ForPostLoop != cast(metac_expression_t*) emptyPointer)
            {
                PrintExpression(self, stmt_for->ForPostLoop);
            }
            PrintChar(self, ')');
            PrintNewline(self);
            PrintIndent(self);
            if (stmt_for->ForBody != (metac_statement_t*) emptyPointer)
            {
                PrintStatement(self, stmt_for->ForBody);
            }
        } break;
        case stmt_break:
        {
            stmt_break_t* stmt_break = cast(stmt_break_t*) stmt;
            PrintKeyword(self, tok_kw_break);
        } break;
        case stmt_continue:
        {
            stmt_continue_t* stmt_continue = cast(stmt_continue_t*) stmt;
            PrintKeyword(self, tok_kw_continue);
        } break;
        case stmt_case:
        {
            stmt_case_t* caseStatement = cast(stmt_case_t*) stmt;
            if (caseStatement->CaseExp == (metac_expression_t*) emptyPointer)
            {
                PrintKeyword(self, tok_kw_default);
            }
            else
            {
                PrintKeyword(self, tok_kw_case);
                PrintSpace(self);
                PrintExpression(self, caseStatement->CaseExp);
            }
            PrintChar(self, ':');
            if (caseStatement->CaseBody != cast(metac_statement_t*) emptyPointer)
            {
                if (caseStatement->CaseBody->Kind == stmt_block)
                {
                    PrintStatement(self, caseStatement->CaseBody);
                }
                else if (caseStatement->CaseBody->Kind == stmt_case)
                {
                    PrintNewline(self);
                    PrintIndent(self);
                    PrintStatement(self, caseStatement->CaseBody);
                }
                else
                {
                    ++self->IndentLevel;
                    PrintNewline(self);
                    PrintIndent(self);
                    metac_statement_t* stmt = caseStatement->CaseBody;
                    while(stmt && stmt != emptyPointer)
                    {
                        PrintStatement(self, stmt);
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
            PrintExpression(self, stmt_switch->SwitchExp);
            PrintChar(self, ')');
            PrintNewline(self);
            PrintIndent(self);
            PrintStatement(self, cast(metac_statement_t*)stmt_switch->SwitchBody);
        } break;
        case stmt_while:
        {
            stmt_while_t* stmt_while = (stmt_while_t*)stmt;
            PrintKeyword(self, tok_kw_while);
            PrintSpace(self);
            PrintChar(self, '(');
            PrintExpression(self, stmt_while->WhileExp);
            PrintChar(self, ')');
            PrintNewline(self);
            PrintIndent(self);
            PrintStatement(self, stmt_while->WhileBody);
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
                    StatementKind_toChars(stmt->Kind));
            assert(0);
        }
    }
}

static inline metac_token_enum_t AggToken(metac_declaration_kind_t declKind)
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

static inline void PrintDeclaration(metac_printer_t* self,
                                    metac_declaration_t* decl,
                                    uint32_t level)
{
    bool printSemicolon = true;

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
                    PrintExpression(self, member->Value);
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
            PrintDeclaration(self, (metac_declaration_t*)typdef->Type, level);
            if (typdef->Identifier.v != empty_identifier.v)
            {
                PrintIdentifier(self, typdef->Identifier);
            }
            level--;
        } break;
        case decl_type:
        {
            PrintType(self, (decl_type_t*) decl);
        }  break;
        case decl_type_union :
        case decl_type_struct :
        {
            decl_type_struct_t* struct_ = (decl_type_struct_t*) decl;
            PrintKeyword(self, AggToken(decl->Kind));
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
            decl_field_t* f = struct_->Fields;
            for(uint32_t memberIndex = 0;
                memberIndex < struct_->FieldCount;
                memberIndex++)
            {
                PrintDeclaration(self, (metac_declaration_t*)f, level);
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
            if (self->IndentLevel)
                PrintNewline(self);
            else
                PrintSpace(self);
        } break;
        case decl_type_array:
        {
            PrintType(self, (decl_type_t*)decl);
        } break;
        case decl_field :
        {
            decl_field_t* field = (decl_field_t*) decl;
            PrintVariable(self, field->Field);
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
                PrintStatement(self, (metac_statement_t*)function_->FunctionBody);
                printSemicolon = false;
            }
        } break;
        case decl_comment:
        {
            decl_comment_t* comment = (decl_comment_t*) decl;
            PrintComment(self, comment->Text, comment->Length);
        } break;
        case decl_label:
        {
            decl_label_t* label = (decl_label_t*) decl;
            PrintIdentifier(self, label->Identifier);
            PrintChar(self, ':');
        } break;
    }
    if (!!printSemicolon) PrintToken(self, tok_semicolon);
    if (self->SupressNewlineAfterDeclaration)
    {
        self->SupressNewlineAfterDeclaration = false;
    }
    else
    {
        PrintNewline(self);
    }
}

static inline void PrintExpression(metac_printer_t* self, metac_expression_t* exp)
{
    if (exp->Kind == exp_paren)
    {
        if (!IsBinaryExp(exp->E1->Kind))
            PrintChar(self, '(');

        PrintExpression(self, exp->E1);

        if (!IsBinaryExp(exp->E1->Kind))
            PrintChar(self, ')');
    }
    else if (exp->Kind == exp_ternary)
    {
        PrintChar(self, '(');
        PrintExpression(self, exp->Econd);
        PrintSpace(self);
        PrintChar(self, '?');
        PrintSpace(self);
        PrintExpression(self, exp->E1);
        PrintSpace(self);
        PrintChar(self, ':');
        PrintSpace(self);
        PrintExpression(self, exp->E2);
        PrintChar(self, ')');
    }
    else if (exp->Kind == exp_tuple)
    {
        PrintChar(self, '{');
        exp_tuple_t* tupleElement =
            exp->TupleExpressionList;
        for(uint32_t i = 0;
            i < exp->TupleExpressionCount;
            i++)
        {
            PrintExpression(self, tupleElement->Expression);
            if (i != (exp->TupleExpressionCount - 1))
            {
                PrintChar(self, ',');
                PrintSpace(self);
            }
            tupleElement = tupleElement->Next;
        }
        PrintChar(self, '}');
    }
    else if (exp->Kind == exp_type)
    {
        PrintChar(self, '(');
        PrintType(self, exp->TypeExp);
        PrintChar(self, ')');
    }
    else if (exp->Kind == exp_identifier)
    {
        PrintIdentifier(self, exp->IdentifierPtr);
    }
    else if (exp->Kind == exp_string)
    {
        uint32_t stringLength = LENGTH_FROM_STRING_KEY(exp->StringKey);
        PrintChar(self, '"');
        PrintString(self,
            IdentifierPtrToCharPtr(self->StringTable, exp->StringPtr),
            stringLength);
        PrintChar(self, '"');
    }
    else if (exp->Kind == exp_signed_integer)
    {
        PrintI64(self, exp->ValueI64);
    }
    else if (exp->Kind == exp_char)
    {
        PrintChar(self, '\'');
        PrintString(self, exp->Chars, LENGTH_FROM_CHAR_KEY(exp->CharKey));
        PrintChar(self, '\'');
    }
    else if (exp->Kind == exp_index)
    {
        PrintExpression(self, exp->E1);
        PrintToken(self, tok_lBracket);
        PrintExpression(self, exp->E2);
        PrintToken(self, tok_rBracket);
    }
    else if (IsBinaryExp(exp->Kind))
    {
        PrintChar(self, '(');
        PrintExpression(self, exp->E1);

        PrintSpace(self);
        const char* op = BinExpTypeToChars((metac_binary_expression_kind_t)exp->Kind);
        PrintString(self, op, strlen(op));
        PrintSpace(self);

        PrintExpression(self, exp->E2);
        PrintChar(self, ')');
    }
    else if (exp->Kind == exp_cast)
    {
        PrintString(self, "cast", 4);
        PrintChar(self, '(');
        PrintType(self, exp->CastType);
        PrintChar(self, ')');

        PrintExpression(self, exp->CastExp);
    }
    else if (exp->Kind == exp_call)
    {
        PrintExpression(self, exp->E1);
        if (METAC_NODE(exp->E2) != emptyPointer)
        {
            PrintExpression(self, exp->E2);
        }
        else
        {
            PrintString(self, "()", 2);
        }
    }
    else if (exp->Kind == exp_template_instance)
    {
        PrintExpression(self, exp->E1);
        PrintChar(self, '!');
        PrintExpression(self, exp->E2);
    }
    else if (exp->Kind == exp_argument)
    {
        PrintChar(self, '(');
        for(exp_argument_t* arg = (exp_argument_t*)exp;
            arg != emptyPointer;
            arg = arg->Next)
        {
            PrintExpression(self, arg->Expression);
            if (arg->Next != emptyPointer)
                PrintString(self, ", ", 2);
        }
        PrintChar(self, ')');
    }
    else if (exp->Kind == exp_sizeof)
    {
        PrintKeyword(self, tok_kw_sizeof);
        PrintToken(self, tok_lParen);
        PrintExpression(self, exp->E1);
        PrintToken(self, tok_rParen);
    }
    else if (exp->Kind == exp_addr || exp->Kind == exp_ptr
          || exp->Kind == exp_not  || exp->Kind == exp_compl
          || exp->Kind == exp_umin)
    {
        {
            const char* op = 0;
            if (exp->Kind == exp_addr)
                op = "&";
            else if (exp->Kind == exp_ptr)
                op = "*";
            else if (exp->Kind == exp_not)
                op = "!";
            else if (exp->Kind == exp_compl)
                op = "~";
            else if (exp->Kind == exp_umin)
                op = "-";

            PrintString(self, op, strlen(op));
        }

        if (!IsBinaryExp(exp->E1->Kind))
            PrintChar(self, '(');

        PrintExpression(self, exp->E1);

        if (!IsBinaryExp(exp->E1->Kind))
            PrintChar(self, ')');
    }
    else if (exp->Kind == exp_post_increment || exp->Kind == exp_post_decrement)
    {
        const char* op = 0;
        if (exp->Kind == exp_post_increment)
            op = "++";
        else if (exp->Kind == exp_post_decrement)
            op = "--";

        assert(op);

        if (!IsBinaryExp(exp->E1->Kind))
            PrintChar(self, '(');

        PrintExpression(self, exp->E1);

        if (!IsBinaryExp(exp->E1->Kind))
            PrintChar(self, ')');

        PrintString(self, op, strlen(op));
    }
    else if (exp->Kind == exp_inject || exp->Kind == exp_eject
          || exp->Kind == exp_typeof || exp->Kind == exp_assert
          || exp->Kind == exp_unary_dot)
    {
        {
            const char* op = 0;
            if (exp->Kind == exp_inject)
                op = "inject";
            else if (exp->Kind == exp_eject)
                op = "eject";
            else if (exp->Kind == exp_typeof)
                op = "typeof";
            else if (exp->Kind == exp_assert)
                op = "assert";
            else if (exp->Kind == exp_unary_dot)
                op = ".";

            assert(op);

            PrintString(self, op, strlen(op));
        }

        if (!IsBinaryExp(exp->E1->Kind))
           PrintChar(self, '(');

        PrintExpression(self, exp->E1);

        if (!IsBinaryExp(exp->E1->Kind))
            PrintChar(self, ')');
    }
    else
    {
        printf("don't know how to print %s\n", (MetaCExpressionKind_toChars(exp->Kind)));
    }
}

#ifndef NO_SEMANTIC

#include "metac_semantic.h"

static inline void PrintSemaStatement(metac_printer_t* self,
                                      metac_semantic_state_t* sema,
                                      metac_sema_statement_t* stmt);

static inline void PrintSemaType(metac_printer_t* self,
                                 metac_semantic_state_t* sema,
                                 metac_type_index_t typeIndex);

static inline void PrintSemaFunctionType(metac_printer_t* self,
                                         metac_semantic_state_t* sema,
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

static inline void PrintSemaType(metac_printer_t* self,
                                 metac_semantic_state_t* sema,
                                 metac_type_index_t typeIndex)
{
    switch(TYPE_INDEX_KIND(typeIndex))
    {
        case type_index_basic:
        {
            decl_type_t basicType = {(metac_declaration_kind_t)0};
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
            const int32_t typeCount = cast(int32_t)tupleType->typeCount;
            for(int32_t i = 0; i < typeCount - 1; i++)
            {
                PrintSemaType(self, sema, tupleType->typeIndicies[i]);
                PrintString(self, ", ", sizeof(", ") - 1);
            }
            if (typeCount)
            {
                PrintSemaType(self, sema, tupleType->typeIndicies[typeCount - 1]);
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
        default:
        {
            fprintf(stderr, "Invalid type_index_kind: %x\n", TYPE_INDEX_KIND(typeIndex));
            assert(0);
        }
    }
}

static inline void PrintSemaVariable(metac_printer_t* self,
                                     metac_semantic_state_t* sema,
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

       if (variable->VarInitExpression != emptyPointer)
        {
            assert(variable->VarInitExpression);

            PrintSpace(self);
            PrintToken(self, tok_assign);
            PrintSpace(self);
            PrintSemaExpression(self, sema, variable->VarInitExpression);
        }
    }


}

static inline void PrintSemaDeclaration(metac_printer_t* self,
                                        metac_semantic_state_t* sema,
                                        metac_sema_declaration_t* semaDecl,
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
                //PrintSemaDeclaration(self, sema, f + memberIndex, level);
                sema_decl_variable_t synVar = {};
                synVar.TypeIndex = (f + memberIndex)->Type;
                synVar.VarIdentifier = (f + memberIndex)->Identifier;
                METAC_NODE(synVar.VarInitExpression) = emptyNode;
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
                PrintSemaStatement(self, sema, (metac_sema_statement_t*)function_->FunctionBody);
                printSemicolon = false;
            }
        } break;
    }
    if (!self->AsType)
    {
        if (!!printSemicolon) PrintToken(self, tok_semicolon);
        PrintNewline(self);
    }
}

static inline void PrintSemaExpression(metac_printer_t* self,
                                       metac_semantic_state_t* sema,
                                       metac_sema_expression_t* semaExp)
{
    if (semaExp->Kind == exp_paren)
    {
        if (!IsBinaryExp(semaExp->E1->Kind))
            PrintChar(self, '(');

        PrintSemaExpression(self, sema,  semaExp->E1);

        if (!IsBinaryExp(semaExp->E1->Kind))
            PrintChar(self, ')');
    }
    else if (semaExp->Kind == exp_variable)
    {
        // printf("Don't know how to print exp_variable\n");
        PrintIdentifier(self, semaExp->Variable->VarIdentifier);
    }
    else if (semaExp->Kind == exp_tuple)
    {
        PrintChar(self, '{');
        metac_sema_expression_t** tupleElement =
            semaExp->TupleExpressions;
        for(uint32_t i = 0;
            i < semaExp->TupleExpressionCount;
            i++)
        {
            PrintSemaExpression(self, sema,  tupleElement[i]);
            if (i != (semaExp->TupleExpressionCount - 1))
            {
                PrintChar(self, ',');
                PrintSpace(self);
            }
        }
        PrintChar(self, '}');
    }
    else if (semaExp->Kind == exp_type)
    {
        PrintChar(self, '(');
        PrintSemaType(self, sema, semaExp->TypeExp);
        PrintChar(self, ')');
    }
    else if (semaExp->Kind == exp_identifier || semaExp->Kind == exp_variable)
    {
        PrintIdentifier(self, semaExp->IdentifierPtr);
    }
    else if (semaExp->Kind == exp_string)
    {
        uint32_t stringLength = LENGTH_FROM_STRING_KEY(semaExp->StringKey);
        PrintChar(self, '"');
        PrintString(self,
            IdentifierPtrToCharPtr(self->StringTable, semaExp->StringPtr),
            stringLength);
        PrintChar(self, '"');
    }
    else if (semaExp->Kind == exp_signed_integer)
    {
        PrintI64(self, semaExp->ValueI64);
    }
    else if (semaExp->Kind == exp_char)
    {
        PrintChar(self, '\'');
        PrintString(self, semaExp->Chars, LENGTH_FROM_CHAR_KEY(semaExp->CharKey));
        PrintChar(self, '\'');
    }
    else if (IsBinaryExp(semaExp->Kind) && semaExp->Kind != exp_index)
    {
        PrintChar(self, '(');
        PrintSemaExpression(self, sema,  semaExp->E1);

        PrintSpace(self);
        const char* op = BinExpTypeToChars((metac_binary_expression_kind_t)semaExp->Kind);
        PrintString(self, op, strlen(op));
        PrintSpace(self);

        PrintSemaExpression(self, sema,  semaExp->E2);
        PrintChar(self, ')');
    }
    else if (semaExp->Kind == exp_cast)
    {
        PrintString(self, "cast", 4);
        PrintChar(self, '(');
        PrintSemaType(self, sema,semaExp->CastType);
        PrintChar(self, ')');

        PrintSemaExpression(self, sema,  semaExp->CastExp);
    }
    else if (semaExp->Kind == exp_call)
    {
        PrintSemaExpression(self, sema,  semaExp->E1);
        PrintChar(self, '(');
        sema_exp_argument_list_t* argList =
            semaExp->E2->ArgumentList;
        metac_sema_expression_t** onePastLastArg =
            cast(metac_sema_expression_t**)
            argList->Arguments + argList->ArgumentCount;
        for(metac_sema_expression_t** arg = argList->Arguments;
            arg < onePastLastArg;
            arg++)
        {
            PrintSemaExpression(self, sema,  *arg);
            if (arg != (onePastLastArg - 1))
                PrintString(self, ", ", 2);
        }
        PrintChar(self, ')');
    }
    else if (semaExp->Kind == exp_index)
    {
        PrintSemaExpression(self, sema,  semaExp->E1);
        PrintToken(self, tok_lBracket);
        PrintSemaExpression(self, sema,  semaExp->E2);
        PrintToken(self, tok_rBracket);
    }
    else if (semaExp->Kind == exp_sizeof)
    {
        PrintKeyword(self, tok_kw_sizeof);
        PrintToken(self, tok_lParen);
        PrintSemaExpression(self, sema,  semaExp->E1);
        PrintToken(self, tok_rParen);
    }
    else if (semaExp->Kind == exp_addr || semaExp->Kind == exp_ptr
          || semaExp->Kind == exp_not  || semaExp->Kind == exp_compl
          || semaExp->Kind == exp_umin)
    {
        {
            const char* op = 0;
            if (semaExp->Kind == exp_addr)
                op = "&";
            else if (semaExp->Kind == exp_ptr)
                op = "*";
            else if (semaExp->Kind == exp_not)
                op = "!";
            else if (semaExp->Kind == exp_compl)
                op = "~";
            else if (semaExp->Kind == exp_umin)
                op = "-";

            PrintString(self, op, strlen(op));
        }

        if (!IsBinaryExp(semaExp->E1->Kind))
            PrintChar(self, '(');

        PrintSemaExpression(self, sema,  semaExp->E1);

        if (!IsBinaryExp(semaExp->E1->Kind))
            PrintChar(self, ')');
    }
    else if (semaExp->Kind == exp_post_increment || semaExp->Kind == exp_post_decrement)
    {
        const char* op = 0;
        if (semaExp->Kind == exp_post_increment)
            op = "++";
        else if (semaExp->Kind == exp_post_decrement)
            op = "--";

        assert(op);

        if (!IsBinaryExp(semaExp->E1->Kind))
            PrintChar(self, '(');

        PrintSemaExpression(self, sema,  semaExp->E1);

        if (!IsBinaryExp(semaExp->E1->Kind))
            PrintChar(self, ')');

        PrintString(self, op, strlen(op));
    }
    else if (semaExp->Kind == exp_inject || semaExp->Kind == exp_eject
          || semaExp->Kind == exp_typeof || semaExp->Kind == exp_assert
          || semaExp->Kind == exp_unary_dot)
    {
        {
            const char* op = 0;
            if (semaExp->Kind == exp_inject)
                op = "inject";
            else if (semaExp->Kind == exp_eject)
                op = "eject";
            else if (semaExp->Kind == exp_typeof)
                op = "typeof";
            else if (semaExp->Kind == exp_assert)
                op = "assert";
            else if (semaExp->Kind == exp_unary_dot)
                op = ".";

            assert(op);

            PrintString(self, op, strlen(op));
        }

        if (!IsBinaryExp(semaExp->E1->Kind))
           PrintChar(self, '(');

        PrintSemaExpression(self, sema,  semaExp->E1);

        if (!IsBinaryExp(semaExp->E1->Kind))
            PrintChar(self, ')');
    }
    else if (semaExp->Kind == decl_enum_member)
    {
        metac_enum_member_t* enumMember = cast(metac_enum_member_t*) semaExp;
        //PrintSemaDeclaration(self, sema, enumMember, self->IndentLevel);
        PrintIdentifier(self, enumMember->Identifier);
    }
    else
    {
        printf("don't know how to print %s\n", (MetaCExpressionKind_toChars(semaExp->Kind)));
    }
}


static inline void PrintSemaStatement(metac_printer_t* self, metac_semantic_state_t* sema, metac_sema_statement_t* stmt)
{
    // printf("Kind: %s\n", StatementKind_toChars(stmt->Kind));
    switch(stmt->Kind)
    {
        case stmt_casebody :
        {
            sema_stmt_casebody_t* stmtCasebody = cast(sema_stmt_casebody_t*) stmt;
            const uint32_t statementCount = stmtCasebody->StatementCount;
            for(uint32_t i = 0;
                i < statementCount;
                i++)
            {
                PrintSemaStatement(self, sema, stmtCasebody->Statements[i]);

                if (i < (statementCount - 1))
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
                PrintSemaExpression(self, sema, stmt_return->ReturnExp);
            PrintToken(self, tok_semicolon);
        } break;
        case stmt_yield :
        {
            sema_stmt_yield_t* stmt_yield = cast(sema_stmt_yield_t*) stmt;

            PrintKeyword(self, tok_kw_yield);
            PrintSpace(self);
            if (stmt_yield->YieldExp != emptyPointer)
                PrintSemaExpression(self, sema, stmt_yield->YieldExp);
            PrintToken(self, tok_semicolon);
        } break;
        case stmt_block :
        {
            sema_stmt_block_t* blockStatement
                = cast(sema_stmt_block_t*) stmt;

            PrintToken(self, tok_lBrace);
            ++self->IndentLevel;
            PrintNewline(self);
            PrintIndent(self);


            const uint32_t statementCount = blockStatement->StatementCount;
            for(uint32_t i = 0;
                i < statementCount;
                i++)
            {
                PrintSemaStatement(self, sema, blockStatement->Body[i]);
                if(i < (statementCount - 1))
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
            PrintSemaExpression(self, sema, stmt_if_->IfCond);
            PrintChar(self, ')');

            if (stmt_if_->IfBody->Kind != stmt_block)
                ++self->IndentLevel;
            PrintNewline(self);
            PrintIndent(self);
            PrintSemaStatement(self, sema, stmt_if_->IfBody);
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
                PrintSemaStatement(self, sema, stmt_if_->ElseBody);

                if (stmt_if_->ElseBody->Kind != stmt_if)
                {
                    if (stmt_if_->ElseBody->Kind != stmt_block)
                        --self->IndentLevel;
                }

                PrintNewline(self);
                PrintIndent(self);
            }
        } break;
        case stmt_exp :
        {
            sema_stmt_exp_t* exp_stmt = cast(sema_stmt_exp_t*) stmt;
            PrintSemaExpression(self, sema, exp_stmt->Expression);
            PrintToken(self, tok_semicolon);
        } break;
        case stmt_decl:
        {
            sema_stmt_decl_t* decl_stmt = cast(sema_stmt_decl_t*) stmt;
            PrintSemaDeclaration(self, sema, decl_stmt->Declaration, 0);
        } break;
        case stmt_for:
        {
            sema_stmt_for_t* stmt_for = cast(sema_stmt_for_t*) stmt;
            PrintKeyword(self, tok_kw_for);
            PrintChar(self, '(');
            if (stmt_for->ForInit != emptyPointer)
            {
                if (IsExpressionNode(stmt_for->ForInit->Kind))
                {
                    PrintSemaExpression(self, sema, (cast(metac_sema_expression_t*)stmt_for->ForInit));
                }
                else
                {
                    self->SupressNewlineAfterDeclaration = true;
                    PrintSemaDeclaration(self, sema, (cast(metac_sema_declaration_t*)stmt_for->ForInit), self->IndentLevel);
                }
            }
            else
            {
                PrintToken(self, tok_semicolon);
            }

            if (stmt_for->ForCond != cast(metac_sema_expression_t*) emptyPointer)
            {
                PrintSemaExpression(self, sema, stmt_for->ForCond);
            }
            PrintToken(self, tok_semicolon);
            if (stmt_for->ForPostLoop != cast(metac_sema_expression_t*) emptyPointer)
            {
                PrintSemaExpression(self, sema, stmt_for->ForPostLoop);
            }
            PrintChar(self, ')');
            PrintNewline(self);
            PrintIndent(self);
            PrintSemaStatement(self, sema, stmt_for->ForBody);
        } break;
        case stmt_break:
        {
            sema_stmt_break_t* stmt_break = cast(sema_stmt_break_t*) stmt;
            PrintKeyword(self, tok_kw_break);
        } break;
        case stmt_continue:
        {
            sema_stmt_continue_t* stmt_continue = cast(sema_stmt_continue_t*) stmt;
            PrintKeyword(self, tok_kw_continue);
        } break;
        case stmt_case:
        {
            sema_stmt_case_t* caseStatement = cast(sema_stmt_case_t*) stmt;
        LprintCase:
            if (caseStatement->CaseExp == (metac_sema_expression_t*) emptyPointer)
            {
                PrintKeyword(self, tok_kw_default);
            }
            else
            {
                PrintKeyword(self, tok_kw_case);
                PrintSpace(self);
                PrintSemaExpression(self, sema, caseStatement->CaseExp);
            }
            PrintChar(self, ':');
            if (caseStatement->CaseBody != cast(sema_stmt_casebody_t*) emptyPointer)
            {
                if (caseStatement->CaseBody->Kind == stmt_case)
                {
                    caseStatement = cast(sema_stmt_case_t*)
                        caseStatement->CaseBody;
                    PrintNewline(self);
                    PrintIndent(self);
                    goto LprintCase;
                }
                else if (caseStatement->CaseBody->Kind == stmt_casebody)
                {
                    sema_stmt_casebody_t* caseBody = caseStatement->CaseBody;
                    const uint32_t statementCount = caseBody->StatementCount;
                    assert(statementCount);

                    ++self->IndentLevel;
                    PrintNewline(self);
                    PrintIndent(self);
                    for(uint32_t i = 0;
                        i < statementCount;
                        i++)
                    {
                        metac_sema_statement_t* s = caseBody->Statements[i];
                        PrintSemaStatement(self, sema, s);
                        if (i < (statementCount - 1))
                        {
                            PrintIndent(self);
                        }
                    }
                    --self->IndentLevel;
                }
                else
                {
                    if (caseStatement->CaseBody->Kind != stmt_block)
                    {
                        ++self->IndentLevel;
                        PrintNewline(self);
                        PrintIndent(self);
                    }
                    PrintSemaStatement(self, sema, cast(metac_sema_statement_t*)caseStatement->CaseBody);
                    if (caseStatement->CaseBody->Kind != stmt_block)
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
            PrintSemaExpression(self, sema, stmt_switch->SwitchExp);
            PrintChar(self, ')');
            PrintNewline(self);
            PrintIndent(self);
            PrintSemaStatement(self, sema, cast(metac_sema_statement_t*)stmt_switch->SwitchBody);
        } break;
        case stmt_while:
        {
            sema_stmt_while_t* stmt_while = (sema_stmt_while_t*)stmt;
            PrintKeyword(self, tok_kw_while);
            PrintSpace(self);
            PrintChar(self, '(');
            PrintSemaExpression(self, sema, stmt_while->WhileExp);
            PrintChar(self, ')');
            PrintNewline(self);
            PrintIndent(self);
            PrintSemaStatement(self, sema, stmt_while->WhileBody);
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
                    StatementKind_toChars(stmt->Kind));
            assert(0);
        }
    }
}


const char* MetaCPrinter_PrintSemaNode(metac_printer_t* self,
                                       metac_semantic_state_t* sema,
                                       metac_node_t node)
{
    const char* result = self->StringMemory + self->StringMemorySize;
    uint32_t posBegin = self->StringMemorySize;

    if (node->Kind > node_exp_invalid && node->Kind < node_exp_max)
    {
        PrintSemaExpression(self, sema,  (metac_sema_expression_t*) node);
    }
    else if (node->Kind > stmt_min && node->Kind < stmt_max)
    {
        PrintSemaStatement(self, sema, (metac_sema_statement_t*) node);
    }
    else if (node->Kind > decl_min && node->Kind < decl_max)
    {
        PrintSemaDeclaration(self, sema, (metac_sema_declaration_t*) node, 0);
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
    self->SupressNewlineAfterDeclaration = false;
    self->AsType = false;
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
    PrintString(self, str, strlen(str));
}

void MetacPrinter_PrintI64(metac_printer_t* self, int64_t val)
{
    PrintI64(self, val);
}

const char* MetaCPrinter_PrintExpression(metac_printer_t* self, metac_expression_t* exp)
{
    const char* result = self->StringMemory + self->StringMemorySize;

    PrintExpression(self, exp);

    self->StringMemory[self->StringMemorySize++] = '\0';

    return result;
}

const char* MetaCPrinter_PrintDeclaration(metac_printer_t* self, metac_declaration_t* decl)
{
    const char* result = self->StringMemory + self->StringMemorySize;

    PrintDeclaration(self, decl, 0);

    self->StringMemory[self->StringMemorySize++] = '\0';

    return result;
}

const char* MetaCPrinter_PrintStatement(metac_printer_t* self, metac_statement_t* exp)
{
    const char* result = self->StringMemory + self->StringMemorySize;
    uint32_t posBegin = self->StringMemorySize;
    PrintStatement(self, exp);
    int advancement = self->StringMemorySize - posBegin;

    self->StringMemory[self->StringMemorySize++] = '\0';

    return result;
}

const char* MetaCPrinter_PrintNode(metac_printer_t* self, metac_node_t node, uint32_t level)
{
    const char* result = self->StringMemory + self->StringMemorySize;
    uint32_t posBegin = self->StringMemorySize;

    if (node->Kind > node_exp_invalid && node->Kind < node_exp_max)
    {
        PrintExpression(self, (metac_expression_t*) node);
    }
    else if (node->Kind > stmt_min && node->Kind < stmt_max)
    {
        PrintStatement(self, (metac_statement_t*) node);
    }
    else if (node->Kind > decl_min && node->Kind < decl_max)
    {
        PrintDeclaration(self, (metac_declaration_t*) node, level);
    }
    else
        assert(0);

    int advancement = self->StringMemorySize - posBegin;
    self->StringMemory[self->StringMemorySize++] = '\0';
    return result;

}

