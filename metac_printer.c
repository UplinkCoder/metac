// metac-printer.c
#include "metac_printer.h"
#include "metac_lexer.h"
#include "metac_parser.h"
#include "int_to_str.c"
#include <assert.h>
#include <string.h>

#define abort()

static inline void PrintExpression(metac_printer_t* self, metac_expression_t* exp);

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
            abort();
        }
    }
}

static inline void PrintString(metac_printer_t* self,
                 const char* string, uint32_t length)
{
    char c;

    while((c = *string++))
    {
        self->StringMemory[self->StringMemorySize++] = c;
    }
    self->CurrentColumn += length;
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
    if (variable->VarType->DeclKind == decl_type_functiontype)
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
    switch(type->DeclKind)
    {
        case decl_type_array:
        {
            decl_type_array_t *arrayType = (decl_type_array_t*) type;
            // PrintTypeName(self, )
            PrintType(self, arrayType->ElementType);
            PrintChar(self, '[');
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
            }
            if (type->TypeKind >= type_auto && type->TypeKind <= type_double)
            {
                metac_token_enum_t tok = (metac_token_enum_t)
                    ((type->TypeKind - type_auto) + tok_kw_auto);
                PrintKeyword(self, tok);
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
            else
                assert(0);
        } break;
        case decl_type_struct :
        {
            decl_type_struct_t* structType = (decl_type_struct_t*) type;
            PrintIdentifier(self, structType->Identifier);
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
    PrintString(self, Text, Length);
    PrintString(self, "*/", 2);
}
static inline void PrintStatement(metac_printer_t* self, metac_statement_t* stmt)
{
    // printf("StmtKind: %s\n", StatementKind_toChars(stmt->StmtKind));
    switch(stmt->StmtKind)
    {
        case stmt_return :
        {
            stmt_return_t* stmt_return = cast(stmt_return_t*) stmt;

            PrintKeyword(self, tok_kw_return);
            PrintSpace(self);
            if (stmt_return->Expression != emptyPointer)
                PrintExpression(self, stmt_return->Expression);
            PrintChar(self, ';');
        } break;
        case stmt_yield :
        {
            stmt_yield_t* stmt_yield = cast(stmt_yield_t*) stmt;

            PrintKeyword(self, tok_kw_yield);
            PrintSpace(self);
            PrintExpression(self, stmt_yield->Expression);
            PrintChar(self, ';');
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

            if (stmt_if_->IfBody->StmtKind != stmt_block)
                ++self->IndentLevel;
            PrintNewline(self);
            PrintIndent(self);
            PrintStatement(self, stmt_if_->IfBody);
            if (stmt_if_->IfBody->StmtKind != stmt_block)
            {

                --self->IndentLevel;
            }
            PrintNewline(self);
            PrintIndent(self);

            if (stmt_if_->ElseBody != emptyPointer)
            {
                PrintKeyword(self, tok_kw_else);
                if (stmt_if_->ElseBody->StmtKind == stmt_if)
                {
                    PrintSpace(self);
                }
                else
                {
                    if (stmt_if_->ElseBody->StmtKind != stmt_block)
                        ++self->IndentLevel;

                    PrintNewline(self);
                    PrintIndent(self);
                }
                PrintStatement(self, stmt_if_->ElseBody);

                if (stmt_if_->ElseBody->StmtKind != stmt_if)
                {
                    if (stmt_if_->ElseBody->StmtKind != stmt_block)
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
            PrintToken(self, tok_semicolon);
        } break;
        case stmt_for:
        {
            stmt_return_t* stmt_return = cast(stmt_return_t*) stmt;

        } break;
        default : {
            fprintf(stderr,
                "Statement Kind: not handled by printer\n");
                //MetaCStatementKind_toChars(stmt->StmtKind));
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

    switch (decl->DeclKind)
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
            PrintIdentifier(self, typdef->Identifier);
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
            PrintKeyword(self, AggToken(decl->DeclKind));
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
            PrintParameterList(self, function_->Parameters);

            if (function_->FunctionBody != emptyPointer)
            {
                PrintStatement(self, (metac_statement_t*)function_->FunctionBody);
                printSemicolon = false;
            }
        } break;
        case decl_comment:
        {
            decl_comment_t* comment = (decl_comment_t*) decl;
            PrintComment(self, comment->Text, comment->Length);
        } break;
    }
    if (!!printSemicolon) PrintChar(self, ';');
    PrintNewline(self);
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
        PrintChar(self, '(');

        for(exp_argument_t* arg = (exp_argument_t*)exp->E2;
            arg != emptyPointer;
            arg = arg->Next)
        {
            PrintExpression(self, arg->Expression);
            if (arg->Next != emptyPointer)
                PrintString(self, ", ", 2);
        }
        PrintChar(self, ')');
    }
    else if (exp->Kind == exp_index)
    {
        PrintExpression(self, exp->E1);
        PrintToken(self, tok_lBracket);
        PrintExpression(self, exp->E2);
        PrintToken(self, tok_rBracket);
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

static inline void PrintSemaType(metac_printer_t* self,
                                 metac_semantic_state_t* sema,
                                 metac_type_index_t typeIndex)
{
    switch(TYPE_INDEX_KIND(typeIndex))
    {
        case type_index_basic:
        {
            decl_type_t basicType = {(metac_declaration_kind_t)0};
            basicType.DeclKind = decl_type;
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
        case type_index_ptr:
        {

        } break;
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

        PrintSemaExpression(self, sema, semaExp->E1);

        if (!IsBinaryExp(semaExp->E1->Kind))
            PrintChar(self, ')');
    }
    else if (semaExp->Kind == exp_tuple)
    {
        PrintChar(self, '{');
        metac_sema_expression_t* tupleElement =
            semaExp->TupleExpressions;
        for(uint32_t i = 0;
            i < semaExp->TupleExpressionCount;
            i++)
        {
            PrintSemaExpression(self, sema, tupleElement + i);
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
    else if (semaExp->Kind == exp_identifier)
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
    else if (IsBinaryExp(semaExp->Kind))
    {
        PrintChar(self, '(');
        PrintSemaExpression(self, sema, semaExp->E1);

        PrintSpace(self);
        const char* op = BinExpTypeToChars((metac_binary_expression_kind_t)semaExp->Kind);
        PrintString(self, op, strlen(op));
        PrintSpace(self);

        PrintSemaExpression(self, sema, semaExp->E2);
        PrintChar(self, ')');
    }
    else if (semaExp->Kind == exp_cast)
    {
        PrintString(self, "cast", 4);
        PrintChar(self, '(');
        PrintSemaType(self, sema,semaExp->CastType);
        PrintChar(self, ')');

        PrintSemaExpression(self, sema, semaExp->CastExp);
    }
    else if (semaExp->Kind == exp_call)
    {
        PrintSemaExpression(self, sema, semaExp->E1);
        PrintChar(self, '(');
        sema_exp_argument_list_t* argList =
            semaExp->E2->ArgumentList;
        const metac_sema_expression_t* onePastLastArg =
            argList->Arguments + argList->ArgumentCount;
        for(metac_sema_expression_t* arg = argList->Arguments;
            arg < onePastLastArg;
            arg++)
        {
            PrintSemaExpression(self, sema, arg);
            if (arg != (onePastLastArg - 1))
                PrintString(self, ", ", 2);
        }
        PrintChar(self, ')');
    }
    else if (semaExp->Kind == exp_index)
    {
        PrintSemaExpression(self, sema, semaExp->E1);
        PrintToken(self, tok_lBracket);
        PrintSemaExpression(self, sema, semaExp->E2);
        PrintToken(self, tok_rBracket);
    }
    else if (semaExp->Kind == exp_sizeof)
    {
        PrintKeyword(self, tok_kw_sizeof);
        PrintToken(self, tok_lParen);
        PrintSemaExpression(self, sema, semaExp->E1);
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

        PrintSemaExpression(self, sema, semaExp->E1);

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

        PrintSemaExpression(self, sema, semaExp->E1);

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

        PrintSemaExpression(self, sema, semaExp->E1);

        if (!IsBinaryExp(semaExp->E1->Kind))
            PrintChar(self, ')');
    }
    else
    {
        printf("don't know how to print %s\n", (MetaCExpressionKind_toChars(semaExp->Kind)));
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

        PrintSemaType(self, sema, funcType->ReturnType);
        PrintSpace(self);
        PrintChar(self, '(');
        PrintChar(self, '*');
        PrintIdentifier(self, variable->VarIdentifier);
        PrintChar(self, ')');
        PrintSpace(self);
        PrintChar(self, '(');
        PrintChar(self, '{');
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
        PrintChar(self, '}');
        PrintChar(self, ')');
    }
    else
    {
        PrintSemaType(self, sema, variable->TypeIndex);
        PrintSpace(self);
        PrintIdentifier(self, variable->VarIdentifier);
    }

    if (variable->VarInitExpression != emptyPointer)
    {
        PrintSpace(self);
        PrintToken(self, tok_assign);
        PrintSpace(self);
        PrintSemaExpression(self, sema, variable->VarInitExpression);
    }
}


static inline void PrintSemaDeclaration(metac_printer_t* self,
                                        metac_semantic_state_t* sema,
                                        metac_sema_declaration_t* semaDecl,
                                        uint32_t level)
{
    bool printSemicolon = true;

    switch (semaDecl->DeclKind)
    {
        case decl_type_enum:
        {
            PrintIdentifier(self, semaDecl->sema_decl_type_enum.Name);
        } break;
        case decl_type_typedef:
        case decl_type:
            assert(0);

        case decl_type_union :
        case decl_type_struct :
        {
            metac_type_aggregate_t* struct_ = (metac_type_aggregate_t*) semaDecl;
            PrintKeyword(self, AggToken(semaDecl->DeclKind));
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
                sema_decl_variable_t synVar;
                synVar.TypeIndex = (f + memberIndex)->Type;
                synVar.VarIdentifier = (f + memberIndex)->Identifier;
                PrintSemaVariable(self, sema, &synVar);
                //PrintChar(self, ';');
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
            //decl_field_t* field = (decl_field_t*) decl;
            //PrintVariable(self, field->Field);
            assert(0);
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
                PrintStatement(self, (metac_statement_t*)function_->FunctionBody);
                printSemicolon = false;
            }
        } break;
    }
    if (!!printSemicolon) PrintChar(self, ';');
    PrintNewline(self);
}

const char* MetaCPrinter_PrintSemaNode(metac_printer_t* self,
                                       metac_semantic_state_t* sema,
                                       metac_node_t node)
{
    const char* result = self->StringMemory + self->StringMemorySize;
    uint32_t posBegin = self->StringMemorySize;

    if (node->Kind > node_exp_invalid && node->Kind < node_exp_max)
    {
        PrintSemaExpression(self, sema, (metac_sema_expression_t*) node);
    }
    else if (node->Kind > stmt_min && node->Kind < stmt_max)
    {
        // PrintSemaStatement(self, (metac_sema_statement_t*) node);
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

    MetaCPrinter_Reset(self);

    self->IdentifierTable = identifierTable;
    self->StringTable = stringTable;
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

const char* MetaCPrinter_PrintNode(metac_printer_t* self, metac_node_t node)
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
        PrintDeclaration(self, (metac_declaration_t*) node, 0);
    }
    else
        assert(0);

    int advancement = self->StringMemorySize - posBegin;
    self->StringMemory[self->StringMemorySize++] = '\0';
    return result;

}

