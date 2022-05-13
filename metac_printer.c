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
        uint32_t newCapa = self->StringMemoryCapacity * 1.3;
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
            if (stmt_return->Expression != _emptyPointer)
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
                nextStmt != _emptyPointer;
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
        default : {
            fprintf(stderr,
                "Statement Kind: not handled by printer\n");
                //MetaCStatementKind_toChars(stmt->StmtKind));
            assert(0);
        }
    }
}

static inline void PrintDeclaration(metac_printer_t* self,
                                    metac_declaration_t* decl,
                                    uint32_t level)
{
    bool printSemicolon = true;

    switch (decl->DeclKind)
    {
        case decl_typedef:
        {
            decl_typedef_t* typdef = (decl_typedef_t*) decl;
            printf("typedef ");
            level++;
            PrintDeclaration(self, (metac_declaration_t*)typdef->Type, level);
            PrintIdentifier(self, typdef->Identifier);
            level--;
        } break;
        case decl_type:
        {
            PrintType(self, (decl_type_t*) decl);
        }  break;
        case decl_type_struct :
        {
            decl_type_struct_t* struct_ = (decl_type_struct_t*) decl;
            PrintKeyword(self, tok_kw_struct);
            if (struct_->Identifier.v != empty_identifier.v)
            {
                PrintSpace(self);
                PrintIdentifier(self, struct_->Identifier);
            }
            PrintSpace(self);
            PrintToken(self, tok_lBrace);
            PrintNewline(self);
            ++level;
            ++self->IndentLevel;
            decl_field_t* f = struct_->Fields;
            for(uint32_t memberIndex = 0;
                memberIndex < struct_->FieldCount;
                memberIndex++)
            {
                PrintDeclaration(self, (metac_declaration_t*)f, level);
                PrintNewline(self);
                f = f->Next;
            }
            --self->IndentLevel;
            --level;
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
            PrintType(self, field->Field.VarType);
            PrintSpace(self);
            PrintIdentifier(self, field->Field.VarIdentifier);

        } break;
        case decl_variable:
        {
            decl_variable_t* variable = (decl_variable_t*) decl;
            PrintType(self, variable->VarType);
            PrintSpace(self);
            PrintIdentifier(self, variable->VarIdentifier);
            if (variable->VarInitExpression != _emptyPointer)
            {
                PrintSpace(self);
                PrintToken(self, tok_assign);
                PrintSpace(self);
                PrintExpression(self, variable->VarInitExpression);
            }
        } break;
        case decl_function:
        {
            decl_function_t* function_ = (decl_function_t*) decl;
            PrintType(self, function_->ReturnType);
            PrintSpace(self);
            PrintIdentifier(self, function_->Identifier);
            PrintToken(self, tok_lParen);

            for(decl_parameter_t* param = function_->Parameters;
                param != emptyPointer;
                param = param->Next)
            {
                PrintType(self, param->Type);
                if (param->Identifier.v != empty_identifier.v)
                {
                    PrintSpace(self);
                    PrintIdentifier(self, param->Identifier);
                }
                if (param->Next != emptyPointer)
                    PrintString(self, ", ", 2);
            }

            PrintToken(self, tok_rParen);

            if (function_->FunctionBody != _emptyPointer)
            {
                PrintStatement(self, (metac_statement_t*)function_->FunctionBody);
                printSemicolon = false;
            }
        } break;
    }
    if (!printSemicolon) PrintChar(self, ';');
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
            arg != _emptyPointer;
            arg = arg->Next)
        {
            PrintExpression(self, arg->Expression);
            if (arg->Next != _emptyPointer)
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
        PrintType(self, exp->SizeofType);
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
          || exp->Kind == exp_typeof || exp->Kind == exp_assert)
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
    self->StringMemoryCapacity = INITIAL_SIZE;
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
