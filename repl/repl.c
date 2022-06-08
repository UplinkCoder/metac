#ifdef ACCEL
#include "../compat.h"
#include "../metac_lexer.h"
#include "../metac_parsetree.h"
#include "../metac_parser.h"
#include "../metac_printer.h"
#include "../metac_semantic.h"
#include "../metac_driver.h"
#include "../metac_compiler_interface.h"
#include "../metac_type_table.h"
#include "../bsr.h"
#include "../crc32c.h"
//#include "../metac_eeP.c"
#include "../3rd_party/linenoise/linenoise.c"
#include "../int_to_str.c"
#include <stdio.h>
#include "exp_eval.c"
#include "../metac_type_table.h"

extern bool g_exernalIdentifierTable;
extern metac_lexer_t g_lineLexer;
extern void LineLexerInit();

metac_statement_t* MetaCParser_ParseStatementFromString(const char* str);
metac_declaration_t* MetaCParser_ParseDeclarationFromString(const char* str);

const char* MetaCTokenEnum_toChars(metac_token_enum_t tok);

typedef enum parse_mode_t
{
    parse_mode_token = 0,

    parse_mode_decl,
    parse_mode_stmt,
    parse_mode_expr,
    parse_mode_file,

    parse_mode_ds,
    parse_mode_ss,

    parse_mode_ee,
    parse_mode_es,
    parse_mode_setvars,

    parse_mode_max
} parse_mode_t;

void PrintHelp(void)
{
    printf("Type :e for expression mode\n"
       "      :ee for evaluation mode\n"
       "      :es for expression semantic mode\n"
       "      :d for declaration mode\n"
       "      :ds for declaration semantic mode\n"
       "      :v for varible mode (set vars for eval)\n"
       "      :s for statement mode\n"
       "      :ss for statement semantic mode\n"
       "      :t for token mode\n"
       "      :l Load and lex file\n"
       "      :p for preprocessor mode\n"
       "      :q to quit\n");
}

#include "../utils/read_file.c"

typedef struct identifier_translation_context_t
{
    const uint32_t FunctionKey;
    const metac_identifier_table_t* SrcTable;
    metac_identifier_table_t* DstTable;
} identifier_translation_context_t;

static inline void TranslateIdentifier(metac_identifier_table_t* dstTable,
                         const metac_identifier_table_t* srcTable,
                         metac_identifier_ptr_t* idPtrP)
{
    assert(idPtrP->v);
    const char* idChars =
        IdentifierPtrToCharPtr((metac_identifier_table_t*)srcTable, *idPtrP);
    const uint32_t idLen = strlen(idChars);
    const uint32_t idHash = crc32c_nozero(~0, idChars, idLen);
    const uint32_t idKey = IDENTIFIER_KEY(idHash, idLen);

    metac_identifier_ptr_t newPtr =
        GetOrAddIdentifier(dstTable, idKey, idChars);
    idPtrP->v = newPtr.v;
}

static inline int TranslateIdentifiers(metac_node_t node, void* ctx)
{
    identifier_translation_context_t* context =
        (identifier_translation_context_t*) ctx;
    assert(crc32c_nozero(~0, __FUNCTION__, strlen(__FUNCTION__) == context->FunctionKey));

    const metac_identifier_table_t* SrcTable = context->SrcTable;
    metac_identifier_table_t* DstTable = context->DstTable;


    switch(node->Kind)
    {
        case decl_variable:
        {
            decl_variable_t* variable = (decl_variable_t*) node;
            TranslateIdentifier(DstTable, SrcTable, &variable->VarIdentifier);
        } break;
        case decl_type:
        {
            decl_type_t* type = (decl_type_t*) node;
            if (type->TypeIdentifier.v && type->TypeIdentifier.v != empty_identifier.v)
                TranslateIdentifier(DstTable, SrcTable, &type->TypeIdentifier);
        } break;
        case decl_type_struct:
        {
            decl_type_struct_t* struct_ = (decl_type_struct_t*) node;
            if (struct_->BaseIdentifier.v != empty_identifier.v)
                TranslateIdentifier(DstTable, SrcTable, &struct_->BaseIdentifier);
            if (struct_->Identifier.v != empty_identifier.v)
                TranslateIdentifier(DstTable, SrcTable, &struct_->Identifier);
        }
        default : break;
    }

    return 0;
}

typedef struct presemantic_context_t
{
    const uint32_t FunctionKey;
    metac_semantic_state_t* Sema;
} presemantic_context_t;

static inline int Presemantic(metac_node_t node, void* ctx)
{
    presemantic_context_t* context =
        (presemantic_context_t*) ctx;
    assert(crc32c_nozero(~0, __FUNCTION__, strlen(__FUNCTION__) == context->FunctionKey));

    if (node->Kind == node_decl_type_typedef)
    {
        decl_type_typedef_t* typedef_ = (decl_type_typedef_t*) node;
        metac_identifier_ptr_t typedefId = typedef_->Identifier;
        printf("found a typedef: %s\n", IdentifierPtrToCharPtr(context->Sema->ParserIdentifierTable, typedefId));

        metac_type_index_t typeIndex =
            MetaCSemantic_doTypeSemantic(context->Sema, node);

        return 1;
    }
    else
    {
        //printf("Not a typedef: %s\n", MetaCNodeKind_toChars(node->Kind));
    }

    return 0;
}

int main(int argc, const char* argv[])
{
    const char* line;

    char* srcBuffer = 0;
    void* freePtr = 0;
    uint32_t srcBufferLength = 0;

    parse_mode_t parseMode = parse_mode_ee;
    metac_lexer_state_t repl_state;
    repl_state.Position = 0;
    repl_state.Line = 1;
    repl_state.Column = 1;

    metac_lexer_t lexer;
    MetaCLexer_Init(&lexer);

    g_lineLexer.Tokens =
        (metac_token_t*)malloc(128 * sizeof(metac_token_t));
    g_lineLexer.TokenCapacity = 128;
    g_lineLexer.LocationStorage.Locations =
        (metac_location_t*)malloc(128 * sizeof(metac_location_t));
    g_lineLexer.LocationStorage.LocationCapacity = 128;

    metac_type_aggregate_t* compilerStruct = 0;

    read_result_t fCompilterInterface =
        ReadFileAndZeroTerminate("metac_compiler_interface.h");
    if (!fCompilterInterface.FileContent0)
        fCompilterInterface =
        ReadFileAndZeroTerminate("../metac_compiler_interface.h");

    metac_semantic_state_t sema;
    // make sure we know our special identifiers
    LineLexerInit();

    if (fCompilterInterface.FileContent0)
    {
        metac_lexer_t tmpLexer;
        metac_parser_t tmpParser;

        DeclarationArray decls = {0};
        {
            MetaCLexer_Init(&tmpLexer);

            LexFile(&tmpLexer, "metac_compiler_interface.h",
                    fCompilterInterface.FileContent0,
                    fCompilterInterface.FileLength);

            MetaCParser_InitFromLexer(&tmpParser, &tmpLexer);
            ParseFile(&tmpParser, "metac_compiler_interface.h", &decls);
        }
        MetaCLexer_Free(&tmpLexer);

        metac_semantic_state_t tmpSema;
        MetaCSemantic_Init(&tmpSema, &tmpParser, 0);
        MetaCSemantic_PushNewScope(&tmpSema, scope_parent_module, 0);

        presemantic_context_t presemanticContext = {
            crc32c_nozero(~0, "Presemantic", sizeof("Presemantic") - 1),
            &tmpSema,
        };

        for(int i = 0;
            i < decls.Length;
            i++)
        {
            MetaCDeclaration_Walk(decls.Ptr[i], Presemantic, &presemanticContext);
        }

        for(int i = 0;
            i < decls.Length;
            i++)
        {
            metac_identifier_ptr_t printIdentifier = {0};

            metac_declaration_t* decl = decls.Ptr[i];

            if (decl->DeclKind == decl_type_typedef)
            {
                decl_type_typedef_t* typedef_ = (decl_type_typedef_t*) decl;
                if (typedef_->Type->DeclKind == decl_type_struct)
                {
                    decl_type_struct_t* structPtr = (decl_type_struct_t*)typedef_->Type;
                    if (structPtr->Identifier.v == empty_identifier.v)
                    {
                        printIdentifier = typedef_->Identifier;
                    }
                    decl = (metac_declaration_t*)typedef_->Type;
                }
            }

            if (decl->DeclKind == decl_type_struct)
            {
                decl_type_struct_t* struct_ = (decl_type_struct_t*) decl;
                if (struct_->Identifier.v != empty_identifier.v)
                {
                    printIdentifier = struct_->Identifier;
                }
                printf("found struct : '%s'\n",
                    IdentifierPtrToCharPtr(&tmpParser.IdentifierTable, printIdentifier));

                compilerStruct = MetaCSemantic_doDeclSemantic(&tmpSema, struct_);
            }
        }

        MetaCSemantic_Init(&sema, &g_lineParser, compilerStruct);
        MetaCSemantic_PushNewScope(&sema, scope_parent_module, 1);


        MetaCSemantic_Handoff(&tmpSema,
                              (metac_sema_declaration_t**)&sema.CompilerInterface,
                              &sema);
        // FreeSema
        MetaCParser_Free(&tmpParser);
    }
    else
    {
        MetaCSemantic_Init(&sema, &g_lineParser, 0);
        MetaCSemantic_PushNewScope(&sema, scope_parent_module, 1);
    }

    metac_printer_t printer;
    MetaCPrinter_Init(&printer,
        &g_lineParser.IdentifierTable,
        &g_lineParser.StringTable);
/*
    printf("Transfered decl: %s\n",
        MetaCPrinter_PrintDeclaration(&printer, compilerStruct));
*/

    // only here can we destroy tmpSema

    PrintHelp();
    linenoiseHistoryLoad(".repl_history");
    const char* promt_;



    metac_dot_printer_t dot_printer;
    MetaCDotPrinter_Init(&dot_printer, &g_lineParser.IdentifierTable);
    g_lineParser.DotPrinter = &dot_printer;

    variable_store_t vstore;
    VariableStore_Init(&vstore);

    _ReadContextCapacity = 32;
    _ReadContexts = (ReadI32_Ctx*)
        malloc(sizeof(ReadI32_Ctx) * _ReadContextCapacity);
    _ReadContextSize = 0;

LswitchMode:
    switch (parseMode)
    {
    case parse_mode_max: assert(0);
    case parse_mode_file:
        promt_ = ">File<";
        break;
    case parse_mode_token:
        promt_ = "Token>";
        break;
    case parse_mode_decl:
        promt_ = "Decl>";
        linenoiseSetMultiLine(1);
        break;
    case parse_mode_expr:
        promt_ = "Exp>";
        break;
    case parse_mode_stmt:
        promt_ = "Stmt>";
        break;
    case parse_mode_ee:
        promt_ = "EE>";
        break;
    case parse_mode_es:
        promt_ = "ES>";
        break;
    case parse_mode_ss:
        promt_ = "SS>";
        break;
    case parse_mode_ds:
        promt_ = "DS>";
        break;
    case parse_mode_setvars:
        promt_ = "SetVars>";
        break;
    }

    // while(0) makes sure we can only call free
    // in the case we jumped to the LnextLine label
    while(0)
    {
LnextLine:
        linenoiseFree((void*)line);
    }
    while ((line = linenoise(promt_)))
    {
        linenoiseHistoryAdd(line);
        uint32_t line_length = strlen(line);
        if (*line == ':')
        {
            switch(line[1])
            {
            case 'q':
                linenoiseHistorySave(".repl_history");
                return 0;
            case 'l' :
            {
                const char* filename = line + 3;
                printf("loading and lexing: '%s'\n", filename);
                FILE* fd = fopen(filename, "rb");
                if (!fd)
                {
                    perror("loading file failed");
                    // printf("cwd: %s\n", get_current_dir_name());
                }
                else
                {
                    fseek(fd, 0, SEEK_END);
                    uint32_t sz = ftell(fd);
                    fseek(fd, 0, SEEK_SET);

                    uint32_t estimatedTokenCount = (((sz / 5) + 128) & ~127);
                    if (lexer.TokenCapacity < estimatedTokenCount)
                    {
                        lexer.Tokens = (metac_token_t*)
                            malloc(sizeof(metac_token_t) * estimatedTokenCount);
                        lexer.TokenSize = 0;
                        lexer.TokenCapacity = estimatedTokenCount;
                        lexer.Tokens = (metac_token_t*)
                            malloc(sizeof(metac_token_t) * estimatedTokenCount);

                        lexer.LocationStorage.Locations = (metac_location_t*)
                            malloc(sizeof(metac_location_t) * estimatedTokenCount);
                        lexer.LocationStorage.LocationSize = 0;
                        lexer.LocationStorage.LocationCapacity = estimatedTokenCount;
                    }

                    freePtr = srcBuffer = calloc(1, sz + 4);
                    srcBufferLength = sz;
                    fread((void*)srcBuffer, 1, sz, fd);
                    parseMode = parse_mode_file;
                    repl_state.Position = 0;
                    repl_state.Line = 1;
                    repl_state.Column = 1;
                    repl_state.Size = sz;
                }
                break;
            }
            case 't' :
                parseMode = parse_mode_token;
                goto LswitchMode;
            case 'd' :
                switch (line[2])
                {
                default:
                    parseMode = parse_mode_decl;
                    goto LswitchMode;

                 case 's':
                    parseMode = parse_mode_ds;
                    goto LswitchMode;
                }

            case 'v' :
                parseMode = parse_mode_setvars;
                goto LswitchMode;
            case 'e' :
                switch(line[2])
                {
                default:
                    parseMode = parse_mode_expr;
                    goto LswitchMode;
                case 'e':
                    parseMode = parse_mode_ee;
                    goto LswitchMode;
                case 's':
                    parseMode = parse_mode_es;
                    goto LswitchMode;
                }
            case 's' :
                switch (line[2])
                {
                default:
                    parseMode = parse_mode_stmt;
                    goto LswitchMode;
                case 's':
                    parseMode = parse_mode_ss;
                    goto LswitchMode;
                }
            case 'i' :
#ifdef ACCEL
                printf("Accelerator: %s\n", ACCELERATOR);
#else
                printf("MetaC compiled without Accelerator\n");
#endif
                continue;
            default :
                printf("Command :%c unknown type :h for help\n", *(line + 1));
                continue;
            case 'h' :
                PrintHelp();
                continue;
            }
        }
        else if (line[0] == '.')
        {
            if (line_length == sizeof("scope")
             && 0 == memcmp(line + 1, "scope", strlen("scope")))
            {
                if (sema.CurrentScope)
                {
                    metac_scope_table_t* table = &sema.CurrentScope->ScopeTable;
                    uint32_t nMembers = table->SlotsUsed;

                    for(uint32_t slotIdx = 0, memberIdx = 0; memberIdx < nMembers; slotIdx++)
                    {
                        metac_scope_table_slot_t slot = table->Slots[slotIdx];
                        if (slot.Hash)
                        {
                            printf("Member [%u] : %s\n", memberIdx++, MetaCPrinter_PrintSemaNode(&printer, &sema, slot.Node));
                        }
                    }
                }
                goto LnextLine;
            }
        }

        if (parseMode != parse_mode_file)
        {
            srcBuffer = (char*)line;
            srcBufferLength = line_length;
        }

        while (srcBufferLength > 0)
        {
            metac_token_t token;

            metac_expression_t* exp;
            metac_statement_t* stmt;
            metac_declaration_t* decl;

            uint32_t initalPosition = repl_state.Position;
            switch(parseMode)
            {
            case parse_mode_max: break;
            case parse_mode_expr:
            {
                 exp =
                    MetaCParser_ParseExpressionFromString(line);

                const char* str = MetaCPrinter_PrintExpression(&printer, exp);
                printf("expr = %s\n", str);
                MetaCPrinter_Reset(&printer);
                goto LnextLine;
            }
            case parse_mode_ee:
            {
                exp =
                    MetaCParser_ParseExpressionFromString(line);

                metac_sema_expression_t* result =
                    MetaCSemantic_doExprSemantic(&sema, exp);

                metac_expression_t printExpStorage;

                metac_sema_expression_t eval_exp = evalWithVariables(result, &vstore);
                result = &eval_exp;

                const char* str = MetaCPrinter_PrintExpression(&printer, exp);
                const char* result_str;
                if (eval_exp.Kind == exp_type)
                {
                    result_str = MetaCPrinter_PrintSemaNode(&printer, &sema, &eval_exp);
                }
                else
                {
                    result_str = MetaCPrinter_PrintSemaNode(&printer, &sema, &eval_exp);
                }
                printf("%s = %s\n", str, result_str);
                MetaCPrinter_Reset(&printer);
                // XXX static and fixed size state like _ReadContext
                // should go away soon.
                _ReadContextSize = 0;
                goto LnextLine;
            }
            case parse_mode_es:
            {
                exp =
                    MetaCParser_ParseExpressionFromString(line);

                const char* str = MetaCPrinter_PrintExpression(&printer, exp);
                metac_sema_expression_t* result =
                    MetaCSemantic_doExprSemantic(&sema, exp);

                const char* type_str = TypeToChars(&sema, result->TypeIndex);

                printf("typeof(%s) = %s\n", str, type_str);
                // XXX static and fixed size state like _ReadContext
                // should do away soon.
                _ReadContextSize = 0;
                goto LnextLine;
            }
            case parse_mode_setvars :
            {
                metac_declaration_t* decl = MetaCParser_ParseDeclarationFromString(line);
                if (decl)
                {
                    metac_identifier_ptr_t idPtr = {0};
                    idPtr = IdentifierPtrFromDecl(decl);

                    if (idPtr.v == 0)
                    {
                        fprintf(stderr, "declaration could not be handled only functions are supported for now\n");
                        goto LnextLine;
                    }

                    const char* idChars = IdentifierPtrToCharPtr(&g_lineParser.IdentifierTable, idPtr);
                    const uint32_t length = strlen(idChars);
                    uint32_t idHash = crc32c_nozero(~0, idChars, length);
                    uint32_t idKey = IDENTIFIER_KEY(idHash, length);
/*
                    metac_identifier_ptr_t dstoreId
                        = GetOrAddIdentifier(&dstore.Table, idKey, idChars);

                    if (decl->DeclKind == decl_function)
                    {
                        decl->decl_function.Identifier = dstoreId;
                        printf("Setting dStore ID: %u\n", dstoreId.v);
                    }
                    else if (decl->DeclKind == decl_variable)
                    {
                        decl->decl_variable.VarIdentifier = dstoreId;

                        //VariableStore_SetValueI32(&vstore, assignExp->E1, (int32_t)assignExp->E2->ValueI64);
                    }

                    DeclarationStore_SetDecl(&dstore, dstoreId, decl);
*/
                    printf("Registering %s [v=%u] in scope\n", idChars, idPtr.v);
                    MetaCSemantic_RegisterInScope(&sema, idPtr, decl);
                    goto LnextLine;
                }
                else
                {
                    metac_expression_t* assignExp = MetaCParser_ParseExpressionFromString(line);
                    if (assignExp)
                    {
                        if (assignExp->Kind != exp_assign)
                        {
                            fprintf(stderr, "You must write an expression of the from identifier = value");
                            goto LnextLine;
                        }

                        metac_sema_expression_t* ae =
                            MetaCSemantic_doExprSemantic(&sema, assignExp);
                        if (ae)
                        {
                            if (ae->E1->Kind == exp_identifier)
                            {
                                assert(0);
                            }
                            assert(ae->E2->Kind == exp_signed_integer);

                            VariableStore_SetValueI32(&vstore, ae->E1, (int32_t)ae->E2->ValueI64);
                        }
                        else
                        {
                            fprintf(stderr, "Semantic on assign exp failed\n");
                        }
                        // MetaCSemantic_Free(&sema);
                        goto LnextLine;
                    }
                    else
                    {
                        fprintf(stderr, "Input did not parse as either an assign-expression or declaration\n");
                    }
                }
            } break;

            case parse_mode_stmt :
            {
                stmt = MetaCParser_ParseStatementFromString(line);
                if (stmt)
                    printf("stmt = %s\n", MetaCPrinter_PrintStatement(&printer, stmt));
                else
                   fprintf(stderr, "couldn't parse statement\n");
                MetaCPrinter_Reset(&printer);
                goto LnextLine;
            }
            case parse_mode_decl :
            {
                decl = MetaCParser_ParseDeclarationFromString(line);
                if (decl)
                    printf("decl = %s\n", MetaCPrinter_PrintDeclaration(&printer, decl));
                else
                    printf("Couldn't parse Declaration\n");
                linenoiseSetMultiLine(false);

                goto LnextLine;
            }

            case parse_mode_ds :
            {
                decl = MetaCParser_ParseDeclarationFromString(line);
                if (decl)
                    printf("decl = %s\n", MetaCPrinter_PrintDeclaration(&printer, decl));
                else
                    printf("Couldn't parse Declaration\n");

                metac_sema_declaration_t* ds =
                    MetaCSemantic_doDeclSemantic(&sema, decl);

                goto LnextLine;
            }

            case parse_mode_file :
                goto LlexSrcBuffer;
            case parse_mode_token :

LlexSrcBuffer:
                   token = *MetaCLexerLexNextToken(&lexer, &repl_state, srcBuffer, srcBufferLength);

                uint32_t eaten_chars = repl_state.Position - initalPosition;
                const uint32_t token_length = MetaCTokenLength(token);
#if 1
                const uint32_t locPtr = token.LocationId;
                const metac_location_t loc = lexer.LocationStorage.Locations[locPtr - 4];

                printf("read tokenType: %s {length: %d}\n Location: {Line: %d, Col: %d}",
                        MetaCTokenEnum_toChars(token.TokenType), token_length,
                        loc.StartLine, loc.StartColumn
                );

                if (token.TokenType == tok_identifier)
                {
#if ACCEL == ACCEL_TABLE
                    printf("    %.*s\n", LENGTH_FROM_IDENTIFIER_KEY(token.Key), IDENTIFIER_PTR(&lexer.MEMBER_SUFFIX(Identifier), token));
#elif ACCEL == ACCEL_TREE
                    printf("    %.*s\n", LENGTH_FROM_IDENTIFIER_KEY(token.Key), IDENTIFIER_PTR(&lexer.IdentifierTree, token));
#endif
                }
                else if (token.TokenType == tok_char)
                {
                    printf("    '%.*s'\n", token.charLength, token.chars);
                }
                else if (token.TokenType == tok_char_uni)
                {
                    printf("    '\\U%.*s'\n", token.charLength, token.chars);
                }
                else if (token.TokenType == tok_uint)
                {
                    char buffer[21];
                    printf("    %s\n", u64tostr(token.ValueU64, buffer));
                }
                else if (token.TokenType == tok_string)
                {
                    printf("    \"%.*s\"\n", LENGTH_FROM_STRING_KEY(token.Key), STRING_PTR(&lexer.String, token.StringPtr));
                }
#endif

                srcBufferLength -= eaten_chars;
                srcBuffer += eaten_chars;
                printf("eaten_chars: %d\n", eaten_chars);
                if (!token_length)
                    break;
            }
        }

        {
            repl_state.Line++;
            repl_state.Column = 1;
            lexer.TokenSize = 0;
            lexer.LocationStorage.LocationSize = 0;
        }

        if (freePtr)
        {
            free(freePtr);
            freePtr = 0;
            parseMode = parse_mode_token;
            goto LswitchMode;
        }
        srcBuffer = 0;
    }
}
#endif
