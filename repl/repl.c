#ifdef ACCEL
#include "../compat.h"
#include "../metac_lexer.h"
#include "../metac_parser.h"
#include "../metac_printer.h"
#include "../metac_semantic.h"
#include "../metac_compiler_interface.h"
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
    MetaCLexerInit(&lexer);

    g_lineLexer.Tokens =
        (metac_token_t*)malloc(128 * sizeof(metac_token_t));
    g_lineLexer.TokenCapacity = 128;
    g_lineLexer.LocationStorage.Locations =
        (metac_location_t*)malloc(128 * sizeof(metac_location_t));
    g_lineLexer.LocationStorage.LocationCapacity = 128;
//    decl_type_struct_t* compiler_struct = 0;

    decl_type_struct_t* compilerStruct = 0;

    read_result_t fCompilterInterface =
        ReadFileAndZeroTerminate("metac_compiler_interface.h");
    if (!fCompilterInterface.FileContent0)
        fCompilterInterface =
        ReadFileAndZeroTerminate("../metac_compiler_interface.h");

    metac_semantic_state_t sema;

    if (fCompilterInterface.FileContent0)
    {
        compilerStruct = (decl_type_struct_t*)
            MetaCParser_ParseDeclarationFromString(
                fCompilterInterface.FileContent0);
    }
    MetaCSemantic_Init(&sema, &g_lineParser, compilerStruct);
    MetaCSemantic_PushScope(&sema, scope_parent_module, 0);


    PrintHelp();
    linenoiseHistoryLoad(".repl_history");
    const char* promt_;

    // make sure we know our special keywords
    LineLexerInit();

    metac_printer_t printer;
    MetaCPrinter_Init(&printer,
        &g_lineParser.IdentifierTable,
        &g_lineParser.StringTable);

    metac_dot_printer_t dot_printer;
    MetaCDotPrinter_Init(&dot_printer, &g_lineParser.IdentifierTable);
    g_lineParser.DotPrinter = &dot_printer;

    declaration_store_t dstore;
    DeclarationStore_Init(&dstore);

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

                metac_sema_expression_t eval_exp = evalWithVariables(result, &vstore, &dstore);
                printExpStorage.ValueU64 = eval_exp.ValueU64;
                printExpStorage.Kind = eval_exp.Kind;
                result = &eval_exp;

                const char* str = MetaCPrinter_PrintExpression(&printer, exp);
                const char* result_str = MetaCPrinter_PrintExpression(&printer, &printExpStorage);

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
                printf("type_str = %p\n", type_str);

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
                    uint32_t idHash = crc32c(~0, idChars, length);
                    uint32_t idKey = IDENTIFIER_KEY(idHash, length);
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
                    stmt = MetaCParser_ParseStatementFromString(line);
                    if (stmt)
                        printf("stmt = %s\n", MetaCPrinter_PrintStatement(&printer, stmt));
                    else
                       fprintf(stderr, "couldn't parse statement\n");
                    MetaCPrinter_Reset(&printer);

                goto LnextLine;
            case parse_mode_decl :
                    decl = MetaCParser_ParseDeclarationFromString(line);
                    if (decl)
                        printf("decl = %s\n", MetaCPrinter_PrintDeclaration(&printer, decl));
                    else
                        printf("Couldn't parse Declaration\n");
                    linenoiseSetMultiLine(false);
                goto LnextLine;

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
