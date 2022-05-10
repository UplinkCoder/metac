#ifdef ACCEL
#include "../compat.h"
#include "../metac_lexer.h"
#include "../metac_parser.h"
#include "../metac_printer.h"
#include "../cache/crc32.c"
//#include "../metac_eeP.c"
#include "../3rd_party/linenoise/linenoise.c"
#include "../int_to_str.c"
#include <stdio.h>
#include "exp_eval.c"

extern bool g_exernalIdentifierTable;

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

    parse_mode_ee,
    parse_mode_setvars,

    parse_mode_max
} parse_mode_t;

void PrintHelp(void)
{
    printf("Type :e for expression mode\n"
       "      :ee for evaluation mode\n"
       "      :d for declaration mode\n"
       "      :v for varible mode (set vars for eval)\n"
       "      :s for statement mode\n"
       "      :t for token mode\n"
       "      :l Load and lex file\n"
       "      :p for preprocessor mode\n"
       "      :q to quit\n");
}
int main(int argc, const char* argv[])
{
    const char* line;

    char* srcBuffer = 0;
    void* freePtr = 0;
    uint32_t srcBufferLength = 0;

    parse_mode_t parseMode = parse_mode_token;
    metac_lexer_state_t repl_state;
    repl_state.Position = 0;
    repl_state.Line = 1;
    repl_state.Column = 1;
    metac_lexer_t lexer;
    MetaCLexerInit(&lexer);

    PrintHelp();
    linenoiseHistoryLoad(".repl_history");
    const char* promt_;

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
    case parse_mode_setvars:
        promt_ = "SetVars>";
        break;
    }

LnextLine:
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
                    printf("cwd: %s\n", get_current_dir_name());
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
                parseMode = parse_mode_decl;
                goto LswitchMode;
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
                }
            case 's' :
                parseMode = parse_mode_stmt;
                goto LswitchMode;
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

                metac_expression_t result = evalWithVariables(exp, &vstore, &dstore);

                const char* str = MetaCPrinter_PrintExpression(&printer, exp);
                const char* result_str = MetaCPrinter_PrintExpression(&printer, &result);

                printf("%s = %s\n", str, result_str);
                MetaCPrinter_Reset(&printer);
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
                        fprintf(stderr, "declation could not be handled only functions are supported for now\n");
                        goto LnextLine;
                    }

                    const char* idChars = IdentifierPtrToCharPtr(&g_lineParser.IdentifierTable, idPtr);
                    const uint32_t length = strlen(idChars);
                    uint32_t idHash = crc32c(~0, idChars, length);
                    uint32_t idKey = IDENTIFIER_KEY(idHash, length);
                    metac_identifier_ptr_t dstoreId
                        = GetOrAddIdentifier(&dstore.Table, idKey, idChars, length);

                    if (decl->DeclKind == decl_function)
                    {
                        decl->decl_function.Identifier = dstoreId;
                        printf("Setting dStore ID: %u\n", dstoreId.v);
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
                        }
                        else
                        {
                            assert(assignExp->E1->Kind == exp_identifier);
                            assert(assignExp->E2->Kind == exp_signed_integer);

                            VariableStore_SetValueI32(&vstore, assignExp->E1, (int32_t)assignExp->E2->ValueI64);
                        }
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
