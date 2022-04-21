#ifdef ACCEL
#include "../compat.h"
#include "../metac_lexer.h"
#include "../metac_parser.h"
//#include "../metac_eeP.c"
#include "../3rd_party/linenoise/linenoise.c"
#include "../int_to_str.c"
#include <stdio.h>

metac_statement_t* MetaCParserParseStatementFromString(const char* str);
metac_declaration_t* MetaCParserParseDeclarationFromString(const char* str);
void PrintDeclaration(metac_parser_t* self, metac_declaration_t* decl,
					  uint32_t indent, uint32_t level);

const char* MetaCTokenEnum_toChars(metac_token_enum_t tok);

typedef enum parse_mode_t
{
    parse_mode_token = 0,

    parse_mode_decl,
    parse_mode_stmt,
    parse_mode_expr,
    parse_mode_file,

    parse_mode_ee,

    parse_mode_max
} parse_mode_t;

void PrintHelp(void)
{
    printf("Press :e for expression mode\n"
       "      :d for declaration mode\n"
       "      :s for statement mode\n"
       "      :t for token mode\n"
       "      :l Load and lex file\n"
       "      :p for preprocessor mode\n"
       "      :q to quit\n");
}
int main(int argc, const char* argv[])
{
    const char* line;

    const char* srcBuffer = 0;
    const void* freePtr = 0;
    uint32_t srcBufferLength = 0;

    parse_mode_t parseMode = parse_mode_token;
    metac_lexer_state_t repl_state;
    repl_state.Line = 1;
    repl_state.Column = 1;
    metac_lexer_t lexer;
    MetaCLexerInit(&lexer);

    PrintHelp();
    linenoiseHistoryLoad(".repl_history");
    const char* promt_;

LswitchMode:
    switch (parseMode)
    {
    case parse_mode_max: assert(0);
    case parse_mode_token:
        promt_ = "Token>";
        break;
    case parse_mode_decl:
        promt_ = "Decl>";
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

                    uint32_t estimatedTokenCount = (((sz / 5) + 128) & 127);
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
                    fread(srcBuffer, 1, sz, fd);
                    parseMode = parse_mode_file;

                    repl_state.Line = 1;
                }

                goto LlexSrcBuffer;
            }
            case 't' :
                parseMode = parse_mode_token;
                goto LswitchMode;
            case 'd' :
                parseMode = parse_mode_decl;
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
            srcBuffer = line;
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
                 exp =
                    MetaCParserParseExpressionFromString(line);

                const char* str = PrintExpression(&g_lineParser, exp);
                printf("expr = %s\n", str);
                goto LnextLine;

            case parse_mode_stmt :
                   stmt = MetaCParserParseStatementFromString(line);
                goto LnextLine;
            case parse_mode_decl :
                    decl = MetaCParserParseDeclarationFromString(line);
                    if (decl)
                        PrintDeclaration(&g_lineParser, decl, 0, 0);
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
                else if (token.TokenType == tok_charLiteral)
                {
                    printf("    '%.*s'\n", token.CharLiteralLength, token.chars);
                }
                else if (token.TokenType == tok_unsignedNumber)
                {
                    char buffer[21];
                    printf("    %s\n", u64tostr(token.ValueU64, buffer));
                }
                else if (token.TokenType == tok_stringLiteral)
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
