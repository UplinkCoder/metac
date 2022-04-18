#ifdef ACCEL
#include "../compat.h"
#include "../metac_lexer.h"
#include "../metac_parser.h"
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

    parse_mode_max
} parse_mode_t;

void PrintHelp(void)
{
    printf("Press :e for expression mode\n"
       "      :d for declaration mode\n"
       "      :s for statement mode\n"
       "      :t for token mode\n"
       "      :p for preprocessor mode\n"
       "      :q to quit\n");
}
int main(int argc, const char* argv[])
{
    const char* line;

    parse_mode_t parseMode = parse_mode_token;
    metac_lexer_state_t repl_state = {0, 0, 0, 0};
    metac_lexer_t lexer;
    MetaCLexerInit(&lexer);

    PrintHelp();
    linenoiseHistoryLoad(".repl_history");
    const char* promt_;
LswitchMode:
    switch (parseMode)
    {
    case parse_mode_max: break;
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
    }
    
LnextLine:
    while ((line = linenoise(promt_)))
    {
        linenoiseHistoryAdd(line);
        uint32_t line_length = strlen(line);
        if (*line == ':')
        {
            switch(*(line + 1))
            {
            case 'q':
                linenoiseHistorySave(".repl_history");
                return 0;
            case 't' :
                parseMode = parse_mode_token;
                goto LswitchMode;
            case 'd' :
                parseMode = parse_mode_decl;
                goto LswitchMode;
            case 'e' :
                parseMode = parse_mode_expr;
                goto LswitchMode;
            case 's' :
                parseMode = parse_mode_stmt;
                goto LswitchMode;
            case 'i' :
#ifdef ACCEL
                printf("Accelerator: %s\n", ACCELERATOR);
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

        while (line_length > 0)
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

            case parse_mode_token :
                   token = *MetaCLexerLexNextToken(&lexer, &repl_state, line, line_length);

                uint32_t eaten_chars = repl_state.Position - initalPosition;
                const uint32_t token_length = MetaCTokenLength(token);
#if 1
                printf("read tokenType: %s {length: %d}\n",
                        MetaCTokenEnum_toChars(token.TokenType), token_length);

                if (token.TokenType == tok_identifier)
                {
#if ACCEL == ACCEL_TABLE
                    printf("    %.*s\n", LENGTH_FROM_IDENTIFIER_KEY(token.Key), IDENTIFIER_PTR(&lexer.MEMBER_SUFFIX(Identifier), token));
#elif ACCEL == ACCEL_TREE
                    printf("    %.*s\n", LENGTH_FROM_IDENTIFIER_KEY(token.Key), IDENTIFIER_PTR(&lexer.IdentifierTree, token));
#endif
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

                line_length -= eaten_chars;
                line += eaten_chars;

                if (!token_length)
                    break;
            }
        }
    }
}
#endif
