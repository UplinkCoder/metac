#include "../compat.h"
#define IDENTIFIER_TABLE
#include "../metac_lexer.h"
#include "../metac_parser.h"
#include "../3rd_party/linenoise/linenoise.c"
#include "../int_to_str.c"
#include <stdio.h>

metac_statement_t* MetaCParserParseStatementFromString(const char* str);
const char* MetaCTokenEnum_toChars(metac_token_enum_t tok);

typedef enum parse_mode_t
{
    parse_mode_token = 0,
    
    parse_mode_decl,
    parse_mode_stmt,
    parse_mode_expr,
    
    parse_mode_max
} parse_mode_t;
void dummyStatement() {}

int main(int argc, const char* argv[])
{
    const char* line;

    parse_mode_t parseMode = parse_mode_token;
    metac_lexer_state_t repl_state = {0, 0, 0, 0};
    metac_lexer_t lexer;
    MetaCLexerInit(&lexer);

    const char* promt_ = "Token>";
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
                return 0;
            case 't' :
                parseMode = parse_mode_token;
                continue;
            case 'd' :
                parseMode = parse_mode_decl;
                promt_ = "Decl>";
                continue;
            case 'e' :
                parseMode = parse_mode_expr;
                promt_ = "Exp>";
                continue;
            case 's' :
                parseMode = parse_mode_stmt;
                promt_ = "Stmt>";
                continue;
            default :
                printf("Command :%c unknown type :h for help\n", *(line + 1));
                continue;
            case 'h' :
                printf("Press :e for expression mode\n"
                       "      :d for declaration mode\n"
                       "      :s for statement mode\n"
                       "      :t for token mode\n"
                       "      :p for preprocessor mode\n"
                       "      :q to quit\n");
                continue;
            }
        }

        while (line_length > 0)
        {
            metac_token_t token;
            
            metac_expression_t* exp;
            metac_statement_t* stmt;
            metac_declaration_t decl;
            
            metac_parser_t lineParser;

            uint32_t initalPosition = repl_state.Position;
            switch(parseMode)
            
            {
            case parse_mode_expr:
                 exp =
                    MetaCParserParseExpressionFromString(line);

                const char* str = PrintExpression(&g_lineParser, exp);
                printf("expr = %s\n", str);
                goto LnextLine;
                
            case parse_mode_stmt :
                   stmt = MetaCParserParseStatementFromString(line);
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
#ifdef IDENTIFIER_TABLE
                    printf("    %.*s\n", LENGTH_FROM_IDENTIFIER_KEY(token.Key), IDENTIFIER_PTR(&lexer.IdentifierTable, token));
#endif
#ifdef IDENTIFIER_TREE
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
                    printf("    \"%.*s\"\n", LENGTH_FROM_STRING_KEY(token.Key), token.String);
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
