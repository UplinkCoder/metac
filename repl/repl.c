#include "../compat.h"

#include "../metac_lexer.h"
#include "../metac_parser.h"
#include "../3rd_party/linenoise/linenoise.c"
#include "../int_to_str.c"
#include <stdio.h>

const char* MetaCTokenEnum_toChars(metac_token_enum_t tok);
const char* PrintExpression(metac_expression_t*);

int main(int argc, const char* argv[])
{
    const char* line;

    metac_lexer_state_t repl_state = {0, 0, 0, 0};
    metac_lexer_t lexer;
    InitMetaCLexer(&lexer);
    metac_parser_t parser;
    MetaCParserInitFromLexer(&parser, &lexer);

    bool parsingExpression = false;
    bool parsingStatement  = false;

    const char* promt_ = "REPL>";
LinputLoop:
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
            case 'e' :
                    parsingExpression = true;
                    promt_ = "Exp>";
                    goto LinputLoop;
            case 's' :
                    parsingStatement = true;
                    promt_ = "Stmt>";
                    goto LinputLoop;
            }
        }
        while (line_length > 0)
        {
            metac_parser_t lineParser;

            uint32_t initalPosition = repl_state.Position;
            if (parsingExpression)
            {
                metac_expression_t* exp =
                    MetaCParserParseExpressionFromString(line);

                const char* str = PrintExpression(exp);
                printf("result = %s\n", str);
                parsingExpression = false;
                promt_ = "REPL>";
                goto LinputLoop;
            }
 #if 1
            else if (parsingStatement)
            {
                metac_statement_t* stmt
                    = MetaCParserParseStatementFromString(line);

                parsingStatement = false;
                promt_ = "REPL>";
                goto LinputLoop;
            }
#endif
            metac_token_t token =
                *MetaCLexerLexNextToken(&lexer, &repl_state, line, line_length);

            uint32_t eaten_chars = repl_state.Position - initalPosition;
            const uint32_t token_length = MetaCTokenLength(token);
#if 1
            printf("read tokenType: %s {length: %d}\n",
                    MetaCTokenEnum_toChars(token.TokenType), token_length);

            if (token.TokenType == tok_identifier)
            {
                printf("    %.*s\n", LENGTH_FROM_IDENTIFIER_KEY(token.Key), token.Identifier);
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
