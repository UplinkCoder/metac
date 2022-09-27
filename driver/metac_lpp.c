#include "metac_lpp.h"

static inline void LexString(metac_lexer_t* lexer, const char* line)
{
    int32_t line_length = strlen(line);
    metac_lexer_state_t lexer_state =
        MetaCLexerStateFromString(0, line);

    while(line_length > 0)
    {
        int32_t initialPosition = lexer_state.Position;

        metac_token_t token =
            *MetaCLexerLexNextToken(lexer, &lexer_state, line, line_length);
        if (token.TokenType == tok_eof)
        {
            line += line_length;
            line_length = 0;
            break;
        }

        uint32_t eaten_chars = lexer_state.Position - initialPosition;
        line += eaten_chars;
        line_length -= eaten_chars;
    }
}

void MetaCLPP_Init(metac_lpp_t* lpp)
{
    MetaCLexer_Init(&lpp->Lexer);
#ifndef NO_PREPROCESSOR
    MetaCPreProcessor_Init(&lpp->Preprocessor, &lpp->Lexer, 0, 0);
#endif
    MetaCParser_InitFromLexer(&lpp->Parser, &lpp->Lexer);
}

metac_expression_t* MetaCLPP_ParseExpressionFromString(metac_lpp_t* lpp, const char* exp)
{
    // assert(g_lineLexer.TokenCapacity == ARRAY_SIZE(g_lineLexer.inlineTokens));
    LexString(&lpp->Lexer, exp);

    metac_expression_t* result = MetaCParser_ParseExpression(&lpp->Parser, expr_flags_none, 0);

    return result;
}

metac_statement_t* MetaCLPP_ParseStatementFromString(metac_lpp_t* lpp, const char* stmt)
{
    // assert(g_lineLexer.TokenCapacity == ARRAY_SIZE(g_lineLexer.inlineTokens));
    LexString(&lpp->Lexer, stmt);

    metac_statement_t* result = MetaCParser_ParseStatement(&lpp->Parser, 0, 0);

    return result;
}

metac_declaration_t* MetaCLPP_ParseDeclarationFromString(metac_lpp_t* lpp, const char* decl)
{
    LexString(&lpp->Lexer, decl);

    metac_declaration_t* result = MetaCParser_ParseDeclaration(&lpp->Parser, 0);

    return result;
}

#ifndef NO_PREPROCESSOR
metac_preprocessor_directive_t MetaCLPP_ParsePreprocFromString(metac_lpp_t* lpp, const char* line,
                                                               metac_token_buffer_t* tokenBuffer)
{
    LexString(&lpp->Lexer, line);

    metac_preprocessor_directive_t dirc = MetaCParser_ParsePreproc(&lpp->Parser, &lpp->Preprocessor, tokenBuffer);
    return dirc;
}
#endif
