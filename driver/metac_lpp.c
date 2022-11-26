#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "../hash/crc32c.h"
#include "metac_lpp.h"

#ifndef ALIGN4
#  define ALIGN4(N) (((N) + 3) & ~3)
#  define HAD_NO_ALIGN4
#endif

#ifndef emptyNode
# define emptyNode ((metac_node_t) 0x1)
#endif

bool errored = false;

static inline uint32_t EstimateNumberOfTokens(uint32_t length)
{
    uint32_t aligned_length = (length + 15) & ~15;
    float token_estimate = ( aligned_length / 4.3f );
    return (((uint32_t) token_estimate) + 128) & ~127;
}

/// Note text is expected to end with a '\0' character
void LexFile(metac_lexer_t* lexer,
             const char* path,
             const char* text, int32_t length)
{
    if (length)
    {
        uint32_t estimated =  EstimateNumberOfTokens( length );
        if (estimated > 96 && estimated < 768)
            estimated = 1024;

        uint32_t fileHash = ~0;

        metac_lexer_state_t lexer_state =
            MetaCLexerStateFromBuffer(1, text, length);
        if (estimated > ARRAY_SIZE(lexer->inlineTokens))
        {
            lexer->Tokens = (metac_token_t*) Allocator_Calloc(lexer->Allocator, metac_token_t, estimated);
            lexer->TokenCapacity = estimated;
            lexer->LocationStorage.Locations =
                (metac_location_t*) Allocator_Calloc(lexer->Allocator, metac_location_t, estimated);
            lexer->LocationStorage.LocationCapacity = estimated;
        }
        else
        {
            lexer->Tokens = lexer->inlineTokens;
            lexer->TokenCapacity =
                ARRAY_SIZE(lexer->inlineTokens);
            lexer->LocationStorage.Locations =
                lexer->inlineLocations;
            lexer->LocationStorage.LocationCapacity =
                ARRAY_SIZE(lexer->inlineTokens);
        }

        while(length > 0)
        {
            uint32_t initialPosition = lexer_state.Position;

            metac_token_t token =
                *MetaCLexerLexNextToken(lexer, &lexer_state, text, length);
            uint32_t eaten_chars = lexer_state.Position - initialPosition;
            fileHash = crc32c_nozero(fileHash, text, eaten_chars);

            if (eaten_chars == 0)
            {
                printf("No chars consumed ... Token:%s\n", MetaCTokenEnum_toChars(token.TokenType));
                printf("String around the halt: : %.*s\n" ,20 , text - 10);
                errored = true;
                return ;
            }

            text += eaten_chars;
            length -= eaten_chars;
        }
    }
}


void ParseFile(metac_parser_t* parser,
               const char* path,
               DeclarationArray* result)
{
#if DRIVER_PRINT_DECLS
    metac_printer_t printer;

    MetaCPrinter_Init(&printer,
        &parser->IdentifierTable,
        &parser->StringTable
    );
#endif
    uint32_t declarationSize = 0;
    uint32_t declarationCapacity = 1024;

    metac_decl_t** declarations =
        (metac_decl_t**)
            Allocator_Calloc(&parser->Allocator, metac_decl_t*,
            declarationCapacity);

    while(parser->CurrentTokenIndex < parser->Lexer->TokenCount)
    {
        metac_decl_t* decl =
            MetaCParser_ParseDeclaration(parser, 0);
        assert(decl);
        if (METAC_NODE(decl) == emptyNode)
        {
            continue;
        }
        declarations[declarationSize++] = decl;
        metac_token_t* afterDecl = MetaCParser_PeekToken(parser, 1);
        if (afterDecl && afterDecl->TokenType == tok_semicolon)
            MetaCParser_Match(parser, tok_semicolon);

        // printf("Parsed %u tokens\n", parser->CurrentTokenIndex);
        metac_token_t* lastToken = MetaCParser_PeekToken(parser, 1);
#if DRIVER_PRINT_DECLS
        const char* str =
            MetaCPrinter_PrintDeclaration(&printer,
                declarations[declarationSize - 1]
        );
        printf("%s\n", str);
        MetaCPrinter_Reset(&printer);
#endif
        if (!lastToken || lastToken->TokenType == tok_eof)
            break;
    }


    if (result != 0)
    {
        if (result->Capacity < declarationSize)
        {
            result->Capacity = ALIGN4(declarationSize);
            result->Ptr = (metac_decl_t**)calloc(result->Capacity, sizeof(metac_decl_t*));
        }
        result->Length = declarationSize;
        memcpy(result->Ptr, declarations, declarationSize * sizeof(metac_decl_t*));
        // free(declarations);
    }
}

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

void MetaCLPP_Init(metac_lpp_t* lpp, metac_alloc_t* allocator, metac_file_storage_t* fileStorage)
{
    if(!allocator)
    {
        assert(!"Allocator must not me null");
    }

    MetaCLexer_Init(&lpp->Lexer, allocator);
    MetaCParser_InitFromLexer(&lpp->Parser, &lpp->Lexer, allocator);

#ifndef NO_PREPROCESSOR
    MetaCPreProcessor_Init(&lpp->Preprocessor, &lpp->Lexer, allocator, fileStorage, 0);
    lpp->Parser.Preprocessor = &lpp->Preprocessor;
#endif
}

metac_expr_t* MetaCLPP_ParseExpressionFromString(metac_lpp_t* lpp, const char* exp)
{
    // assert(g_lineLexer.TokenCapacity == ARRAY_SIZE(g_lineLexer.inlineTokens));
    LexString(&lpp->Lexer, exp);

    metac_expr_t* result = MetaCParser_ParseExpression(&lpp->Parser, expr_flags_none, 0);

    return result;
}

metac_stmt_t* MetaCLPP_ParseStatementFromString(metac_lpp_t* lpp, const char* stmt)
{
    // assert(g_lineLexer.TokenCapacity == ARRAY_SIZE(g_lineLexer.inlineTokens));
    LexString(&lpp->Lexer, stmt);

    metac_stmt_t* result = MetaCParser_ParseStatement(&lpp->Parser, 0, 0);

    return result;
}

metac_decl_t* MetaCLPP_ParseDeclarationFromString(metac_lpp_t* lpp, const char* decl)
{
    LexString(&lpp->Lexer, decl);

    metac_decl_t* result = MetaCParser_ParseDeclaration(&lpp->Parser, 0);

    return result;
}

#ifndef NO_PREPROCESSOR
metac_preprocessor_directive_t MetaCLPP_ParsePreprocFromString(metac_lpp_t* lpp, const char* line,
                                                               metac_token_buffer_t* tokenBuffer)
{
    LexString(&lpp->Lexer, line);

    metac_preprocessor_directive_t dirc =
        MetaCParser_ParsePreprocDirective(&lpp->Parser, &lpp->Preprocessor);
    return dirc;
}
#endif

#include "../utils/read_file.c"

DeclarationArray ReadLexParse(const char* filename, metac_lpp_t* lpp, metac_alloc_t* parent)
{
    DeclarationArray result = {0};

    read_result_t readResult =
        ReadFileAndZeroTerminate(filename);

    LexFile(&lpp->Lexer, filename,
        readResult.FileContent0, readResult.FileLength
    );

    metac_alloc_t alloc;
    Allocator_Init(&alloc, parent);

    MetaCParser_InitFromLexer(&lpp->Parser, &lpp->Lexer, &alloc);

#ifndef NO_PREPROCESSOR
    MetaCPreProcessor_Init(&lpp->Preprocessor, &lpp->Lexer, &alloc, lpp->Preprocessor.FileStorage, 0);
    lpp->Parser.Preprocessor = &lpp->Preprocessor;
#endif

    ParseFile(&lpp->Parser, filename, &result);

    return result;
}

#ifdef HAD_NO_ALIGN4
#  undef ALIGN4
#endif
