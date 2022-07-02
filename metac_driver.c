#include "metac_driver.h"
#include "metac_lexer.h"
#include "metac_parsetree.h"
#include "metac_parser.h"
#include "metac_printer.h"
#include "crc32c.h"
#include <stdlib.h>

bool errored = false;
#ifndef ALIGN4
#  define ALIGN4(N) (((N) + 3) & ~3)
#endif
static inline uint32_t EstimateNumberOfTokens(uint32_t length)
{
    uint32_t aligned_length = (length + 16) & ~15;
    float token_estimate = ( aligned_length / 4.3f );
    return (((uint32_t) token_estimate) + 128) & ~127;
}

/// Note text is expected to end with a '\0' character
void LexFile(metac_lexer_t* lexer,
             const char* path,
             const char* text, uint32_t length)
{
    if (length)
    {
        uint32_t estimated =  EstimateNumberOfTokens( length );
        if (estimated > 96 && estimated < 768)
            estimated = 1024;

        uint32_t fileHash = ~0;

        metac_lexer_state_t lexer_state =
            MetaCLexerStateFromBuffer(1, text, length);
        if (estimated > 96)
        {
            lexer->Tokens = (metac_token_t*) malloc(estimated * sizeof(metac_token_t));
            lexer->TokenCapacity = estimated;
            lexer->LocationStorage.Locations =
                (metac_location_t*) malloc(estimated * sizeof(metac_location_t));
            lexer->LocationStorage.LocationCapacity = estimated;
        }
        else
        {
            lexer->Tokens = lexer->inlineTokens;
            lexer->TokenCapacity =
                sizeof(lexer->inlineTokens) / sizeof(lexer->Tokens[0]);
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

    metac_declaration_t** declarations =
        (metac_declaration_t**)
            calloc(sizeof(metac_declaration_t*),
            declarationCapacity);

    while(parser->CurrentTokenIndex < parser->Lexer->TokenSize)
    {
        declarations[declarationSize++] = MetaCParser_ParseDeclaration(parser, 0);
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
            result->Ptr = (metac_declaration_t**)calloc(result->Capacity, sizeof(metac_declaration_t*));
        }
        result->Length = declarationSize;
        memcpy(result->Ptr, declarations, declarationSize * sizeof(metac_declaration_t*));
        free(declarations);
    }
}

#ifdef DRIVER_MAIN
#endif
#undef ALIGN4
