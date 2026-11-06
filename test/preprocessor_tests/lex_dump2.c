#include "../../parser/metac_parser_obj.c"
#include <stdio.h>
static metac_lexer_t* globalLexer;
static metac_preprocessor_t* globalPreproc;
static void LexStringIntoLexer(metac_lexer_t* lexer, const char* str, int32_t length)
{
    metac_lexer_state_t lexer_state = MetaCLexerStateFromBuffer(1, str, length);
    while(length > 0)
    {
        uint32_t initialPosition = lexer_state.Position;
        (void) *MetaCLexerLexNextToken(lexer, &lexer_state, str, length);
        uint32_t eaten = lexer_state.Position - initialPosition;
        if (eaten == 0) break;
        str += eaten;
        length -= eaten;
    }
}
int main(int argc, char** argv)
{
    metac_alloc_t topAlloc; metac_lexer_t lexer;
    Allocator_Init(&topAlloc, 0);
    MetaCLexer_Init(&lexer, &topAlloc);
    globalLexer = &lexer;
    FILE* f = fopen(argv[1], "rb");
    fseek(f, 0, SEEK_END); long size = ftell(f); fseek(f, 0, SEEK_SET);
    char* text = calloc(1, size + 1);
    fread(text, 1, size, f); fclose(f);
    LexStringIntoLexer(&lexer, text, size);
    for(uint32_t i = 0; i < lexer.TokenCount; i++)
        printf("[%3u] %-28s pos=%4u\n", i, MetaCTokenEnum_toChars(lexer.Tokens[i].TokenType), lexer.Tokens[i].Position);
    return 0;
}
