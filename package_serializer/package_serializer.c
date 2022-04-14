#define _GNU_SOURCE 1

#include "../compat.h"

#define IDENTIFIER_TABLE
#include "../metac_lexer.h"
#include "../metac_parser.c"

#include "../utils/read_file.c"
#include "../cache/crc32.c"
#include <string.h>

int main(int argc, const char* argv[])
{
    metac_lexer_state_t repl_state = {0, 0, 0, 0};
    for(int arg_idx = 1;
        arg_idx < argc;
        arg_idx++)
    {
        const char* arg = argv[arg_idx];
        metac_lexer_t lexer;
        metac_lexer_state_t lexerState;
        InitMetaCLexer(&lexer);
        read_result_t readResult = ReadFileAndZeroTerminate(arg);
//        metac_source_t sourceId =
        LexFile(&lexer, arg, basename(arg),
            readResult.FileContent0, readResult.FileLength
        );
        metac_parser_t parser;
        MetaCParserInitFromLexer(&parser, &lexer);
    }

}
