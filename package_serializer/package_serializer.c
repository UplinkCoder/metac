#include "../compat.h"
#include "../utils/read_file.c"
#include "../crc32c.h"
#include "../metac_parser_obj.c"
//#include "../3rd_party/tracyC.h"

#include <stdlib.h>
#include <stdio.h>

const char** includePaths = 0;
uint32_t includePathCount = 0;
uint32_t includePathCapacity = 0;

void AddIncludePath(const char* path)
{
    assert(includePathCount < includePathCapacity);

    *(includePaths + includePathCount++) = path;
}

int main(int argc, const char* argv[])
{
    includePathCount = 0;
    includePathCapacity = 256;
    includePaths = (const char**)malloc(sizeof(char**) * includePathCapacity);
    const char* arg = "bigcode.c";

    for(int arg_idx = 1;
        arg_idx < argc;
        arg_idx++)
    {
        arg = argv[arg_idx];
        if (arg[0] == '-')
        {
            if (arg[1] == 'I')
            {
                AddIncludePath(arg + 2);
            }
            else
            {
                fprintf(stderr, "Unkown option: %s", arg);
            }
            continue;
        }
        printf("arg: %s\n", arg);
        metac_lexer_t lexer;
        MetaCLexer_Init(&lexer);

        read_result_t readResult = ReadFileAndZeroTerminate(arg);

        LexFile(&lexer, arg,
            readResult.FileContent0, readResult.FileLength
        );

        metac_parser_t parser;
        MetaCParser_InitFromLexer(&parser, &lexer);

        ParseFile(&parser, arg, 0);

        metac_identifier_table_slot_t firstEntry = {0};

        metac_identifier_table_slot_t* firstEntryP = findFirstEntry(&lexer.IdentifierTable);
        if (firstEntryP)
            firstEntry = *firstEntryP;

        printf("First Entry = {Hash:%x Value:%u}\n", firstEntry.HashKey, firstEntry.Ptr.v);


#ifndef NO_DUMP
        char formatBuffer[512];
        sprintf(formatBuffer, "%s.tokens", arg);
        FILE* tokens_fd = fopen(formatBuffer, "wb");
        fwrite(lexer.Tokens, 1, lexer.TokenSize * sizeof(metac_token_t), tokens_fd);
        fclose(tokens_fd);

#if ACCEL == ACCEL_TABLE
        sprintf(formatBuffer, "%s.identifiers", arg);
        WriteTable(&lexer.IdentifierTable, formatBuffer, 20, 0);

        metac_identifier_table_t newIdTable = ReadTable(formatBuffer);
        sprintf(formatBuffer, "%s.identifiers.new", arg);
        WriteTable(&newIdTable, formatBuffer, 20, "new");

        printf("First entry is in read out table: %d\n",
               IsInTable(&newIdTable, firstEntry.HashKey,
                         firstEntry.Ptr)
        );


        sprintf(formatBuffer, "%s.strings", arg);
        WriteTable(&lexer.StringTable, formatBuffer, 12, 0);
#endif
#endif
    }
    return errored;
}
