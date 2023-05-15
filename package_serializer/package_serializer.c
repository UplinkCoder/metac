#include "../os/compat.h"
#include "../utils/read_file.c"
#include "../hash/crc32c.h"
#include "../parser/metac_parser_obj.c"
#include "../semantic/metac_semantic_obj.c"
#include "../codegen/metac_codegen.c"
#include "../driver/metac_driver.c"
#include "../3rd_party/tracy/TracyC.h"

#ifdef DEBUG_SERVER
#  include "../debug/debug_server.h"
#endif


#include <stdlib.h>
#include <stdio.h>

#if 0
#include <time.h>

// call this function to start a nanosecond-resolution timer
struct timespec timer_start() {
    struct timespec start_time;
    clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &start_time);
    return start_time;
}

// call this function to end a timer, returning nanoseconds elapsed as a long
long timer_end(struct timespec start_time){
    struct timespec end_time;
    clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &end_time);
    long diffInNanos = (end_time.tv_sec - start_time.tv_sec) * (long)1e9 + (end_time.tv_nsec - start_time.tv_nsec);
    return diffInNanos;
}
#endif

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
#ifdef DEBUG_SERVER
    debug_server_t thisDebugServer = {0};
    g_DebugServer = &thisDebugServer;
    Debug_Init(g_DebugServer, 8180);
#endif
    includePathCount = 0;
    includePathCapacity = 256;
    includePaths = (const char**)malloc(sizeof(char**) * includePathCapacity);
    const char* arg = "bigcode.c";

    uint32_t nWorkers = 3;

    //tasksystem_t theTaskystem;
    //worker_context_t* contexts =
    //    (worker_context_t*)calloc(sizeof(worker_context_t), nWorkers);

    // TaskSystem_Init(&theTaskystem, nWorkers, 0);

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
        metac_alloc_t lexerAlloc = {0};
        Allocator_Init(&lexerAlloc, 0);
        MetaCLexer_Init(&lexer, &lexerAlloc);

        read_result_t readResult = ReadFileAndZeroTerminate(arg);
#if 0
        struct timespec t = timer_start();
#endif
        LexFile(&lexer, arg,
            readResult.FileContent0, readResult.FileLength
        );
#if 0
        {
            long ns = timer_end(t);
            float us = ns / 1000.0f;
            printf("lexing %u bytes took %f us -- %f bytes/ns\n", readResult.FileLength, us, cast(float)readResult.FileLength / ns);
        }
#endif
        metac_parser_t parser;
        metac_alloc_t parserAlloc = {0};
        Allocator_Init(&parserAlloc, 0);
        MetaCParser_InitFromLexer(&parser, &lexer, &parserAlloc);
        ParseFile(&parser, arg, 0);
#if 0
        {
            long ns = timer_end(t);
            float us = ns / 1000.0f;
            printf("parsing and lexing %u bytes took %f us -- %f bytes/ns\n", readResult.FileLength, us, cast(float)readResult.FileLength / ns);
        }
#endif



        metac_identifier_table_slot_t firstEntry = {0};

#if !defined(NO_DUMP) && defined(WRITE_TABLE)
        metac_identifier_table_slot_t* firstEntryP = findFirstEntry(&lexer.IdentifierTable);
        if (firstEntryP)
            firstEntry = *firstEntryP;

        printf("First Entry = {Hash:%x Value:%u}\n", firstEntry.HashKey, firstEntry.Ptr.v);


        char formatBuffer[512];
        sprintf(formatBuffer, "%s.tokens", arg);
        FILE* tokens_fd = fopen(formatBuffer, "wb");
        fwrite(lexer.Tokens, 1, lexer.TokenCount * sizeof(metac_token_t), tokens_fd);
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

    getchar();
    return errored;
}
