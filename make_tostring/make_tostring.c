#include <stdio.h>

#define NO_FIBERS
#define NO_SEMANTIC

#include "../os/compat.h"
#include "../driver/metac_driver.c"
#include "../utils/read_file.c"
#include "../parser/metac_parser_obj.c"

#ifdef DEBUG_SERVER
#  include "../debug/debug_server.c"
#endif

int main(int argc, const char* argv[])
{
    metac_lpp_t lpp;
    decl_array_t decls;
    metac_alloc_t alloc;
    int i;
    ARENA_ARRAY(metac_decl_t*, selectedDecls);

    if (argc < 2)
    {
        fprintf(stderr, "Usage: file symbolnames ...\n");
        return -1;
    }

    Allocator_Init(&alloc, 0);
    /* lpp stands for lexer, preprocessor, parser */
    MetaCLPP_Init(&lpp, &alloc, 0);

    decls = ReadLexParse(argv[1], &lpp, 0);

    for(i = 1; i < argc + 1; i++)
    {
        metac_decl_t* decl = FindDecl(decls, &lpp.Parser, argv[i]);
        if (decl && decl != cast(metac_decl_t*)emptyPointer)
        {
            ARENA_ARRAY_ADD(selectedDecls, decl);
        }
    }

    {
        uint32_t i;
        metac_decl_t* decl = 0;

        STACK_ARENA_ARRAY(metac_stmt_t*, printFunctionStmts, 16, &alloc)

        for(i = 0; i < selectedDeclsCount; i++)
        {
            decl = selectedDecls[i];

            MetaCPrinter_PrintForHeader(&lpp.Parser.DebugPrinter, decl);
            printf("%.*s\n", (int) lpp.Parser.DebugPrinter.StringMemorySize, lpp.Parser.DebugPrinter.StringMemory);
            memset(lpp.Parser.DebugPrinter.StringMemory, ' ', lpp.Parser.DebugPrinter.StringMemorySize);
            lpp.Parser.DebugPrinter.StringMemorySize = 0;
        }

    //    printf("%.*s\n", (int) lpp.Parser.DebugPrinter.StringMemorySize, lpp.Parser.DebugPrinter.StringMemory);
    }

}
