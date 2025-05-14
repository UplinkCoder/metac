#include <stdio.h>

#define NO_FIBERS
#define NO_SEMANTIC
#define OLD_PARSER

#include "../os/compat.h"
#include "../driver/metac_driver.c"
#include "../utils/read_file.c"
#include "../parser/metac_parser_obj.c"
// #include "../semantic/metac_semantic_obj.c"
// #include "../codegen/metac_codegen.c"

#ifdef DEBUG_SERVER
#  include "../debug/debug_server.c"
#endif

int main(int argc, const char* argv[])
{
    metac_lpp_t lpp;
    decl_array_t decls;
    metac_alloc_t alloc;

    Allocator_Init(&alloc, 0);
    /* lpp stands for lexer, preprocessor, parser */
    MetaCLPP_Init(&lpp, &alloc, 0);

    decls = ReadLexParse(argv[1], &lpp, 0);

    printf("Got %d declarations\n", (int32_t)decls.Length);
    {
        uint32_t i;
        metac_decl_t* decl = 0;
        for(i = 0; i < decls.Length; i++)
        {
            decl = decls.Ptr[i];
            if (!decl)
            {
                continue;
            }

            MetaCPrinter_PrintForHeader(&lpp.Parser.DebugPrinter, decl);
            printf("%.*s\n", (int) lpp.Parser.DebugPrinter.StringMemoryCount, lpp.Parser.DebugPrinter.StringMemory);
            memset(lpp.Parser.DebugPrinter.StringMemory, ' ', lpp.Parser.DebugPrinter.StringMemoryCount);
            lpp.Parser.DebugPrinter.StringMemoryCount = 0;
        }

    //    printf("%.*s\n", (int) lpp.Parser.DebugPrinter.StringMemorySize, lpp.Parser.DebugPrinter.StringMemory);
    }

}
