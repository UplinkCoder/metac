#define ACCEL ACCEL_TABLE
#define NO_FIBERS
#define PRINT_CODE

#include "../os/compat.h"
#include "../driver/metac_driver.c"
#include "../utils/read_file.c"
#include "../parser/metac_parser_obj.c"
#include "../semantic/metac_semantic_obj.c"
#include "../driver/metac_lpp.c"
#include "../codegen/metac_codegen.c"


#ifndef POSIX
#  include <unistd.h>
#endif

DeclarationArray ReadLexParse(const char* filename, metac_lpp_t* lpp)
{
    DeclarationArray result = {0};

    read_result_t readResult =
        ReadFileAndZeroTerminate(filename);

    LexFile(&lpp->Lexer, filename,
        readResult.FileContent0, readResult.FileLength
    );

    metac_alloc_t alloc;
    Allocator_Init(&alloc, 0);

    MetaCParser_InitFromLexer(&lpp->Parser, &lpp->Lexer, &alloc);

    ParseFile(&lpp->Parser, filename, &result);

    return result;
}

metac_declaration_t* FindDeclaration(DeclarationArray decls,
                                     metac_parser_t* parser, const char* name)
{
    const uint32_t len = cast(uint32_t) strlen(name);
    const uint32_t hash = crc32c_nozero(~0, name, len);
    const uint32_t key = IDENTIFIER_KEY(hash, len);

    metac_identifier_ptr_t NameId =
        IsIdentifierInTable(&parser->IdentifierTable, key, name);

    if (NameId.v == 0)
        return 0;

    for(uint32_t idx = 0;
        idx < decls.Length;
        idx++)
    {
        metac_declaration_t* decl = decls.Ptr[idx];
        // printf("decl: %s\n",
        //    MetaCPrinter_PrintDeclaration(&parser->DebugPrinter, decl));
        metac_identifier_ptr_t idPtr = {0};
        if (decl->Kind == decl_type_enum)
        {
            idPtr = (cast(decl_type_enum_t*) decl)->Identifier;
        }
        else if (decl->Kind == decl_function)
        {
            idPtr = (cast(decl_function_t*) decl)->Identifier;
        }
        else if (decl->Kind == decl_type_typedef)
        {
            decl_type_typedef_t* typdef = cast(decl_type_typedef_t*) decl;
            idPtr = typdef->Identifier;
            decl = cast(metac_declaration_t*) typdef->Type;
        }

        if (idPtr.v == NameId.v)
            return decl;
    }

    return 0;
}

metac_identifier_ptr_t IdentifierFromDecl(metac_declaration_t* decl)
{
    metac_identifier_ptr_t result = {0};

    if (decl->Kind == decl_type_enum)
    {
        result = (cast(decl_type_enum_t*) decl)->Identifier;
    }
    else if (decl->Kind == decl_function)
    {
        result = (cast(decl_function_t*) decl)->Identifier;
    }
    else if (decl->Kind == decl_type_typedef)
    {
        decl_type_typedef_t* typdef = cast(decl_type_typedef_t*) decl;
        result = typdef->Identifier;
    }

    return result;
}

DeclarationArray FilterDeclarations(DeclarationArray decls, metac_parser_t* parser,
                                    metac_alloc_t* alloc,
                                    bool (*filterFunc) (metac_declaration_t*, metac_parser_t*))
{
    ARENA_ARRAY(metac_declaration_t*, result)

    ARENA_ARRAY_INIT(metac_declaration_t*, result, alloc);

    for(uint32_t idx = 0;
        idx < decls.Length;
        idx++)
    {
        if (filterFunc(decls.Ptr[idx], parser))
        {
            ARENA_ARRAY_ADD(result, decls.Ptr[idx]);
        }
    }

    DeclarationArray retval;

    retval.Ptr = result;
    retval.Length = resultCount;

    return retval;
}

bool DeclarationIsParseFunc(metac_declaration_t* decl, metac_parser_t* parser)
{
    bool result = false;
    const char* name = 0;
    metac_identifier_ptr_t idPtr = {0};
    idPtr = IdentifierFromDecl(decl);

    if (!idPtr.v)
        goto Lret;

    name = IdentifierPtrToCharPtr(&parser->IdentifierTable, idPtr);
    if (!name)
        goto Lret;

    result = (0 == strncmp(name, "MetaCParser_Parse", (sizeof("MetaCParser_Parse") - 1)));
Lret:
    return result;
}

int main(int argc, const char* argv[])
{
    if (argc != 2)
    {
        fprintf(stderr, "Usage %s filename\n", argv[0]);
        return -1;
    }

    const char* filename = argv[1];
    char cwdBuffer[256];

    const char* cwd = getcwd(cwdBuffer, sizeof(cwdBuffer));

    metac_lpp_t LPP;
    // lpp stands for lexer preprocessor parser
    // it simply bundles them as they are mostly used together
    metac_alloc_t alloc;
    Allocator_Init(&alloc, 0);

    MetaCLPP_Init(&LPP, &alloc, 0);

    DeclarationArray decls = ReadLexParse(filename, &LPP);
    metac_alloc_t resultAlloc;
    Allocator_Init(&resultAlloc, 0, 0);

    DeclarationArray parseFunctions =
        FilterDeclarations(decls, &LPP.Parser,
                          &resultAlloc, DeclarationIsParseFunc);

    printf("Found %d parserFuncs\n", (int)parseFunctions.Length);
    for(uint32_t i = 0; i < parseFunctions.Length; i++)
    {
        metac_identifier_ptr_t id = IdentifierFromDecl(parseFunctions.Ptr[i]);
        const char* s = IdentifierPtrToCharPtr(&LPP.Parser.IdentifierTable, id);
        printf("  %s\n", s);
    }
    return 0;
}
