#define ACCEL ACCEL_TABLE
#define NO_FIBERS
#define PRINT_CODE
#include "compat.h"

#include "metac_driver.c"
#include "utils/read_file.c"
#include "metac_parser_obj.c"
#include "metac_semantic_obj.c"
#include "metac_lpp.c"
#include "repl/exp_eval.c"
#include "metac_codegen.c"

DeclarationArray ReadLexParse(const char* filename, metac_lpp_t* lpp)
{
    DeclarationArray result = {0};

    read_result_t readResult =
        ReadFileAndZeroTerminate(filename);

    LexFile(&lpp->Lexer, filename,
        readResult.FileContent0, readResult.FileLength
    );

    MetaCParser_InitFromLexer(&lpp->Parser, &lpp->Lexer);

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

int main(int argc, const char* argv[])
{
    if (argc != 4)
    {
        fprintf(stderr, "Usage %s filename funcname numeric_arg\n", argv[0]);
        return -1;
    }

    const char* filename = argv[1];
    const char* funcname = argv[2];
    uint32_t arg = atoi(argv[3]);

    metac_lpp_t LPP;
    // lpp stands for lexer preprocessor parser
    // it simply bundles them as they are mostly used together
    MetaCLPP_Init(&LPP);
    // TODO MetaCLPP_Init should take an allocator

    DeclarationArray decls = ReadLexParse(filename, &LPP);

    decl_function_t* funcDecl = cast(decl_function_t*)
        FindDeclaration(decls, &LPP.Parser, funcname);

    if(!funcDecl || funcDecl->Kind != decl_function)
    {
        printf("%s is not a function declaration\n", funcname);
        return -1;
    }
/*
    printf("func: %s\n",
        MetaCPrinter_PrintNode(&LPP.Parser.DebugPrinter, METAC_NODE(funcDecl), 0));
*/
    metac_semantic_state_t Sema;
    MetaCSemantic_Init(&Sema, &LPP.Parser, 0);
    metac_scope_t* scope =
        MetaCSemantic_PushNewScope(&Sema, scope_owner_module, (metac_node_t)1);

    sema_decl_function_t* semaFunc =
        MetaCSemantic_doFunctionSemantic(&Sema, funcDecl);
/*
    printf("semafunc: %s\n",
            MetaCPrinter_PrintSemaNode(&LPP.Parser.DebugPrinter, &Sema, METAC_NODE(semaFunc)));
*/
    metac_alloc_t interpAlloc;
    Allocator_Init(&interpAlloc, 0, 0);

    metac_bytecode_ctx_t ctx;
    MetaCCodegen_Init(&ctx, 0);
    MetaCCodegen_Begin(&ctx, &LPP.Parser.IdentifierTable, &Sema);

    metac_bytecode_function_t fCode =
        MetaCCodegen_GenerateFunction(&ctx, semaFunc);

    MetaCCodegen_End(&ctx);

    uint32_t result =
        MetaCCodegen_RunFunction(&ctx, fCode, &interpAlloc, "u", arg);

    printf("%s(%u) = %u\n", funcname, arg, result);

    return 0;
}
