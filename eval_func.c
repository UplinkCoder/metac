#define ACCEL ACCEL_TABLE
#define NO_FIBERS 1
#define PRINT_CODE
#include "os/compat.h"

#include "driver/metac_driver.c"
#include "utils/read_file.c"
#include "parser/metac_parser_obj.c"
#include "semantic/metac_semantic_obj.c"
#include "repl/exp_eval.c"
#include "codegen/metac_codegen.c"


metac_decl_t* FindDecl(decl_array_t decls,
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
        metac_decl_t* decl = decls.Ptr[idx];
        // printf("decl: %s\n",
        //    MetaCPrinter_PrintDecl(&parser->DebugPrinter, decl));
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
            decl = cast(metac_decl_t*) typdef->Type;
        }

        if (idPtr.v == NameId.v)
            return decl;
    }

    return 0;
}

int main(int argc, const char* argv[])
{

    const char* filename = argv[1];
    const char* funcname = argv[2];
    uint32_t arg = atoi(argv[3]);
    metac_alloc_t alloc;
    metac_lpp_t LPP;
    decl_array_t decls;

    if (argc != 4)
    {
        fprintf(stderr, "Usage %s filename funcname numeric_arg\n", argv[0]);
        return -1;
    }

    Allocator_Init(&alloc, 0);
    // lpp stands for lexer preprocessor parser
    // it simply bundles them as they are mostly used together
    MetaCLPP_Init(&LPP, &alloc, 0);
    // TODO MetaCLPP_Init should take an allocator
    {
        decls = ReadLexParse(filename, &LPP, 0);

        decl_function_t* funcDecl = cast(decl_function_t*)
            FindDecl(decls, &LPP.Parser, funcname);

        if(!funcDecl || funcDecl->Kind != decl_function)
        {
            printf("%s is not a function declaration\n", funcname);
            return -1;
        }
/*
    printf("func: %s\n",
        MetaCPrinter_PrintNode(&LPP.Parser.DebugPrinter, METAC_NODE(funcDecl), 0));
*/
    metac_sema_state_t Sema;
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

    }

    return 0;
}
