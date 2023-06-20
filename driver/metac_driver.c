#include "../driver/metac_driver.h"
#include "../printer/metac_printer.h"
#include "../hash/crc32c.h"

#include "../driver/metac_lpp.c"

metac_decl_t* FindDecl(decl_array_t decls,
                       metac_parser_t* parser,
                       const char* name)
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
