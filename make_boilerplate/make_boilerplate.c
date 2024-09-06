#define NO_FIBERS
#define ACCEL ACCEL_TABLE

#include "../os/compat.h"
#include "../utils/read_file.c"
#include "../hash/crc32c.h"
#include "../parser/metac_parser_obj.c"
#include "../semantic/metac_semantic_obj.c"
#include "../codegen/metac_codegen.c"
#include "../driver/metac_driver.c"

#include <stdlib.h>
#include <stdio.h>

typedef struct make_boiler_ctx_t
{
    metac_identifier_table_t* IdentifierTable;

    char* StatementVisitor;
    uint32_t StatementVisitorSize;
    uint32_t StatementVisitorCapacity;

    char* DeclarationVisitor;
    uint32_t DeclarationVisitorSize;
    uint32_t DeclarationVisitorCapacity;

} make_boiler_ctx_t;

bool IsNode(decl_type_t* t, metac_identifier_table_t* table)
{
    const char* typeName = 0;

    if (t->Kind == decl_type_ptr)
    {
        decl_type_ptr_t* ptrType = (decl_type_ptr_t*) t;
        if (ptrType->ElementType->Kind == decl_type &&
            ptrType->ElementType->TypeIdentifier.v != 0)
        {
            typeName = IdentifierPtrToCharPtr(table, ptrType->ElementType->TypeIdentifier);
        }
        else if (ptrType->ElementType->Kind == decl_type_struct)
        {
            decl_type_struct_t* type_struct = (decl_type_struct_t*)ptrType->ElementType;
            typeName = IdentifierPtrToCharPtr(table, type_struct->Identifier);
        }
    }

    if (typeName)
        return true;

    return false;
}

void WriteSwitchCaseEntryFor(decl_type_struct_t* decl_type_struct,
                             metac_identifier_table_t* table,
                             char** OutputStringP, uint32_t* OutputSizeP)
{
    /// hold typename without the _t
    char scratchpad[256];
    const char* indent8 = "        ";
    uint32_t outputSize = *OutputSizeP;
    char* outputString = *OutputStringP;
    const char* typeName =
        IdentifierPtrToCharPtr(table, decl_type_struct->Identifier);
    uint32_t typeNameLen = strlen(typeName);

    memcpy(scratchpad, typeName, typeNameLen -2);
    scratchpad[typeNameLen -2] = '\0';

    outputSize += sprintf(outputString + outputSize,
        "%scase %s:\n%s{\n", indent8, scratchpad, indent8);
    outputSize += sprintf(outputString + outputSize,
        "%s    %s_t* %s = cast(%s_t*) node;\n",
        indent8, scratchpad, scratchpad, scratchpad);

    {
        decl_field_t* fields = decl_type_struct->Fields;
        for(uint32_t fieldIdx = 0;
            fieldIdx < decl_type_struct->FieldCount;
            fieldIdx++)
        {
            decl_variable_t* fieldVar = fields->Field;
            const char* fieldName = IdentifierPtrToCharPtr(table, fieldVar->VarIdentifier);

            if (IsNode(fieldVar->VarType, table))
            {
                outputSize += sprintf(outputString + outputSize,
                                      "%s    if (METAC_NODE(%s->%s) != emptyNode)\n",
                                       indent8, scratchpad, fieldName);

                outputSize += sprintf(outputString + outputSize,
                                      "%s        result = walker_fn(%s->%s, ctx);\n",
                                      indent8, scratchpad, fieldName);
                outputSize += sprintf(outputString + outputSize,
                                      "%s    if(result)\n", indent8);
                outputSize += sprintf(outputString + outputSize,
                                      "%s        return result;\n", indent8);
            }

            fields = fields->Next;
        }
        outputSize += sprintf(outputString + outputSize,
                              "%s} break;\n\n", indent8);

    }

    (*OutputSizeP) = outputSize;

    return;
}

/// return 0 means keep recursing down
/// return != 0 means stop recursing
int MakeBoilerCb(metac_node_t node, void* ctxP)
{
    make_boiler_ctx_t* ctx = cast(make_boiler_ctx_t*) ctxP;

    switch(node->Kind)
    {
        case node_decl_type_struct:
        {
            decl_type_struct_t* decl_struct = cast (decl_type_struct_t*) node;
            const char* structName = 0;

            if (decl_struct->Identifier.v != -1)
            {
                structName =
                    IdentifierPtrToCharPtr(ctx->IdentifierTable, decl_struct->Identifier);
            }

            printf("Me found a struct '%s'\n",
                ((decl_struct->Identifier.v != -1) ?
                    structName : "anonymous")
            );

            if (decl_struct->Identifier.v == -1)
                return 1;

            if(!memcmp(structName, "stmt", 4))
            {
                WriteSwitchCaseEntryFor(decl_struct,
                    ctx->IdentifierTable,
                    &ctx->StatementVisitor, &ctx->StatementVisitorSize);
            }
            else if(!memcmp(structName, "decl", 4))
            {
                WriteSwitchCaseEntryFor(decl_struct,
                    ctx->IdentifierTable,
                    &ctx->DeclarationVisitor, &ctx->DeclarationVisitorSize);
            }
            // make sure we don't traverse down the types of the fields
            // as then we would see duplicates
            return 1;
        }
    }

    return 0;
}

int main(int argc, const char** argv)
{
    const char* arg;
    for(int arg_idx = 1;
        arg_idx < argc;
        arg_idx++)
    {
        metac_lpp_t LPP;
        metac_alloc_t alloc;

        decl_array_t result = {0};

        Allocator_Init(&alloc, 0);

        MetaCLPP_Init(&LPP, &alloc, 0);
        // TODO MetaCLPP_Init should take an allocator

	    // lpp stands for lexer preprocessor parser
        // it simply bundles them as they are mostly used together
        MetaCLPP_Init(&LPP, &alloc, 0);

        arg = argv[arg_idx];
        printf("filename: %s\n", arg);
        result = ReadLexParse(arg, &LPP, 0);

        printf("Found %u declarations\n", result.Length);
        metac_printer_t printer;
        MetaCPrinter_Init(&printer, &LPP.Parser.IdentifierTable,
                                    &LPP.Parser.StringTable,
                                    0);
        make_boiler_ctx_t makeBoilerCtx = {
            &LPP.Parser.IdentifierTable,

            (char*) malloc(16384*2),0, 16384*2,
            (char*) malloc(16384*2),0, 16384*2
        };

        for(uint32_t i = 0; i < result.Length; i++)
        {
            metac_decl_t* decl = result.Ptr[i];
            MetaCNode_TreeWalk_Real(decl, MakeBoilerCb, &makeBoilerCtx);
            // printf("decl: %s\n\n", MetaCPrinter_PrintDeclaration(&printer, *decl));
            MetaCPrinter_Reset(&printer);
        }

        printf("%.*s\n\n", (int) makeBoilerCtx.DeclarationVisitorSize,
                                 makeBoilerCtx.DeclarationVisitor);
        printf("%.*s\n\n", (int) makeBoilerCtx.StatementVisitorSize,
                                 makeBoilerCtx.StatementVisitor);
    }
}

