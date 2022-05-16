#include "metac_cgen.h"
#include "metac_parser.h"
#include "libinterpret/backend_interface_funcs.h"

extern const BackendInterface BCGen_interface;

//FIXME we never hit this ... why?
// maybe an issue with the declaration store or maybe with
// aligning the tables ....

int GenerateFunctionCode(metac_codegen_function_t* cgfunction, void* c)
{
    decl_function_t* func = cgfunction->decl_func;

    for(decl_parameter_t* p = func->Parameters;
        p != emptyPointer;
        p = p->Next)
    {
        const char* name = IdentifierPtrToCharPtr(&g_lineParser.IdentifierTable,
                                                  p->Identifier);
        BCGen_interface.genParameter(c, BCType_i32, name);
    }
    int result = BCGen_interface.beginFunction(c, 0, "func");

    for(metac_statement_t* stmt = func->FunctionBody->Body;
        stmt != emptyPointer;
        stmt = stmt->Next)
    {
        switch(stmt->StmtKind)
        {
            case stmt_return:
            {
                stmt_return_t* return_ = cast(stmt_return_t*) stmt;
                metac_expression_t* exp = return_->Expression;
                BCValue retval = BCGen_interface.genTemporary(c, BCType_i32);
                // TODO we don't want to use WalkTree on this
                // as we want to generate functionbodies indepentnt of vstore and dstore

                BCGen_interface.Ret(c, &retval);
            } break;
        }
    }

    BCGen_interface.endFunction(c, result);
/*
    BCGen* gen = cast(BCGen*) c;
    printf("We should follow with some code:\n");
    for(int i = 0;
        i < gen->ip;
        i++)
    {
        printf("%d, ", gen->byteCodeArray[i]);
        if ((i % 8) == 0)
        {
            printf("\n");
        }
    }
    printf("\n");
    printf("There should have been some code\n");
*/
    return result;
}

const BCValue* GenerateExpression(metac_codegen_function_t* cgfunction,
                                  void* c, metac_expression_t* expr)
{

}

