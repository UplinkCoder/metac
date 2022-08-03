#include "metac_alloc.h"
#include "metac_codegen.h"
#include "libinterpret/bc_common.h"
#include "libinterpret/backend_interface_funcs.h"
#include "libinterpret/bc_interpreter_backend.h"

uint32_t MetaCCodegen_GetTypeABISize(metac_bytecode_ctx_t* ctx, metac_type_index_t type)
{
    return 0;
}

BCType MetaCCodegen_GetBCType(metac_bytecode_ctx_t* ctx, metac_type_index_t type)
{
    return (BCType){};
}

extern const BackendInterface BCGen_interface;
static const BackendInterface* bc;


metac_function_bytecode_t* MetaCCodegen_GenerateFunction(metac_bytecode_ctx_t* ctx,
                                                         sema_decl_function_t* function)
{
    bc = &BCGen_interface;
    uint32_t frameSize = 0;
    uint32_t functionParameterCount = 1;
    STACK_ARENA_ARRAY(BCValue, parameters, 16, 0);
    STACK_ARENA_ARRAY(BCType, parameterTypes, 16, 0);

    void* c;
    bc->new_instance(&c);
    ctx->c = c;
    bc->Initialize(c, 0);


    for(uint32_t i = 0;
        i < functionParameterCount;
        i++)
    {
        sema_decl_variable_t* param = function->Parameters + i;
        uint32_t paramSize = MetaCCodegen_GetTypeABISize(ctx, param->TypeIndex);

        const char* paramName = IdentifierPtrToCharPtr(ctx->IdentifierTable,
                                                       param->VarIdentifier);
        ARENA_ARRAY_ADD(parameterTypes, MetaCCodegen_GetBCType(ctx, param->TypeIndex));
        ARENA_ARRAY_ADD(parameters,
            bc->genParameter(c,
                             parameterTypes[i],
                             paramName
            )
        );

        frameSize += paramSize;
    }
    assert(parametersCount == functionParameterCount);
    ctx->Parameters = parameters;
    ctx->ParameterTypes = parameterTypes;
    ctx->ParameterCount = functionParameterCount;

    metac_sema_statement_t* s = 0;

    for (uint32_t i = 0;
         i < function->FunctionBody->StatementCount;
         i++)
    {
        MetaCCodegen_doStatement(ctx, function->FunctionBody->Body[i]);
        goto LdoStmt;
    }

    LdoStmt: switch(s->StmtKind)
    {
        case stmt_block:
        {
            stmt_block_t* blockStmt = cast(stmt_block_t*) s;
        } break;

    }
}
static BCValue* MetaCCodegen_doExpression(metac_bytecode_ctx_t* ctx, metac_sema_expression_t* exp)
{

}

static void MetaCCodegen_PushSwitch(metac_bytecode_ctx_t* ctx, BCValue* exp)
{

}

static void MetaCCodegen_PopSwitch(metac_bytecode_ctx_t* ctx, BCValue* exp)
{

}

static void MetaCCodegen_doBlockStmt(metac_bytecode_ctx_t* ctx,
                                     metac_sema_statement_t* stmt)
{
    assert(stmt->StmtKind == stmt_block);
}

void MetaCCodegen_doStatement(metac_bytecode_ctx_t* ctx,
                              metac_sema_statement_t* stmt)
{
    void* c = ctx->c;
    switch(stmt->StmtKind)
    {
        case stmt_switch:
        {
            sema_stmt_switch_t* switchStatement = cast(sema_stmt_switch_t*) stmt;
            BCValue* switchExp = MetaCCodegen_doExpression(ctx, switchStatement->SwitchExp);
            MetaCCodegen_PushSwitch(ctx, switchExp);
            MetaCCodegen_doBlockStmt(ctx, switchStatement->SwitchBody);
            MetaCCodegen_PopSwitch(ctx, switchExp);
        } break;

        case stmt_block:
        {
            MetaCCodegen_doBlockStmt(ctx, stmt);
        } break;

        case stmt_case:
        {
            sema_stmt_case_t* caseStmt = cast(sema_stmt_case_t*) stmt;
            assert(ctx->SwitchStackCount > 0);
            metac_bytecode_switch_t* swtch = &ctx->SwitchStack[ctx->SwitchStackCount];
            metac_sema_expression_t* caseExp = caseStmt->CaseExp;

            // if there's no exp it's the default case
            if (!caseExp)
            {
                // make sure we haven't yet seen a default
                assert(METAC_NODE(swtch->DefaultBody) == emptyNode);
                swtch->DefaultBody = caseStmt->CaseBody;
                break;
            }


            bc->Eq3(c, 0,
                        ctx->SwitchStack[ctx->SwitchStackCount].Exp,
                        MetaCCodegen_doExpression(ctx, caseExp));

            bool hasBody =
                !(caseStmt->CaseBody && caseStmt->CaseBody->StmtKind != stmt_case);
            // if we have our own body we want to jump is the cnd is false
            // otherwise we want to jump if it's true
            CndJmpBegin cndJmp = bc->beginCndJmp(c, 0, !hasBody);
            if (hasBody)
            {
                BCLabel caseLabel;
                const uint32_t caseCount = swtch->PrevCaseJumpsCount;

                if (caseCount > 0)
                {
                    caseLabel = bc->genLabel(c);
                    for(uint32_t i = 0;
                        i < caseCount;
                        i++)
                    {
                        bc->endCndJmp(c, &swtch->PrevCaseJumps[i], caseLabel);
                    }
                }

                MetaCCodegen_doStatement(ctx, caseStmt->CaseBody);
                bc->endCndJmp(c, &cndJmp, bc->genLabel(c));
            }
            else
            {
                ARENA_ARRAY_ADD(swtch->PrevCaseJumps, cndJmp);
            }
        } break;
    }
}
