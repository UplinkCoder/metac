#include "metac_alloc.h"
#include "metac_codegen.h"
#include "libinterpret/bc_common.h"
#include "libinterpret/backend_interface_funcs.h"
#include "libinterpret/bc_interpreter_backend.h"
#include "repl/exp_eval.h"

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


metac_function_bytecode_t MetaCCodegen_GenerateFunction(metac_bytecode_ctx_t* ctx,
                                                         sema_decl_function_t* function)
{
    bc = &BCGen_interface;
    metac_function_bytecode_t result;

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

    for (uint32_t i = 0;
         i < function->FunctionBody->StatementCount;
         i++)
    {
        MetaCCodegen_doStatement(ctx, function->FunctionBody->Body[i]);
    }

    return 0;
}
static BCValue MetaCCodegen_doExpression(metac_bytecode_ctx_t* ctx, metac_sema_expression_t* exp)
{
    BCType expType = MetaCCodegen_GetBCType(ctx, exp->TypeIndex);
    BCValue v = bc->genTemporary(ctx->c, expType);
    WalkTree(ctx->c, &v, exp, &ctx->Vstore);
    return v;
}

static metac_bytecode_switch_t* MetaCCodegen_PushSwitch(metac_bytecode_ctx_t* ctx, BCValue exp)
{
    metac_bytecode_switch_t swtch = {};
    swtch.Exp = exp;

    ARENA_ARRAY_ADD(ctx->SwitchStack, swtch);

    return ctx->SwitchStack + (ctx->SwitchStackCount - 1);
}

static void MetaCCodegen_PopSwitch(metac_bytecode_ctx_t* ctx, BCValue exp)
{
    assert(ctx->SwitchStackCount > 0);

    metac_bytecode_switch_t* swtch =
        ctx->SwitchStack + (ctx->SwitchStackCount - 1);
    assert(BCValue_eq(&exp, &swtch->Exp));
    //FreeArena(ctx->Sw)
    --ctx->SwitchStackCount;
}

static void FixupBreaks(metac_bytecode_ctx_t* ctx, uint32_t breakCount, BCLabel breakLabel)
{
    void* c = ctx->c;
    if (ctx->BreaksCount > breakCount)
    {
        for(uint32_t i = breakCount;
            i < ctx->BreaksCount; i++)
        {
            bc->endJmp(c, (BCAddr)ctx->Breaks[i], breakLabel);
        }
        ctx->BreaksCount = breakCount;
    }
}

static void MetaCCodegen_doBlockStmt(metac_bytecode_ctx_t* ctx,
                                     sema_stmt_block_t* stmt)
{
    assert(stmt->StmtKind == stmt_block);
    uint32_t variableCount = ctx->VariablesCount;
    for(uint32_t i = 0; i < stmt->StatementCount; i++)
    {
        MetaCCodegen_doStatement(ctx, stmt);
    }
}

void MetaCCodegen_doStatement(metac_bytecode_ctx_t* ctx,
                              metac_sema_statement_t* stmt)
{
    void* c = ctx->c;
    switch(stmt->StmtKind)
    {
        case stmt_switch:
        {
            uint32_t currentBreakCount = ctx->BreaksCount;

            sema_stmt_switch_t* switchStatement = cast(sema_stmt_switch_t*) stmt;
            BCValue switchExp = MetaCCodegen_doExpression(ctx, switchStatement->SwitchExp);
            metac_bytecode_switch_t* swtch =
                MetaCCodegen_PushSwitch(ctx, switchExp);
            MetaCCodegen_doBlockStmt(ctx, switchStatement->SwitchBody);
            // gen default case if there is one.
            if (swtch->DefaultBody)
            {
                MetaCCodegen_doStatement(ctx, swtch->DefaultBody);
            }
            MetaCCodegen_PopSwitch(ctx, switchExp);
            BCLabel breakLabel = bc->genLabel(c);
            FixupBreaks(ctx, currentBreakCount, breakLabel);
        } break;

        case stmt_block:
        {
            MetaCCodegen_doBlockStmt(ctx, stmt);
        } break;

        case stmt_break:
        {
            ARENA_ARRAY_ADD(ctx->Breaks, bc->beginJmp(c));
        } break;

        case stmt_case:
        {
            sema_stmt_case_t* caseStmt = cast(sema_stmt_case_t*) stmt;
            assert(ctx->SwitchStackCount > 0);
            metac_bytecode_switch_t* swtch = &ctx->SwitchStack[ctx->SwitchStackCount];
            metac_sema_expression_t* caseExp = caseStmt->CaseExp;
            metac_sema_statement_t*  caseBody = caseStmt->CaseBody;
            // if there's no exp it's the default case
            if (!caseExp)
            {
                // make sure we haven't yet seen a default
                assert(METAC_NODE(swtch->DefaultBody) == emptyNode);
                swtch->DefaultBody = caseBody;
                break;
            }
            BCValue exp_result = MetaCCodegen_doExpression(ctx, caseExp);
            bc->Eq3(c, 0,
                    &ctx->SwitchStack[ctx->SwitchStackCount].Exp,
                    &exp_result);

            bool hasBody =
                !(caseBody && caseBody->StmtKind != stmt_case);
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
                if (caseBody)
                    MetaCCodegen_doStatement(ctx, caseBody);
            }
        } break;

        case stmt_return:
        {
            sema_stmt_return_t* returnStmt = cast(sema_stmt_return_t*) stmt;
            BCValue retVal = MetaCCodegen_doExpression(ctx, returnStmt->ReturnExp);
            bc->Ret(c, &retVal);
        } break;

        default:
        {
            printf("Statement unsupported %s\n", StatementKind_toChars(stmt->StmtKind));
        } break;
    }
}
