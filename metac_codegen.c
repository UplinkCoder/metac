#include "metac_alloc.h"
#include "libinterpret/bc_common.h"
#include "libinterpret/backend_interface_funcs.h"
#include "libinterpret/bc_interpreter_backend.h"
#include "libinterpret/printer_backend.c"
#include "repl/exp_eval.h"
#include "metac_codegen.h"

#include <stdarg.h>
uint32_t MetaCCodegen_GetTypeABISize(metac_bytecode_ctx_t* ctx, metac_type_index_t type)
{
    return 0;
}

void MetaCCodegen_doStatement(metac_bytecode_ctx_t* ctx,
                              metac_sema_statement_t* stmt);

BCType MetaCCodegen_GetBCType(metac_bytecode_ctx_t* ctx, metac_type_index_t type)
{
    BCType result = {};

    if (type.Kind == type_index_enum)
        result = (BCType){BCTypeEnum_i32};

    if (type.Kind == type_index_basic)
    {
        switch(type.Index)
        {
            case type_void:
                result = (BCType){BCTypeEnum_Void};
            break;
            case type_bool:
                result = (BCType){BCTypeEnum_u32};
            break;
            case type_char:
                result = (BCType){BCTypeEnum_i8};
            break;
            case type_short:
                result = (BCType){BCTypeEnum_i16};
            break;
            case type_int:
                result = (BCType){BCTypeEnum_i32};
            break;
            case type_long:
                result = (BCType){BCTypeEnum_i64};
            break;
            case type_size_t:
                result = (BCType){BCTypeEnum_u64};
            break;

            case type_float:
                result = (BCType){BCTypeEnum_f23};
            break;
            case type_double:
                result = (BCType){BCTypeEnum_f52};
            break;

            case type_long_long:
                result = (BCType){BCTypeEnum_i64};
            break;
            case type_long_double:
                result = (BCType){BCTypeEnum_f106};
            break;

            case type_unsigned_char:
                result = (BCType){BCTypeEnum_u8};
            break;
            case type_unsigned_short:
                result = (BCType){BCTypeEnum_u16};
            break;
            case type_unsigned_int:
                result = (BCType){BCTypeEnum_u32};
            break;
            case type_unsigned_long:
                result = (BCType){BCTypeEnum_u64};
            break;
            case type_unsigned_long_long:
                result = (BCType){BCTypeEnum_u64};
            break;
            default : assert(0);
        }
    }

    return  result;
}

extern const BackendInterface BCGen_interface;
const BackendInterface* bc;

int MetaCCodegen_RunFunction(metac_bytecode_ctx_t* self,
                             metac_bytecode_function_t f,
                             metac_alloc_t* interpAlloc,
                             const char* fargs, ...)
{
    va_list l;
    va_start(l, fargs);
    STACK_ARENA_ARRAY(BCValue, args, 8, interpAlloc);

    const char* farg = fargs;
    uint32_t nArgs = 0;
    for(char c = *farg++;c; c = *farg++)
    {
        switch(c)
        {
            case 'd' :
            {
                nArgs++;
                int32_t a = va_arg(l, int32_t);
                ARENA_ARRAY_ADD(args, imm32_(a, true));
            }
            break;
            case 'u':
            {
                nArgs++;
                uint32_t a = va_arg(l, uint32_t);
                ARENA_ARRAY_ADD(args, imm32(a));
            }
            break;
            default:
            {
                fprintf(stderr, "arg specifier, '%c' unsupported\n", c);
                assert(!"Value format unsupported");
            }

        }
    }
    va_end(l);

    BCValue result = bc->run(self->c, f.FunctionIndex, args, nArgs);
    return result.imm32.imm32;
}
void MetaCCodegen_End(metac_bytecode_ctx_t* self)
{
    bc->Finalize(self->c);

    if (bc == &Printer_interface)
    {
        Printer* printer = (Printer*)self->c;
        printf("%s\n\n", printer->BufferStart);
    }

}

void MetaCCodegen_Init(metac_bytecode_ctx_t* self, metac_alloc_t* parentAlloc)
{
    //TODO take BC as a parameter
    if (bc == 0)
    {
#if BC_PRINTER
        bc = &Printer_interface;
#else
        bc = &BCGen_interface;
#endif
    }

    (*self) = (metac_bytecode_ctx_t) {};
    Allocator_Init(&self->Allocator, parentAlloc, 0);

#ifndef BC_PRINTER
    tagged_arena_t* arena =
        AllocateArena(&self->Allocator, bc->sizeof_instance());
    self->c = arena->Memory;
    bc->init_instance(self->c);
#else
    bc->new_instance(&self->c);
    Printer* printer = (Printer*) self->c;
#endif
    bc->Initialize(self->c, 0);

    ARENA_ARRAY_INIT(metac_bytecode_function_t, self->Functions, &self->Allocator);

    // ARENA_ARRAY_INIT(sema_decl_variable_t, self->Locals, &self->Allocator);
    ARENA_ARRAY_INIT(BCValue, self->Globals, &self->Allocator);
}


void MetaCCodegen_Begin(metac_bytecode_ctx_t* self, metac_identifier_table_t* idTable)
{
    assert(self->c != 0);
    self->IdentifierTable = idTable;
    VariableStore_Init(&self->Vstore, idTable);
    ARENA_ARRAY_INIT(metac_bytecode_switch_t, self->SwitchStack, &self->Allocator);
}

metac_bytecode_function_t MetaCCodegen_GenerateFunction(metac_bytecode_ctx_t* ctx,
                                                        sema_decl_function_t* function)
{
    void* c = ctx->c;

    uint32_t frameSize = 0;
    uint32_t functionParameterCount = 1;
    STACK_ARENA_ARRAY(BCValue, parameters, 16, &ctx->Allocator);
    STACK_ARENA_ARRAY(BCValue, locals, 16, &ctx->Allocator);
    STACK_ARENA_ARRAY(BCAddr, breaks, 32, &ctx->Allocator);

    const char* fName =
        IdentifierPtrToCharPtr(ctx->IdentifierTable, function->Identifier);

    uint32_t functionId =
        bc->beginFunction(c, 0, fName);

    bc->Comment(c, "Function Begin.");

    metac_bytecode_function_t result;
    result.FunctionIndex = functionId;

    for(uint32_t i = 0;
        i < functionParameterCount;
        i++)
    {
        sema_decl_variable_t* paramVar = function->Parameters + i;
        uint32_t paramSize = MetaCCodegen_GetTypeABISize(ctx, paramVar->TypeIndex);

        const char* paramName = IdentifierPtrToCharPtr(ctx->IdentifierTable,
                                                       paramVar->VarIdentifier);
        BCType paramType = MetaCCodegen_GetBCType(ctx, paramVar->TypeIndex);
        BCValue param = bc->genParameter(c, paramType, paramName);
        ARENA_ARRAY_ADD(parameters, param);
        VariableStore_AddVariable(&ctx->Vstore, paramVar, &parameters[parametersCount - 1]);

        frameSize += paramSize;
    }
    assert(parametersCount == functionParameterCount);
    ctx->Parameters = parameters;
    ctx->ParameterCount = functionParameterCount;

    ctx->Locals = locals;
    ctx->LocalsArena = localsArena;
    ctx->LocalsAlloc = localsAlloc;
    ctx->LocalsCount = localsCount;

    ctx->Breaks = breaks;
    ctx->BreaksArena = breaksArena;
    ctx->BreaksAlloc = breaksAlloc;
    ctx->BreaksCount = breaksCount;

    for (uint32_t i = 0;
         i < function->FunctionBody->StatementCount;
         i++)
    {
        MetaCCodegen_doStatement(ctx, function->FunctionBody->Body[i]);
    }
    bc->Comment(c, "Function body end");

    if (ctx->BreaksAlloc)
    {
        FreeArena(&ctx->BreaksArena);
    }
    if (ctx->LocalsAlloc)
    {
        FreeArena(&ctx->LocalsArena);
    }

    bc->endFunction(c, result.FunctionIndex);
#ifdef PRINT_BYTECODE
    if (bc == &BCGen_interface)
    {
        BCGen_PrintCode(c, 0, 600);
    }
#endif
    return result;
}
static BCValue MetaCCodegen_doExpression(metac_bytecode_ctx_t* ctx,
                                         metac_sema_expression_t* exp)
{
    BCType expType = MetaCCodegen_GetBCType(ctx, exp->TypeIndex);
    BCValue v;
    if (exp->Kind != exp_signed_integer)
        v = bc->genTemporary(ctx->c, expType);
    WalkTree(ctx->c, &v, exp, &ctx->Vstore);
    return v;
}

static metac_bytecode_switch_t* MetaCCodegen_PushSwitch(metac_bytecode_ctx_t* ctx,
                                                        BCValue exp)
{
    metac_bytecode_switch_t swtch = {};
    swtch.Exp = exp;
    METAC_NODE(swtch.DefaultBody) = emptyNode;
    ARENA_ARRAY_INIT(metac_bytecode_casejmp_t, swtch.PrevCaseJumps, &ctx->Allocator);

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

static void FixupBreaks(metac_bytecode_ctx_t* ctx, uint32_t breakCount,
                        BCLabel breakLabel)
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
    assert(stmt->Kind == stmt_block);
    uint32_t variableCount = ctx->VariablesCount;
    for(uint32_t i = 0; i < stmt->StatementCount; i++)
    {
        MetaCCodegen_doStatement(ctx, stmt->Body[i]);
    }
}

static inline void MetaCCodegen_doCaseStmt(metac_bytecode_ctx_t* ctx,
                                           sema_stmt_case_t* caseStmt)
{
    void* c = ctx->c;
    assert(ctx->SwitchStackCount > 0);

    metac_bytecode_switch_t* swtch = &ctx->SwitchStack[ctx->SwitchStackCount - 1];
    metac_sema_expression_t* caseExp = caseStmt->CaseExp;
    metac_sema_statement_t*  caseBody = cast(metac_sema_statement_t*)caseStmt->CaseBody;

    // if there's no exp it's the default case
    if (METAC_NODE(caseExp) == emptyNode)
    {
        // make sure we haven't yet seen a default
        assert(METAC_NODE(swtch->DefaultBody) == emptyNode);
        swtch->DefaultBody = caseBody;
        return ;
    }
    BCValue* switchExp = &swtch->Exp;
    BCValue exp_result = MetaCCodegen_doExpression(ctx, caseExp);
    bc->Eq3(c, 0, switchExp, &exp_result);

    bool hasBody =
        !(caseBody && caseBody->Kind == stmt_case);
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
                metac_bytecode_casejmp_t caseJmp = swtch->PrevCaseJumps[i];
                switch(caseJmp.Kind)
                {
                    case casejmp_condJmp:
                    {
                       bc->endCndJmp(c, &caseJmp.cndJmp, caseLabel);
                    } break;
                    case casejmp_jmp:
                    {
                        bc->endJmp(c, caseJmp.jmp, caseLabel);
                    } break;
                }
            }
            swtch->PrevCaseJumpsCount = 0;
        }
        if (caseStmt->CaseBody->Kind == stmt_casebody)
        {
            const uint32_t stmtCount =
                caseStmt->CaseBody->StatementCount;

            for(uint32_t i = 0;
                i < stmtCount;
                i++)
            {
                MetaCCodegen_doStatement(ctx, caseStmt->CaseBody->Statements[i]);
            }
        }
        else
        {
            MetaCCodegen_doStatement(ctx, cast(metac_sema_statement_t*)caseStmt->CaseBody);
        }
        BCAddr nextCaseJmp = bc->beginJmp(c);
        metac_bytecode_casejmp_t caseJmp;
        caseJmp.Kind = casejmp_jmp;
        caseJmp.jmp = nextCaseJmp;
        ARENA_ARRAY_ADD(swtch->PrevCaseJumps, caseJmp);

        bc->endCndJmp(c, &cndJmp, bc->genLabel(c));
    }
    else
    {
        metac_bytecode_casejmp_t caseJmp;
        caseJmp.Kind = casejmp_condJmp;
        caseJmp.cndJmp = cndJmp;

        ARENA_ARRAY_ADD(swtch->PrevCaseJumps, caseJmp);
        if (caseBody)
            MetaCCodegen_doStatement(ctx, caseBody);
    }
}

static inline bool IsEmpty(metac_statement_t* stmt)
{
}

void MetaCCodegen_doLocalVar(metac_bytecode_ctx_t* ctx,
                             sema_decl_variable_t* localVar)
{
    const char* localName = IdentifierPtrToCharPtr(ctx->IdentifierTable,
                                                   localVar->VarIdentifier);
    BCType localType = MetaCCodegen_GetBCType(ctx, localVar->TypeIndex);
    BCValue local = bc->genLocal(ctx->c, localType, localName);
    ARENA_ARRAY_ADD(ctx->Locals, local);
    VariableStore_AddVariable(&ctx->Vstore, localVar, &ctx->Locals[(ctx->LocalsCount) - 1]);
    if (localVar->VarInitExpression)
    {
        BCValue initVal = MetaCCodegen_doExpression(ctx, localVar->VarInitExpression);
        bc->Set(ctx->c, &local, &initVal);
    }
}

void MetaCCodegen_doStatement(metac_bytecode_ctx_t* ctx,
                              metac_sema_statement_t* stmt)
{
    void* c = ctx->c;
    switch(stmt->Kind)
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
            if (METAC_NODE(swtch->DefaultBody) != emptyPointer)
            {
                MetaCCodegen_doStatement(ctx, swtch->DefaultBody);
            }
            MetaCCodegen_PopSwitch(ctx, switchExp);
            BCLabel breakLabel = bc->genLabel(c);
            FixupBreaks(ctx, currentBreakCount, breakLabel);
        } break;

        case stmt_block:
        {
            MetaCCodegen_doBlockStmt(ctx, cast(sema_stmt_block_t*)stmt);
        } break;

        case stmt_break:
        {
            ARENA_ARRAY_ADD(ctx->Breaks, bc->beginJmp(c));
        } break;

        case stmt_decl:
        {
            sema_stmt_decl_t* declStmt = cast(sema_stmt_decl_t*) stmt;
            sema_decl_variable_t* localVar = (sema_decl_variable_t*) declStmt->Declaration;
            MetaCCodegen_doLocalVar(ctx, localVar);
            // MetaCCodegen_doStatement(ctx, decl)
        } break;

        case stmt_case:
        {
            sema_stmt_case_t* caseStmt = cast(sema_stmt_case_t*) stmt;
            assert(ctx->SwitchStackCount > 0);
            MetaCCodegen_doCaseStmt(ctx, caseStmt);
        } break;

        case stmt_return:
        {
            sema_stmt_return_t* returnStmt = cast(sema_stmt_return_t*) stmt;
            BCValue retVal = MetaCCodegen_doExpression(ctx, returnStmt->ReturnExp);
            bc->Ret(c, &retVal);
        } break;

        case stmt_exp:
        {
            sema_stmt_exp_t* expStmt = cast(sema_stmt_exp_t*) stmt;
            MetaCCodegen_doExpression(ctx, expStmt->Expression);
        } break;

        case stmt_if:
        {
            sema_stmt_if_t* ifStmt = cast(sema_stmt_if_t*) stmt;
            BCValue cond = MetaCCodegen_doExpression(ctx, ifStmt->IfCond);
            CndJmpBegin cj = bc->beginCndJmp(c, &cond, false);
            {
                MetaCCodegen_doStatement(ctx, ifStmt->IfBody);
            }
            BCAddr skipElse;
            if (METAC_NODE(ifStmt->ElseBody) != emptyNode)
                skipElse =  bc->beginJmp(c);
            bc->endCndJmp(c, &cj, bc->genLabel(c));

            if (METAC_NODE(ifStmt->ElseBody) != emptyNode)
            {
                {
                    MetaCCodegen_doStatement(ctx, ifStmt->ElseBody);
                }
                bc->endJmp(c, skipElse, bc->genLabel(c));
            }
        } break;

        case stmt_do_while:
        {
            sema_stmt_while_t* whileStatement = cast(sema_stmt_while_t*) stmt;
            BCLabel beginLoop = bc->genLabel(c);

            MetaCCodegen_doStatement(ctx, whileStatement->WhileBody);

            BCLabel evalCond = bc->genLabel(c);
            BCValue cond = MetaCCodegen_doExpression(ctx, whileStatement->WhileExp);
            CndJmpBegin condExpJmp = bc->beginCndJmp(c, &cond, true);
            bc->endCndJmp(c, &condExpJmp, beginLoop);
        } break;

        case stmt_while:
        {
            sema_stmt_while_t* whileStatement = cast(sema_stmt_while_t*) stmt;
            BCLabel evalCond = bc->genLabel(c);
            BCValue cond = MetaCCodegen_doExpression(ctx, whileStatement->WhileExp);

            CndJmpBegin condExpJmp = bc->beginCndJmp(c, &cond, false);
            MetaCCodegen_doStatement(ctx, whileStatement->WhileBody);
            bc->Jmp(c, evalCond);
            bc->endCndJmp(c, &condExpJmp, bc->genLabel(c));
        } break;

        case stmt_for:
        {
            sema_stmt_for_t* forStatement = cast(sema_stmt_for_t*) stmt;

            if (forStatement->ForInit != emptyNode)
            {
                if (IsExpressionNode(forStatement->ForInit->Kind))
                {
                    MetaCCodegen_doExpression(ctx,
                        (metac_sema_expression_t*)forStatement->ForInit);
                }
                else
                {
                    MetaCCodegen_doLocalVar(ctx,
                        (sema_decl_variable_t*)forStatement->ForInit);
                }
            }

            CndJmpBegin cndJmpToCondEval;
            BCLabel loopBegin = bc->genLabel(c);

            if (forStatement->ForCond != emptyNode)
            {
                BCValue cond = MetaCCodegen_doExpression(ctx, forStatement->ForCond);
                cndJmpToCondEval = bc->beginCndJmp(c, &cond, false);
            }

            MetaCCodegen_doStatement(ctx, forStatement->ForBody);
            bc->Jmp(c, loopBegin);

            if (forStatement->ForCond != emptyNode)
            {
                bc->endCndJmp(c, &cndJmpToCondEval, bc->genLabel(c));
            }
        } break;

        default:
        {
            printf("Statement unsupported %s\n", StatementKind_toChars(stmt->Kind));
            assert(0);
        } break;
    }
}
