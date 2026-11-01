// Semantic analysis for @run blocks and eject statements.
// Included from metac_semantic.c (unity build).

void MetaCSemantic_RunStmtSemantic(stmt_run_t* runStmt, metac_sema_state_t* parent)
{
    uint32_t runId = 0;
    bool hasEject = false;
    BCValue *compilerArg;
    BCValue *bufferArg;
    metac_semanatic_state_t* sema;

    assert(runStmt->Kind == stmt_run);
    // check if it contains an eject
    // first scan for an eject stmt as this determines wheter we should make
    // a token buffer
    // TODO write a little test. for the eject scanning

    for(metac_stmt_t* child = cast(metac_stmt_t*) runStmt->RunBody;
        METAC_NODE(child) != emptyNode;
        child = child->Next)
    {
        if (child->Kind == stmt_eject)
        {
            hasEject = true;
            break;
        }
    }
    MetaCSemantic_Init(sema, &LPP->Parser, 0);
    MetaCSemantic_PushNewScope(sema, scope_owner_module, cast(metac_node_t)cast(intptr_t)1);


    // TODO run sema on the run block so we can do codegen for it
    gen.Initailze();
    BCType compilerType = {0};
    // GetBCType(parent->CompilerVariable.TypeIndex);
    compilerArg = gen.GenParameter(c, compilerType);
    if (hasEject)
    {
        tokenBufferArg = gen.GenParameter(c, tokenBufferType)
    }
    runId = gen.beginFunction("__run__");
}


static metac_sema_stmt_t* MetaCRunSemantic_Execute(metac_sema_state_t* self,
                                                  sema_decl_function_t* function,
                                                  bool fileScope)
{
    metac_token_buffer_t buffer = {0};
    BCHeap heap = {0};
    AllocDefaultHeap(&heap);
    printf("running @run block\n");
    BCValue value = MetaCCodegen_RunRunBlock(self, function, &buffer, &heap);
    free(heap.heapData);
    if (value.vType == BCValueType_Error || value.vType == BCValueType_ErrorWithMessage ||
        value.vType == BCValueType_Exception || value.vType == BCValueType_Bailout)
    {
        free(buffer.Ptr);
        SemanticError(0, "@run execution failed");
        abort();
    }

    metac_parser_t* parser = self->Parser;
    metac_sema_stmt_t* result = 0;
    STACK_ARENA_ARRAY(metac_sema_stmt_t*, statements, 16, &self->TempAlloc);
    MetaCParser_PushTokenSource(parser, buffer.Ptr, buffer.Length);
    while (MetaCParser_PeekToken(parser, 1))
    {
        uint32_t before = parser->CurrentTokenIndex;
        if (fileScope)
        {
            metac_decl_t* decl = MetaCParser_ParseDecl(parser, 0);
            if (METAC_NODE(decl) == emptyNode)
                break;
            MetaCSemantic_doDeclSemantic(self, decl);
        }
        else
        {
            metac_stmt_t* stmt = MetaCParser_ParseStmt(parser, 0, 0);
            ARENA_ARRAY_ADD(statements, MetaCSemantic_doStmtSemantic(self, stmt));
        }
        if (parser->CurrentTokenIndex == before)
        {
            SemanticError(0, "reparsing @run output made no progress");
            abort();
        }
    }
    MetaCParser_PopTokenSource(parser);
    free(buffer.Ptr);
    if (!fileScope)
    {
        sema_stmt_block_t* block =
            AllocNewSemaBlockStmt(self, 0, statementsCount, cast(void**)&result);
        block->StmtCount = statementsCount;
        for (uint32_t i = 0; i < statementsCount; i++)
            block->Body[i] = statements[i];
    }
    ARENA_ARRAY_FREE(statements);
    return result;
}

static void MetaCRunSemantic_doRunStmt(metac_sema_state_t* self,
                                       stmt_run_t* runStmt,
                                       metac_sema_stmt_t** result)
{
    decl_type_t voidType = {0};
    voidType.Kind = decl_type;
    voidType.TypeKind = type_void;
    decl_function_t function = {0};
    function.Kind = decl_function;
    function.Flags = FuncFlag_Run;
    function.Identifier = empty_identifier;
    function.ReturnType = &voidType;
    function.FunctionBody = runStmt->RunBody;
    function.FunctionParameters = emptyPointer;
    function.YieldType = emptyPointer;
    uint32_t switchDepth = self->SwitchStackSize;
    self->SwitchStackSize = 0;
    sema_decl_function_t* runFunction = MetaCSemantic_doFunctionSemantic(self, &function);
    self->SwitchStackSize = switchDepth;
    *result = MetaCRunSemantic_Execute(self, runFunction, false);
}

static uint32_t MetaCRunSemantic_doEjectStmt(metac_sema_state_t* self,
                                             stmt_eject_t* ejectStmt,
                                             metac_sema_stmt_t** result,
                                             uint32_t hash)
{
    if (!self->RunScope)
    {
        SemanticError(0, "eject is only valid inside @run");
        abort();
    }
    sema_stmt_eject_t* semaEjectStmt =
        AllocNewSemaStmt(self, stmt_eject, result);
    const uint32_t exprCount = ejectStmt->EjectExpressionsCount;
    ARENA_ARRAY_INIT_SZ(metac_sema_expr_t*, semaEjectStmt->EjectExpressions, &self->Allocator, exprCount ? exprCount : 1);
    hash ^= eject_key;

    for(int i = 0; i < exprCount; i++)
    {
        semaEjectStmt->EjectExpressions[i] =
            MetaCSemantic_doExprSemantic(self, ejectStmt->EjectExpressions[i], 0);
        hash = CRC32C_VALUE(hash, semaEjectStmt->EjectExpressions[i]->Hash);
    }
    semaEjectStmt->EjectExpressionsCount = exprCount;

    // copy the tokens .... sigh!
    // TODO maybe we can transfer the array in one go ?
    ARENA_ARRAY_INIT_SZ(metac_token_t, semaEjectStmt->EjectTokens, &self->Allocator,
                       ejectStmt->EjectTokensCount ? ejectStmt->EjectTokensCount : 1);
    for(int i = 0; i < ejectStmt->EjectTokensCount; i++)
    {
        semaEjectStmt->EjectTokens[i] = ejectStmt->EjectTokens[i];
    }
    semaEjectStmt->EjectTokensCount = ejectStmt->EjectTokensCount;

    return hash;
}
