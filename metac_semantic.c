#include "metac_semantic.h"
#include <assert.h>
#include "metac_alloc_node.h"
#include "metac_target_info.h"
#include "metac_default_target_info.h"
#include "bsf.h"
#include <stdlib.h>
#include "crc32c.h"
#define AT(...)

#include "metac_simd.h"

#ifndef NO_FIBERS
#  include "metac_task.h"
#  include "metac_coro.c"
#endif

#include "metac_type_semantic.c"
#include "metac_expr_semantic.c"

const char* MetaCExpressionKind_toChars(metac_expression_kind_t);
bool IsExpressionNode(metac_node_kind_t);

bool Expression_IsEqual_(const metac_sema_expression_t* a,
                         const metac_sema_expression_t* b)
{
    bool result = true;
    if (a == b)
        assert(0);
        // pointer equality comparision should have been done
        // before calling this
    if (a->Kind == b->Kind)
    {
        switch(a->Kind)
        {
            case exp_signed_integer:
               result = a->ValueI64 == b->ValueI64;
            break;

            case exp_argument:
            {
                if (a->ArgumentList->ArgumentCount
                    == b->ArgumentList->ArgumentCount)
                {
                    const uint32_t ArgumentCount =
                        a->ArgumentList->ArgumentCount;
                    const metac_sema_expression_t* ExpsA
                            = a->ArgumentList->Arguments;
                    const metac_sema_expression_t* ExpsB
                            = b->ArgumentList->Arguments;

                    for(uint32_t i = 0;
                        i < ArgumentCount;
                        i++)
                    {
                        result &= Expression_IsEqual(ExpsA + i, ExpsB + i);
                    }
                }
                else
                {
                    result = false;
                }
            } break;

            default: assert(0); // Not handled right now
        }
    }
    else
        result = false;
Lret:
    return result;
}


// In case we do a stand alone compile of semantic we need this
#ifndef _METAC_PARSER_C_
    static uint32_t _nodeCounter = 64;
#endif

#ifndef _emptyPointer
#  define _emptyPointer (void*)0x1
#  define emptyNode (metac_node_t) _emptyPointer
#endif


#include "semantic/node_alloc.c"

void MetaCSemantic_Init(metac_semantic_state_t* self, metac_parser_t* parser,
                        metac_type_aggregate_t* compilerStruct)
{
    //const metac_semantic_state_t _init = {};
    // *self = _init;

    Allocator_Init(&self->Allocator, 0, 0);
    Allocator_Init(&self->TempAlloc, 0, AllocFlags_Temporary);

#define INIT_TYPE_TABLE(TYPE_NAME, MEMBER_NAME, INDEX_KIND) \
    TypeTableInitImpl((metac_type_table_t*)&self->MEMBER_NAME, \
                      sizeof(metac_type_ ## TYPE_NAME ## _t), \
                      type_index_## INDEX_KIND);

    FOREACH_TYPE_TABLE(INIT_TYPE_TABLE)

    FOREACH_SEMA_STATE_ARRAY(self, INIT_ARRAY)

    ARENA_ARRAY_INIT(metac_scope_t*, self->DeclStatementScope, &self->Allocator);

    self->TemporaryScopeDepth = 0;

    self->ExpressionStackCapacity = 64;
    self->ExpressionStackSize = 0;
    self->ExpressionStack = (metac_sema_expression_t*)
        calloc(sizeof(metac_sema_expression_t), self->ExpressionStackCapacity);

    self->SwitchStackCapacity = 4;
    self->SwitchStackSize = 0;
    self->SwitchStack = cast(sema_stmt_switch_t**)
        calloc(sizeof(sema_stmt_switch_t*), self->SwitchStackCapacity);

    self->Waiters.WaiterLock._rwctr = 0;
    self->Waiters.WaiterCapacity = 64;
    self->Waiters.WaiterCount = 0;
    self->Waiters.Waiters = cast(metac_semantic_waiter_t*)
                calloc(sizeof(*self->Waiters.Waiters), self->Waiters.WaiterCapacity);

    IdentifierTable_Init(&self->SemanticIdentifierTable, IDENTIFIER_LENGTH_SHIFT, 13);
    self->ParserIdentifierTable = &parser->IdentifierTable;
    self->ParserStringTable = &parser->StringTable;

    self->CurrentScope = 0;
    self->CurrentDeclarationState = 0;

    memset(&self->LRU, 0, sizeof(self->LRU));

    if (compilerStruct && ((metac_node_t)compilerStruct) != emptyNode)
    {
        self->CompilerInterface = compilerStruct;
    }
    else
    {
        self->CompilerInterface = 0;
    }

    self->initialized = true;
}

void MetaCSemantic_PopScope(metac_semantic_state_t* self)
{
    assert(self->CurrentScope);
    self->CurrentScope = self->CurrentScope->Parent;
}

metac_scope_owner_t ScopeParent(metac_semantic_state_t* sema,
                                 metac_scope_owner_kind_t parentKind,
                                 metac_node_t parentNode)
{
    metac_scope_owner_t scopeParent;

    uint32_t scopeParentIndex = 0;
    switch(parentKind)
    {
    case scope_owner_invalid :
    case scope_owner_extended :
    case scope_owner_unknown :
        assert(0);

    case scope_owner_module :
    //FIXME TODO this is not how it should be;
    // we should allocate module structures and so on
        scopeParentIndex = (intptr_t) parentNode;
    break;
    case scope_owner_function :
        scopeParentIndex = FunctionIndex(sema, (sema_decl_function_t*)parentNode);
    break;
    case scope_owner_struct :
        scopeParentIndex = StructIndex(sema, (metac_type_aggregate_t*)parentNode);
    break;
    case scope_owner_statement :
        scopeParentIndex = StatementIndex(sema, (metac_sema_statement_t*)parentNode);
    break;
    case scope_owner_block :
        scopeParentIndex = BlockStatementIndex(sema, (sema_stmt_block_t*)parentNode);
    break;
    case scope_owner_union :
        scopeParentIndex = UnionIndex(sema, (metac_type_aggregate_t*)parentNode);
    break;
    }
    scopeParent.v = scope_owner_V(parentKind, scopeParentIndex);

    return scopeParent;
}

bool IsTemporaryScope(metac_scope_t* scope_)
{
    return (scope_->ScopeFlags & scope_flag_temporary) != 0;
}
#define MetaCSemantic_PushTemporaryScope(SELF, TMPSCOPE) \
    MetaCSemantic_PushTemporaryScope_(SELF, TMPSCOPE, __LINE__, __FILE__)

metac_scope_t* MetaCSemantic_PushTemporaryScope_(metac_semantic_state_t* self,
                                                 metac_scope_t* tmpScope,
                                                 uint32_t line,
                                                 const char* file)
{
    // printf("[%u]Pushing tmpScope {%s:%u}\n", self->TemporaryScopeDepth, file, line);
    assert((tmpScope->ScopeFlags & scope_flag_temporary) != 0);

    assert ((!self->TemporaryScopeDepth) || IsTemporaryScope(self->CurrentScope));
    self->TemporaryScopeDepth++;

    tmpScope->Parent = self->CurrentScope;
    self->CurrentScope = tmpScope;

    return tmpScope;
}

void MetaCSemantic_PopTemporaryScope_(metac_semantic_state_t* self,
//                                      metac_scope_t* tmpScope,
                                      uint32_t line,
                                      const char* file)
{
    assert(self->TemporaryScopeDepth != 0 && IsTemporaryScope(self->CurrentScope));
    --self->TemporaryScopeDepth;
    // printf("[%u] Popping tmpScope {%s:%u}\n", self->TemporaryScopeDepth, file, line);
    self->CurrentScope = self->CurrentScope->Parent;
}
/*
#define MetaCSemantic_PushMountedScope(SELF, MNTSCOPE) \
    MetaCSemantic_PushMountedScope_(SELF, MNTSCOPE, __LINE__, __FILE__)

metac_scope_t* MetaCSemantic_PushMountedScope_(metac_semantic_state_t* self,
                                               metac_scope_t* mntScope,
                                               uint32_t line,
                                               const char* file)
{
    printf("[%u]Pushing mntScope {%s:%u}\n", self->TemporaryScopeDepth, file, line);
    assert((tmpScope->scopeFlags & scope_flag_mounted) == 0);

    assert ((!self->TemporaryScopeDepth) || IsTemporaryScope(self->CurrentScope));
    self->MountedScopeDepth++;

    tmpScope->Parent = self->CurrentScope;
    self->CurrentScope = tmpScope;

    return tmpScope;
}
 *
#define MetaCSemantic_PopMountedScope(SELF) \
    MetaCSemantic_PopMountedScope_(SELF, __LINE__, __FILE__)
void MetaCSemantic_PopMountedScope_(metac_semantic_state_t* self,
//                                      metac_scope_t* tmpScope,
                                      uint32_t line,
                                      const char* file)
{
    assert(self->TemporaryScopeDepth != 0 && IsTemporaryScope(self->CurrentScope));
    --self->MountedScopeDepth;
    printf("[%u] Popping mntScope {%s:%u}\n", self->TemporaryScopeDepth, file, line);
    self->CurrentScope = self->CurrentScope->Parent;
}
*/
metac_scope_t* MetaCSemantic_PushNewScope(metac_semantic_state_t* self,
                                          metac_scope_owner_kind_t parentKind,
                                          metac_node_t parentNode)
{
    metac_scope_owner_t scopeParent = ScopeParent(self, parentKind, parentNode);
    self->CurrentScope = MetaCScope_PushNewScope(self,
                                                 self->CurrentScope,
                                                 scopeParent);
    return self->CurrentScope;
}

sema_stmt_switch_t* MetaCSemantic_doSwitchSemantic(metac_semantic_state_t* self,
                                                  stmt_switch_t* switchStatement)
{
    sema_stmt_switch_t* semaSwitchStatement;

    AllocNewSemaStatement(self, stmt_switch, &semaSwitchStatement);

    semaSwitchStatement->SwitchExp =
        MetaCSemantic_doExprSemantic(self, switchStatement->SwitchExp, 0);

    self->SwitchStack[self->SwitchStackSize++] = semaSwitchStatement;
    semaSwitchStatement->SwitchBody =
        cast(sema_stmt_block_t*)MetaCSemantic_doStatementSemantic(self, switchStatement->SwitchBody);

    uint32_t statementCount =
        semaSwitchStatement->SwitchBody->StatementCount;

    self->SwitchStack[--self->SwitchStackSize];

    return semaSwitchStatement;
}
sema_stmt_casebody_t* MetaCSemantic_doCaseBodySemantic(metac_semantic_state_t* self,
                                                       stmt_case_t* caseStmt)
{
    sema_stmt_casebody_t* result = 0;

    STACK_ARENA_ARRAY(metac_statement_t*, caseBodyStatements, 16, &self->Allocator);
    metac_statement_t* currentStatement = caseStmt->CaseBody;
    while (METAC_NODE(currentStatement) != emptyNode)
    {
        ARENA_ARRAY_ADD(caseBodyStatements, currentStatement);
        currentStatement = currentStatement->Next;
    }

    if (caseBodyStatementsCount > 1)
    {
        // AllocNewSemaStatement
        AllocNewSemaCasebodyStatement(self, caseBodyStatementsCount, cast(void**)&result);

        for(uint32_t stmtIndex = 0;
            stmtIndex < caseBodyStatementsCount;
            stmtIndex++)
        {
            result->Statements[stmtIndex] =
                MetaCSemantic_doStatementSemantic(self, caseBodyStatements[stmtIndex]);
        }
    }
    else
    {
        result = cast(sema_stmt_casebody_t*)
            MetaCSemantic_doStatementSemantic(self, caseStmt->CaseBody);
    }

    ARENA_ARRAY_FREE(caseBodyStatements);
    return  result;

}
metac_sema_statement_t* MetaCSemantic_doStatementSemantic_(metac_semantic_state_t* self,
                                                           metac_statement_t* stmt,
                                                           const char* callFile,
                                                           uint32_t callLine)
{
    metac_sema_statement_t* result;

    // metac_printer_t printer;
    // MetaCPrinter_Init(&printer, self->ParserIdentifierTable, self->ParserStringTable);

    switch (stmt->Kind)
    {
        default:
        {
            printf("Statement unsupported %s\n", StatementKind_toChars(stmt->Kind));
            assert(0);
        } break;

        case stmt_while:
        {
            stmt_while_t* whileStmt = cast(stmt_while_t*) stmt;
            sema_stmt_while_t* semaWhileStmt =
                AllocNewSemaStatement(self, stmt_while, &result);
            semaWhileStmt->WhileExp =
                MetaCSemantic_doExprSemantic(self, whileStmt->WhileExp, 0);
            if (METAC_NODE(whileStmt->WhileBody) != emptyNode)
            {
                semaWhileStmt->WhileBody = MetaCSemantic_doStatementSemantic(self, whileStmt->WhileBody);
            }
            else
            {
                METAC_NODE(semaWhileStmt->WhileBody) = emptyNode;
            }
        } break;

        case stmt_exp:
        {
            stmt_exp_t* expStatement = (stmt_exp_t*) stmt;
            sema_stmt_exp_t* sse = AllocNewSemaStatement(self, stmt_exp, cast(void**)&result);
            sse->Expression =
                MetaCSemantic_doExprSemantic(self, expStatement->Expression, 0);
        } break;

        case stmt_comment:
        {
             //TODO it's funky to have statements which don't change in sema
            result = cast(metac_sema_statement_t*) stmt;
        } break;

        case stmt_decl:
        {
            stmt_decl_t* declStatement = cast(stmt_decl_t*) stmt;
            sema_stmt_decl_t* semaDeclStatement =
                AllocNewSemaStatement(self, stmt_decl, &result);
            // We now technically need to wrap to the created variable in a new scope
            // however let's be civil with the creation of new scopes
            // and only create one if the next statement is not a decl_stmt
            // scopes are expensive ...
            if (METAC_NODE(stmt->Next) != emptyNode && stmt->Next->Kind != stmt_decl)
            {
                metac_scope_owner_t owner = {
                    scope_owner_V(scope_owner_statement, StatementIndex(self, semaDeclStatement))
                };
                metac_scope_t* declScope =
                    MetaCScope_PushNewScope(self, self->CurrentScope, owner);

                ARENA_ARRAY_ADD(self->DeclStatementScope, declScope);

                // MetaCSemantic_PushTemporaryScope(self, &declScope);
            }
            semaDeclStatement->Declaration =
                MetaCSemantic_doDeclSemantic(self, declStatement->Declaration);
        } break;
/*
        default: {
            fprintf(stderr,
                "Statement not supported by semantic: %s\n",
                MetaCStatementKind_toChars(stmt->StmtKind));
                assert(0);
        } break;
*/
        case stmt_if:
        {
            stmt_if_t* ifStmt = cast(stmt_if_t*) stmt;
            sema_stmt_if_t* semaIfStmt =
                AllocNewSemaStatement(self, stmt_if, &result);
            semaIfStmt->IfCond =
                MetaCSemantic_doExprSemantic(self, ifStmt->IfCond, 0);
            if (METAC_NODE(ifStmt->IfBody) != emptyNode)
            {
                semaIfStmt->IfBody = MetaCSemantic_doStatementSemantic(self, ifStmt->IfBody);
            }
            else
            {
                METAC_NODE(semaIfStmt->IfBody) = emptyNode;
            }

            if (METAC_NODE(ifStmt->ElseBody) != emptyNode)
            {
                semaIfStmt->ElseBody = MetaCSemantic_doStatementSemantic(self, ifStmt->ElseBody);
            }
            else
            {
                METAC_NODE(semaIfStmt->ElseBody) = emptyNode;
            }
        } break;

        case stmt_case:
        {
            stmt_case_t* caseStatement = (stmt_case_t*) stmt;
            sema_stmt_case_t* semaCaseStatement =
                AllocNewSemaStatement(self, stmt_case, &result);

            assert(self->SwitchStackSize != 0);
            // the default statement doesn't have a caseExp
            // therefore we check it here so we can skip it.

            if (cast(metac_node_t)caseStatement->CaseExp != emptyNode)
            {
                semaCaseStatement->CaseExp =
                    MetaCSemantic_doExprSemantic(self, caseStatement->CaseExp, 0);
            }
            else
            {
                semaCaseStatement->CaseExp = (metac_sema_expression_t*)emptyNode;
            }
            semaCaseStatement->CaseBody =
                MetaCSemantic_doCaseBodySemantic(self, caseStatement);

        } break;

        case stmt_block:
        {
            stmt_block_t* blockStatement = (stmt_block_t*) stmt;
            uint32_t statementCount = blockStatement->StatementCount;
            sema_stmt_block_t* semaBlockStatement =
                AllocNewSemaBlockStatement(self, 0, statementCount, cast(void**)&result);

            metac_scope_owner_t parent = {scope_owner_V(scope_owner_statement,
                                           BlockStatementIndex(self, semaBlockStatement))};

            MetaCSemantic_PushNewScope(self,
                                       scope_owner_block,
                                       (metac_node_t)semaBlockStatement);

            metac_statement_t* currentStatement = blockStatement->Body;
            for(uint32_t i = 0;
                i < statementCount;
                i++)
            {
                semaBlockStatement->Body[i] =
                    MetaCSemantic_doStatementSemantic(self, currentStatement);
                currentStatement = currentStatement->Next;
            }
            //TODO this doesn't handle inject statements
            semaBlockStatement->StatementCount = statementCount;

            MetaCSemantic_PopScope(self);
        } break;

        case stmt_for:
        {
            stmt_for_t* for_ = (stmt_for_t*) stmt;
            sema_stmt_for_t* semaFor =
                AllocNewSemaStatement(self, stmt_for, for_);

            metac_scope_t* forScope = semaFor->Scope =
                MetaCSemantic_PushNewScope(self,
                                           scope_owner_statement,
                                           METAC_NODE(semaFor));

            metac_sema_declaration_t* ForInit =
                MetaCSemantic_doDeclSemantic(self, for_->ForInit);
            metac_sema_expression_t* ForCond =
                MetaCSemantic_doExprSemantic(self, for_->ForCond, 0);
            metac_sema_expression_t* ForPostLoop =
                MetaCSemantic_doExprSemantic(self, for_->ForPostLoop, 0);
        } break;

        //TODO for now stmt_yield and stmt_return
        // can share the same code as the layout is the same
        case stmt_yield:
        {
            stmt_yield_t* yieldStatement = (stmt_yield_t*) stmt;
            sema_stmt_yield_t* semaYieldStatement =
                AllocNewSemaStatement(self, stmt_yield, &result);

            metac_sema_expression_t* yieldValue =
                MetaCSemantic_doExprSemantic(self, yieldStatement->YieldExp, 0);
            semaYieldStatement->YieldExp = yieldValue;
        } break;

        case stmt_return:
        {
            stmt_return_t* returnStatement = (stmt_return_t*) stmt;
            sema_stmt_return_t* semaReturnStatement =
                AllocNewSemaStatement(self, stmt_return, &result);

            metac_sema_expression_t* returnValue =
                MetaCSemantic_doExprSemantic(self, returnStatement->ReturnExp, 0);
            semaReturnStatement->ReturnExp = returnValue;
        } break;

        case stmt_switch:
        {
            stmt_switch_t* switchStatement = cast(stmt_switch_t*) stmt;
            return cast(metac_sema_statement_t*)
                MetaCSemantic_doSwitchSemantic(self, switchStatement);
        } break;

        case stmt_break:
        {
            AllocNewSemaStatement(self, stmt_break, &result);
        } break;

        case stmt_continue:
        {
            AllocNewSemaStatement(self, stmt_break, &result);
        } break;
    }

    return result;
}

#ifndef INVALID_SIZE
#  define INVALID_SIZE \
      ((uint32_t)-1)
#endif

#ifndef U32
#  define U32(VAR) \
      (*(uint32_t*)(&VAR))
#endif

#ifdef SSE2
/// taken from https://github.com/AuburnSounds/intel-intrinsics/blob/master/source/inteli/emmintrin.d
/// Thanks Guillaume!
static inline uint32_t _mm_movemask_epi16( __m128i a )
{
    return _mm_movemask_epi8(_mm_packs_epi16(a, _mm_setzero_si128()));
}
#endif

#include "crc32c.h"
scope_insert_error_t MetaCSemantic_RegisterInScope(metac_semantic_state_t* self,
                                                   metac_identifier_ptr_t idPtr,
                                                   metac_node_t node)
{
    scope_insert_error_t result = no_scope;
    uint32_t idPtrHash = crc32c_nozero(~0, &idPtr.v, sizeof(idPtr.v));
    // first search for keys that might clash
    // and remove them from the LRU hashes
    uint16_t hash12 = idPtrHash & 0xFFF0;

    int16x8_t hashes;
    hashes = Load16(&self->LRU.LRUContentHashes);
    const int16x8_t hash12_8 = Set1_16(hash12);
    const int16x8_t hashMask = Set1_16((uint16_t)0xFFF0);
    const int16x8_t maskedHashes = And16(hashes, hashMask);
    const int16x8_t clearMask = Eq16(maskedHashes, hash12_8);
    const int16x8_t cleared = Andnot16(clearMask, hashes);
    Store16(&self->LRU.LRUContentHashes, cleared);

    if (self->CurrentScope != 0)
        result = MetaCScope_RegisterIdentifier(self->CurrentScope, idPtr, node);
#ifndef NO_FIBERS
#if 0
    EmitSignal(MetaCScope_RegisterIdentifier, idPtr);
#endif
    //RLOCK(&self->Waiters.WaiterLock);
    for(uint32_t i = 0; i < self->Waiters.WaiterCount; i++)
    {
        metac_semantic_waiter_t *waiter = &self->Waiters.Waiters[i];
        if (waiter->FuncHash == CRC32C_S("MetaCSemantic_LookupIdentifier")
         && waiter->NodeHash == CRC32C_VALUE(~0, idPtr))
        {
            task_t* waitingTask = cast(task_t*)waiter->Continuation->arg;
            assert(waitingTask->TaskFlags == Task_Waiting);
            printf("Found matching waiter\n");
            waitingTask->TaskFlags &= (~Task_Waiting);
            waitingTask->TaskFlags |= Task_Resumable;
            //RWLOCK(&self->Waiters.WaiterLock);
            {
                *waiter = self->Waiters.Waiters[--self->Waiters.WaiterCount];
            }
            //RWUNLOCK(&self->Waiters.WaiterLock);
        }
    }
    //RUNLOCK(&self->Waiters.WaiterLock);
#endif
    return result;
}


sema_decl_function_t* MetaCSemantic_doFunctionSemantic(metac_semantic_state_t* self,
                                                       decl_function_t* func)
{
    // one cannot do nested function semantic at this point
    // assert(self->CurrentDeclarationState == 0);
    metac_sema_decl_state_t declState = {0};
    self->CurrentDeclarationState = &declState;

    sema_decl_function_t* f = AllocNewSemaFunction(self, func);
    // for now we don't nest functions.
    f->ParentFunc = (sema_decl_function_t*)emptyNode;
    f->Identifier = func->Identifier;
    // printf("doing Function: %s\n", IdentifierPtrToCharPtr(self->ParserIdentifierTable, func->Identifier));

    // let's first do the parameters
    sema_decl_variable_t* params =
        f->Parameters =
            AllocFunctionParameters(self, f, func->ParameterCount);

    decl_parameter_t* currentParam = func->Parameters;
    for(uint32_t i = 0;
        i < func->ParameterCount;
        i++)
    {
        // let's do the parameter semantic inline
        // as we have an easier time if we know at which
        // param we are and how many follow
        decl_variable_t* paramVar = currentParam->Parameter;
        f->Parameters[i].VarFlags |= variable_is_parameter;
        f->Parameters[i].VarIdentifier = paramVar->VarIdentifier;
        if (METAC_NODE(paramVar->VarInitExpression) != emptyNode)
        {
            f->Parameters[i].VarInitExpression =
                MetaCSemantic_doExprSemantic(self, paramVar->VarInitExpression, 0);
        }
        else
        {
            METAC_NODE(f->Parameters[i].VarInitExpression) = emptyNode;
        }
        metac_type_index_t idx;
        idx = f->Parameters[i].TypeIndex =
            MetaCSemantic_doTypeSemantic(self,
                                         currentParam->Parameter->VarType);
        currentParam = currentParam->Next;
    }
    // now we should know the sizes
    assert(currentParam == emptyPointer);

    if (func->FunctionBody == emptyPointer)
    {
        return f;
    }

    metac_scope_owner_t Parent = {scope_owner_V(scope_owner_function, FunctionIndex(self, f))};

    f->Scope = MetaCSemantic_PushNewScope(self, scope_owner_function, (metac_node_t)f);
    // now we compute the position on the stack and Register them in the scope.

	uint32_t frameOffset = ((f->ParentFunc != cast(sema_decl_function_t*)emptyNode)
                           ? f->ParentFunc->FrameOffset : 0);

    metac_type_index_t returnType = MetaCSemantic_doTypeSemantic(self, func->ReturnType);

    // synthesize function type
    decl_type_functiontype_t fType = {};
    fType.Kind = decl_type_functiontype;
    fType.ReturnType = func->ReturnType;
    fType.Parameters = func->Parameters;
    fType.ParameterCount = func->ParameterCount;

    metac_type_index_t fTypeIndex = MetaCSemantic_doTypeSemantic(self, &fType);
    f->TypeIndex = fTypeIndex;

#if 0 // the code below is just for debugging
    {
        uint32_t hash = crc32c_nozero(~0, &returnType, sizeof(returnType));
        STACK_ARENA_ARRAY(metac_type_index_t, fParameterTypes, 16, &self->TempAlloc);
        ARENA_ARRAY_ENSURE_SIZE(fParameterTypes, func->ParameterCount);
        for(uint32_t i = 0; i < func->ParameterCount; i++)
        {
            ARENA_ARRAY_ADD(fParameterTypes, f->Parameters[i].TypeIndex);
        }

        hash = crc32c_nozero(hash, fParameterTypes, sizeof(*fParameterTypes) * func->ParameterCount);
        metac_type_functiontype_t functionType = {
            { decl_type_functiontype, 0, hash, 0 },
            returnType, fParameterTypes, func->ParameterCount
        };
        printf("hash: %x\n", hash);
    }
#endif
    for(uint32_t i = 0;
        i < func->ParameterCount;
        i++)
    {
        decl_variable_t* var = cast(decl_variable_t*)(f->Parameters +i);
        params[i].Storage.v = STORAGE_V(storage_stack, frameOffset);
        frameOffset += Align(MetaCSemantic_GetTypeSize(self, params[i].TypeIndex), 4);

        scope_insert_error_t result =
            MetaCScope_RegisterIdentifier(f->Scope, params[i].VarIdentifier,
                                          cast(metac_node_t)var);
    }
    f->FrameOffset = frameOffset;
    f->FunctionBody = cast(sema_stmt_block_t*)
        MetaCSemantic_doStatementSemantic(self, func->FunctionBody);

    MetaCSemantic_PopScope(self);

    return f;
}

metac_node_t NodeFromTypeIndex(metac_semantic_state_t* sema,
                               metac_type_index_t typeIndex)
{
    const uint32_t index = TYPE_INDEX_INDEX(typeIndex);
    switch(TYPE_INDEX_KIND(typeIndex))
    {
        case type_index_struct:
            return cast(metac_node_t) StructPtr(sema, index);
        case type_index_union:
            return cast(metac_node_t) UnionPtr(sema, index);
        case type_index_typedef:
            return cast (metac_node_t) TypedefPtr(sema, index);
        case type_index_enum:
            return cast(metac_node_t) EnumTypePtr(sema, index);
    }

    return 0;
}
#ifndef NO_FIBERS
typedef struct MetaCSemantic_doDeclSemantic_task_context_t
{
    metac_semantic_state_t* Sema;
    metac_declaration_t* Decl;
    metac_sema_declaration_t* Result;
} MetaCSemantic_doDeclSemantic_task_context_t;

const char* doDeclSemantic_PrintFunction(task_t* task)
{
    MetaCSemantic_doDeclSemantic_task_context_t* ctx =
         (MetaCSemantic_doDeclSemantic_task_context_t*)
            task->Context;
    char* buffer = cast(char*)malloc(256);
    metac_printer_t printer;
    //MetaCPrinter_Init(&printer, ctx->)
    const char* declPrint = 0;
//        MetaCPrinter_PrintDeclaration(&printer, ctx->Decl);

    sprintf(buffer, "doDeclSemantic {Sema: %p, Decl: %s}\n",
                    ctx->Sema, declPrint);

    return buffer;
}
#endif

metac_sema_declaration_t* MetaCSemantic_declSemantic(metac_semantic_state_t* self,
                                                     metac_declaration_t* decl)
{
    metac_sema_declaration_t* result = cast(metac_sema_declaration_t*)0xFEFEFEFE;
    metac_identifier_ptr_t declId = {0};

    switch(decl->Kind)
    {
        case decl_function:
        {
            decl_function_t* f = cast(decl_function_t*) decl;
            result = (metac_sema_declaration_t*)
                MetaCSemantic_doFunctionSemantic(self, f);

        } break;
        case decl_parameter:
            assert(0);
        case decl_variable:
        {
            decl_variable_t* v = cast(decl_variable_t*) decl;
            sema_decl_variable_t* var = AllocNewSemaVariable(self, v, &result);
            var->TypeIndex = MetaCSemantic_doTypeSemantic(self, v->VarType);
            if (METAC_NODE(v->VarInitExpression) != emptyNode)
            {
                var->VarInitExpression = MetaCSemantic_doExprSemantic(self, v->VarInitExpression, 0);
            }
            else
            {
                METAC_NODE(var->VarInitExpression) = emptyNode;
            }
            //TODO RegisterIdentifier
            var->VarIdentifier = v->VarIdentifier;
            MetaCSemantic_RegisterInScope(self, var->VarIdentifier, METAC_NODE(var));
        } break;
        case decl_type_enum:
            (cast(decl_type_t*)decl)->TypeKind = type_enum;
            declId = ((decl_type_enum_t*) decl)->Identifier;
            goto LdoTypeSemantic;
        case decl_type_struct:
            (cast(decl_type_t*)decl)->TypeKind = type_struct;
            declId = ((decl_type_struct_t*) decl)->Identifier;
            goto LdoTypeSemantic;
        case decl_type_union:
            (cast(decl_type_t*)decl)->TypeKind = type_union;
            declId = ((decl_type_union_t*) decl)->Identifier;
            goto LdoTypeSemantic;
        case decl_type_array:
            (cast(decl_type_t*)decl)->TypeKind = type_array;
            goto LdoTypeSemantic;
        case decl_type_typedef:
            (cast(decl_type_t*)decl)->TypeKind = type_typedef;
            declId = ((decl_type_typedef_t*) decl)->Identifier;
        goto LdoTypeSemantic;
        case decl_type_ptr:
            (cast(decl_type_t*)decl)->TypeKind = type_ptr;
        goto LdoTypeSemantic;
    LdoTypeSemantic:
        {
            metac_type_index_t type_index =
                MetaCSemantic_doTypeSemantic(self, (decl_type_t*)decl);
            if (declId.v != 0 && declId.v != -1)
            {
                metac_node_t node =
                    NodeFromTypeIndex(self, type_index);
                MetaCSemantic_RegisterInScope(self, declId, node);
                result = cast(metac_sema_declaration_t*)node;
            }
        } break;
    }
    assert(result != cast(metac_sema_declaration_t*)0xFEFEFEFE);
    return result;
}
#ifndef NO_FIBERS
void MetaCSemantic_doDeclSemantic_Task(task_t* task)
{
    const char* taskPrint = doDeclSemantic_PrintFunction(task);
    printf("Task: %s\n", taskPrint);
    free(taskPrint);

    MetaCSemantic_doDeclSemantic_task_context_t* ctx =
        (MetaCSemantic_doDeclSemantic_task_context_t*)
            task->Context;
    task_origin_t Origin = task->Origin;
    ctx->Result =
        MetaCSemantic_doDeclSemantic_(ctx->Sema, ctx->Decl, Origin.File, Origin.Line);
}
#endif
#define TracyMessage(MSG) \
    TracyCMessage(MSG, sizeof(MSG) - 1)
metac_sema_declaration_t* MetaCSemantic_doDeclSemantic_(metac_semantic_state_t* self,
                                                        metac_declaration_t* decl,
                                                        const char* callFile,
                                                        uint32_t callLine)
{
    metac_sema_declaration_t* result = 0;
    result = MetaCSemantic_declSemantic(self, decl);
    if (!result)
    {
#if !defined(NO_FIBERS)
        taskqueue_t* q = &CurrentWorker()->Queue;
        printf("Couldn't do the decl Semantic, yielding to try again\n");
        MetaCSemantic_doDeclSemantic_task_context_t CtxValue = {
            self, decl
        };

        task_t declTask;
        task_t* currentTask = CurrentTask();
        if (currentTask->TaskFunction == MetaCSemantic_doDeclSemantic_Task)
        {
            TracyMessage("pay attenetion now");
        }
        declTask.TaskFunction = MetaCSemantic_doDeclSemantic_Task;
                declTask.Origin.File = callFile;
        declTask.Origin.Line = callFile;
        declTask.Context = declTask._inlineContext;
        declTask.ContextSize = sizeof(CtxValue);
        assert(sizeof(CtxValue) < sizeof(declTask._inlineContext));
        (*(MetaCSemantic_doDeclSemantic_task_context_t*)declTask.Context) =
            CtxValue;
        MetaCSemantic_doDeclSemantic_task_context_t* CtxValuePtr = &CtxValue;
        printf("We should yield now\n");
        declTask.Continuation = currentTask;
        TaskQueue_Push(q, &declTask);
        currentTask->TaskFlags |= Task_Waiting;
        YIELD(waiting_for_declSemantic);
        printf("We are back\n");
        result = CtxValuePtr->Result;
#else
        printf("Fibers not enable cannot deal with out-of-order things without it\n");
#endif
    }
    return result;
}

#include "metac_printer.h"

/// retruns an emptyNode in case it couldn't be found in the cache
metac_node_t MetaCSemantic_LRU_LookupIdentifier(metac_semantic_state_t* self,
                                                 uint32_t idPtrHash,
                                                 metac_identifier_ptr_t idPtr)
{
    uint32_t mask = 0;
    int16x8_t hashes = Load16(&self->LRU.LRUContentHashes);

    metac_node_t result = emptyNode;
    uint16_t hash12 = idPtrHash & 0xFFF0;

    const int16x8_t hash12_8 = Set1_16(hash12);
    const int16x8_t hashMask = Set1_16(0xFFF0);
    const int16x8_t maskedHashes = And16(hashes, hashMask);
    const int16x8_t matches = Eq16(maskedHashes, hash12_8);
    mask = MoveMask16(matches);

    while(mask)
    {
        const uint32_t i = BSF(mask);
        // remove the bit we are going to check
        mask &= ~(i << i);
        if (self->LRU.Slots[i].Ptr.v == idPtr.v)
        {
            result = self->LRU.Slots[i].Node;
            break;
        }
    }

    return result;
}

/// Returns _emptyNode to signifiy it could not be found
/// a valid node otherwise
metac_node_t MetaCSemantic_LookupIdentifier(metac_semantic_state_t* self,
                                            metac_identifier_ptr_t identifierPtr)
{

    metac_node_t result = emptyNode;
    uint32_t idPtrHash = crc32c_nozero(~0, &identifierPtr.v, sizeof(identifierPtr.v));

    metac_scope_t *currentScope = self->CurrentScope;
    {
        while(currentScope)
        {
          metac_node_t lookupResult =
                MetaCScope_LookupIdentifier(currentScope, idPtrHash, identifierPtr);
            if (lookupResult)
            {
                result = lookupResult;
                break;
            }
            currentScope = currentScope->Parent;
        }
    }

    return result;
}

static inline void TypeToCharsP(metac_semantic_state_t* self,
                                metac_printer_t* printer,
                                metac_type_index_t typeIndex)
{
    uint32_t typeIndexIndex = TYPE_INDEX_INDEX(typeIndex);

    switch (TYPE_INDEX_KIND(typeIndex))
    {
        case type_index_array:
        {
            metac_type_array_t* arrayType =
                (self->ArrayTypeTable.Slots + TYPE_INDEX_INDEX(typeIndex));
            TypeToCharsP(self, printer, arrayType->ElementType);
            MetacPrinter_PrintStringLiteral(printer, "[");
            MetacPrinter_PrintI64(printer, (int64_t)arrayType->Dim);
            MetacPrinter_PrintStringLiteral(printer, "]");
        } break;
        case type_index_basic:
        {
            const char* typeString = BasicTypeToChars(typeIndex);
            MetacPrinter_PrintStringLiteral(printer, typeString);
        } break;
        case type_index_ptr:
        {
            metac_type_ptr_t* ptrType =
                (self->PtrTypeTable.Slots + TYPE_INDEX_INDEX(typeIndex));
            TypeToCharsP(self, printer, ptrType->ElementType);
            MetacPrinter_PrintStringLiteral(printer, "*");
        } break;
        case type_index_tuple:
        {
            metac_type_tuple_t* tupleType =
                (self->TupleTypeTable.Slots + TYPE_INDEX_INDEX(typeIndex));
            MetacPrinter_PrintStringLiteral(printer, "{");
            const uint32_t typeCount = tupleType->typeCount;
            for(uint32_t i = 0;
                i < tupleType->typeCount;
                i++)
            {
                TypeToCharsP(self, printer, tupleType->typeIndicies[i]);
                if (i != (typeCount - 1))
                {
                    MetacPrinter_PrintStringLiteral(printer, ", ");
                }
            }
            MetacPrinter_PrintStringLiteral(printer, "}");
        } break;
    }
}

const char* TypeToChars(metac_semantic_state_t* self, metac_type_index_t typeIndex)
{
    const char* result = 0;
    static metac_printer_t printer = {0};
    if (!printer.StringMemory)
        MetaCPrinter_InitSz(&printer, self->ParserIdentifierTable, 0, 32);
    else
        MetaCPrinter_Reset(&printer);
    TypeToCharsP(self, &printer, typeIndex);
    printer.StringMemory[printer.StringMemorySize++] = '\0';
    result = printer.StringMemory;

    return result;
}

#include <stdio.h>


#undef offsetof

#define offsetof(st, m) \
    ((size_t)((char *)&((st *)0)->m - (char *)0))



const metac_scope_t* GetAggregateScope(metac_type_aggregate_t* agg)
{
    return agg->Scope;
}


static inline int32_t GetConstI32(metac_semantic_state_t* self, metac_sema_expression_t* index, bool *errored)
{
    int32_t result = ~0;

    if (index->Kind == exp_signed_integer)
    {
        result = cast(int32_t) index->ValueI64;
    }
    else
    {
        *errored = true;
    }

    return result;
}

metac_sema_expression_t* MetaCSemantic_doIndexSemantic_(metac_semantic_state_t* self,
                                                        metac_expression_t* expr,
                                                        const char* callFun,
                                                        uint32_t callLine)
{
    metac_sema_expression_t* result = 0;

    metac_sema_expression_t* indexed = MetaCSemantic_doExprSemantic(self, expr->E1, 0/*, expr_asAddress*/);
    metac_sema_expression_t* index = MetaCSemantic_doExprSemantic(self, expr->E2, 0);
    if (indexed->Kind ==  exp_tuple)
    {
        bool errored = false;
        int32_t idx = GetConstI32(self, index, &errored);
        if ((int32_t)indexed->TupleExpressionCount > idx)
        {
            result = indexed->TupleExpressions + idx;
        }
        else if (!errored)
        {
            fprintf(stderr, "TupleIndex needs to be less than: %u", indexed->TupleExpressionCount);
        }
        else
        {
            fprintf(stderr, "index is not a constant value\n");
        }
    }
    else if (indexed->Kind == exp_variable)
    {
        assert(TYPE_INDEX_KIND(indexed->TypeIndex) == type_index_array
            || TYPE_INDEX_KIND(indexed->TypeIndex) == type_index_ptr);

        result = AllocNewSemaExpression(self, expr);
        result->TypeIndex = MetaCSemantic_GetElementType(self, indexed->TypeIndex);
        result->E1 = indexed;
        result->E2 = index;
    }

    return  result;
}
