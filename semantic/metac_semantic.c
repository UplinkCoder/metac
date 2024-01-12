#include "metac_semantic.h"
#ifdef NO_FIBERS
#  include <assert.h>
#endif
#include "../parser/metac_alloc_node.h"
#include "../metac_target_info.h"
#include "../metac_default_target_info.h"
#include "../os/bsf.h"
#include <stdlib.h>
#include "../hash/crc32c.h"
#include "../printer/metac_printer.h"

#ifndef AT
#  define AT(...)
#endif

#include "../os/metac_simd.h"

#ifndef NO_FIBERS
#  include "../os/metac_task.h"
// XXX we probably don't want to include metac_coro.c here
#  include "../os/metac_coro.c"
#endif

#include "metac_type.c"
#include "metac_type_semantic.c"
#include "metac_expr_semantic.c"

const char* MetaCExprKind_toChars(metac_expr_kind_t);
bool IsExprNode(metac_node_kind_t);

bool Expr_IsEqual_(const metac_sema_expr_t* a,
                         const metac_sema_expr_t* b)
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
            case expr_signed_integer:
               result = a->ValueI64 == b->ValueI64;
            break;

            case expr_argument:
            {
                if (a->ArgumentList->ArgumentCount
                    == b->ArgumentList->ArgumentCount)
                {
                    const uint32_t ArgumentCount =
                        a->ArgumentList->ArgumentCount;
                    metac_sema_expr_t** ExpsA
                            = a->ArgumentList->Arguments;
                    metac_sema_expr_t** ExpsB
                            = b->ArgumentList->Arguments;

                    for(uint32_t i = 0;
                        i < ArgumentCount;
                        i++)
                    {
                        result &= Expr_IsEqual(ExpsA[i], ExpsB[i]);
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

#include "../semantic/node_alloc.c"

static const char* OnResolveFail_toChars(metac_semantic_on_resolve_fail_t onFail)
{
    const char* result = 0;

    switch(onFail)
    {
#define CASE(MEMBER) \
    case MEMBER: result = #MEMBER; break;
    FOREACH_ON_RESOLVE_FAIL(CASE)
#undef CASE
    default: break;
    }

    return result;
}


void MetaCSemantic_Init(metac_sema_state_t* self, metac_parser_t* parser,
                        metac_type_aggregate_t* compilerStruct)
{
    //const metac_sema_state_t _init = {};
    // *self = _init;

    self->nLocals = 0;
    self->MountParent = 0;

    Allocator_Init(&self->Allocator, 0, 0);
    Allocator_Init(&self->TempAlloc, 0, AllocFlags_Temporary);

#define INIT_TYPE_TABLE(TYPE_NAME, MEMBER_NAME, INDEX_KIND) \
    TypeTableInitImpl((metac_type_table_t*)&self->MEMBER_NAME, \
                      sizeof(metac_type_ ## TYPE_NAME ## _t), \
                      type_index_## INDEX_KIND, &self->Allocator);

    FOREACH_TYPE_TABLE(INIT_TYPE_TABLE)

    FOREACH_SEMA_STATE_ARRAY(self, INIT_ARENA_STATE_ARRAY)

    ARENA_ARRAY_INIT(metac_scope_t*, self->DeclStmtScope, &self->Allocator);

    ARENA_ARRAY_INIT(metac_sema_decl_t*, self->Globals, &self->Allocator);
    ARENA_ARRAY_INIT(metac_scope_t, self->Scopes, &self->Allocator);

    self->TemporaryScopeDepth = 0;

    self->ExprStackCapacity = 64;
    self->ExprStackSize = 0;
    self->ExprStack = (metac_sema_expr_t*)
        calloc(sizeof(metac_sema_expr_t), self->ExprStackCapacity);

    self->SwitchStackCapacity = 4;
    self->SwitchStackSize = 0;
    self->SwitchStack = cast(sema_stmt_switch_t**)
        calloc(sizeof(sema_stmt_switch_t*), self->SwitchStackCapacity);

    self->Waiters.WaiterLock._rwctr = 0;
    self->Waiters.WaiterCapacity = 64;
    self->Waiters.WaiterCount = 0;
    self->Waiters.Waiters = cast(metac_semantic_waiter_t*)
                calloc(sizeof(*self->Waiters.Waiters), self->Waiters.WaiterCapacity);

    IdentifierTable_Init(&self->SemanticIdentifierTable, IDENTIFIER_LENGTH_SHIFT, 13, &self->Allocator);
    self->ParserIdentifierTable = &parser->IdentifierTable;
    self->ParserStringTable = &parser->StringTable;
    self->ParserLocations = &parser->LocationStorage;

    self->LexerLocations = &parser->Lexer->LocationStorage;

    self->CurrentLocIdx.v = 0;
    self->CurrentScope = 0;
    self->CurrentDeclState = 0;

    ARENA_ARRAY_INIT(metac_semantic_on_resolve_fail_t, self->OnResolveFailStack, &self->Allocator);

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

void MetaCSemantic_PopScope(metac_sema_state_t* self)
{
    assert(self->CurrentScope);
    self->CurrentScope = self->CurrentScope->Parent;
}

metac_scope_owner_t ScopeParent(metac_sema_state_t* sema,
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
        scopeParentIndex = StmtIndex(sema, (metac_sema_stmt_t*)parentNode);
    break;
    case scope_owner_block :
        scopeParentIndex = BlockStmtIndex(sema, (sema_stmt_block_t*)parentNode);
    break;
    case scope_owner_union :
        scopeParentIndex = UnionIndex(sema, (metac_type_aggregate_t*)parentNode);
    break;
    }
    scopeParent.v = SCOPE_OWNER_V(parentKind, scopeParentIndex);

    return scopeParent;
}

bool IsTemporaryScope(metac_scope_t* scope_)
{
    return (scope_->ScopeFlags & scope_flag_temporary) != 0;
}
#define MetaCSemantic_PushTemporaryScope(SELF, TMPSCOPE) \
    MetaCSemantic_PushTemporaryScope_(SELF, TMPSCOPE, __LINE__, __FILE__)

metac_scope_t* MetaCSemantic_PushTemporaryScope_(metac_sema_state_t* self,
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

void MetaCSemantic_PopTemporaryScope_(metac_sema_state_t* self,
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

metac_scope_t* MetaCSemantic_PushMountedScope_(metac_sema_state_t* self,
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

#define MetaCSemantic_PopMountedScope(SELF) \
    MetaCSemantic_PopMountedScope_(SELF, __LINE__, __FILE__)
void MetaCSemantic_PopMountedScope_(metac_sema_state_t* self,
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
metac_scope_t* MetaCSemantic_PushNewScope(metac_sema_state_t* self,
                                          metac_scope_owner_kind_t parentKind,
                                          metac_node_t parentNode)
{
    metac_scope_owner_t scopeParent = ScopeParent(self, parentKind, parentNode);
    self->CurrentScope = MetaCScope_PushNewScope(self,
                                                 self->CurrentScope,
                                                 scopeParent);
    return self->CurrentScope;
}

metac_scope_t* MetaCSemantic_MountScope(metac_sema_state_t* self,
                                        metac_scope_t* scope_)
{
    assert(!self->MountParent);
    self->MountParent = scope_->Parent;
    assert(!(scope_->ScopeFlags & scope_flag_mounted));
    U32(scope_->ScopeFlags) |= scope_flag_mounted;
    scope_->Parent = self->CurrentScope;

    self->CurrentScope = scope_;

    return self->CurrentScope;
}

metac_scope_t* MetaCSemantic_UnmountScope(metac_sema_state_t* self)
{
    // it might be that the parent-scope we capture when mounting the scope
    // is null.
    // Therefore the assert below doesn't hold
    // We might want to make it hold in all cases by making the global scope explicit

    //assert(self->MountParent);
    assert(self->CurrentScope->ScopeFlags & scope_flag_mounted);

    U32(self->CurrentScope->ScopeFlags) &= ~scope_flag_mounted;
    metac_scope_t* parent = self->CurrentScope->Parent;
    self->CurrentScope->Parent = self->MountParent;

    self->MountParent = 0;
    self->CurrentScope = parent;

    return self->CurrentScope;
}


sema_stmt_switch_t* MetaCSemantic_doSwitchSemantic(metac_sema_state_t* self,
                                                   stmt_switch_t* switchStmt)
{
    sema_stmt_switch_t* semaSwitchStmt;

    AllocNewSemaStmt(self, stmt_switch, &semaSwitchStmt);

    semaSwitchStmt->SwitchExp =
        MetaCSemantic_doExprSemantic(self, switchStmt->SwitchExp, 0);

    semaSwitchStmt->Hash = CRC32C_VALUE(stmt_switch, switchStmt->SwitchExp->Hash);

    self->SwitchStack[self->SwitchStackSize++] = semaSwitchStmt;
    semaSwitchStmt->SwitchBody =
        cast(sema_stmt_block_t*)MetaCSemantic_doStmtSemantic(self, switchStmt->SwitchBody);

    uint32_t stmtCount =
        semaSwitchStmt->SwitchBody->StmtCount;

    self->SwitchStack[--self->SwitchStackSize];

    return semaSwitchStmt;
}
sema_stmt_casebody_t* MetaCSemantic_doCaseBodySemantic(metac_sema_state_t* self,
                                                       stmt_case_t* caseStmt)
{
    sema_stmt_casebody_t* result = 0;

    STACK_ARENA_ARRAY(metac_stmt_t*, caseBodyStmts, 16, &self->Allocator);
    metac_stmt_t* currentStmt = caseStmt->CaseBody;
    while (METAC_NODE(currentStmt) != emptyNode)
    {
        ARENA_ARRAY_ADD(caseBodyStmts, currentStmt);
        currentStmt = currentStmt->Next;
    }

    if (caseBodyStmtsCount > 1)
    {
        // AllocNewSemaStmt
        AllocNewSemaCasebodyStmt(self, caseBodyStmtsCount, cast(void**)&result);

        for(uint32_t stmtIndex = 0;
            stmtIndex < caseBodyStmtsCount;
            stmtIndex++)
        {
            result->Stmts[stmtIndex] =
                MetaCSemantic_doStmtSemantic(self, caseBodyStmts[stmtIndex]);
        }
    }
    else
    {
        result = cast(sema_stmt_casebody_t*)
            MetaCSemantic_doStmtSemantic(self, caseStmt->CaseBody);
    }

    ARENA_ARRAY_FREE(caseBodyStmts);
    return  result;

}

metac_type_index_t MetaCSemantic_GetType(metac_sema_state_t* self, metac_node_t node)
{
    metac_type_index_t typeIdx = {0};

    switch(node->Kind)
    {
        case decl_variable:
        {
            sema_decl_variable_t* var = cast(sema_decl_variable_t*) node;
            typeIdx = var->TypeIndex;
        } break;

        default: assert(0);
    }

    return typeIdx;
}

metac_sema_stmt_t* MetaCSemantic_doStmtSemantic_(metac_sema_state_t* self,
                                                           metac_stmt_t* stmt,
                                                           const char* callFile,
                                                           uint32_t callLine)
{
    metac_sema_stmt_t* result = 0;

    // metac_printer_t printer;
    // MetaCPrinter_Init(&printer, self->ParserIdentifierTable, self->ParserStringTable);
    uint32_t hash = ~0;

    switch (stmt->Kind)
    {
        default:
        {
            printf("Statement unsupported %s\n", StmtKind_toChars(stmt->Kind));
            assert(0);
        } break;

        case stmt_do_while:
        case stmt_while:
        {
            hash ^= stmt->Kind;

            stmt_while_t* whileStmt = cast(stmt_while_t*) stmt;
            sema_stmt_while_t* semaWhileStmt =
                AllocNewSemaStmt(self, stmt_while, &result);
            semaWhileStmt->Kind = stmt->Kind;

            semaWhileStmt->WhileExp =
                MetaCSemantic_doExprSemantic(self, whileStmt->WhileExp, 0);
            hash = CRC32C_VALUE(hash, whileStmt->WhileExp->Hash);
            if (METAC_NODE(whileStmt->WhileBody) != emptyNode)
            {
                semaWhileStmt->WhileBody =
                    MetaCSemantic_doStmtSemantic(self, whileStmt->WhileBody);
                hash = CRC32C_VALUE(hash, semaWhileStmt->WhileBody->Hash);
            }
            else
            {
                METAC_NODE(semaWhileStmt->WhileBody) = emptyNode;
            }
        } break;

        case stmt_expr:
        {
            hash ^= stmt_expr;
            stmt_expr_t* exprStmt = (stmt_expr_t*) stmt;
            sema_stmt_expr_t* sse = AllocNewSemaStmt(self, stmt_expr, cast(void**)&result);
            sse->Expr =
                MetaCSemantic_doExprSemantic(self, exprStmt->Expr, 0);
            hash = CRC32C_VALUE(hash, sse->Hash);
        } break;

        case stmt_comment:
        {
            hash ^= stmt_comment;
             //TODO it's funky to have statements which don't change in sema
            result = cast(metac_sema_stmt_t*) stmt;
            assert(stmt->Hash != 0);
            hash = CRC32C_VALUE(hash, stmt->Hash);
        } break;

        case stmt_decl:
        {
            hash ^= stmt_decl;
            stmt_decl_t* declStmt = cast(stmt_decl_t*) stmt;
            sema_stmt_decl_t* semaDeclStmt =
                AllocNewSemaStmt(self, stmt_decl, &result);
            // We now technically need to wrap to the created variable in a new scope
            // however let's be civil with the creation of new scopes
            // and only create one if the next statement is not a decl_stmt
            // scopes are expensive ...
            if (METAC_NODE(stmt->Next) != emptyNode && stmt->Next->Kind != stmt_decl)
            {
                metac_scope_owner_t owner = {
                    SCOPE_OWNER_V(scope_owner_statement, StmtIndex(self, semaDeclStmt))
                };
                metac_scope_t* declScope =
                    MetaCScope_PushNewScope(self, self->CurrentScope, owner);

                ARENA_ARRAY_ADD(self->DeclStmtScope, declScope);

                // MetaCSemantic_PushTemporaryScope(self, &declScope);
            }
            semaDeclStmt->Decl =
                MetaCSemantic_doDeclSemantic(self, declStmt->Decl);
        } break;
/*
        default: {
            fprintf(stderr,
                "Statement not supported by semantic: %s\n",
                MetaCStmtKind_toChars(stmt->StmtKind));
                assert(0);
        } break;
*/
        case stmt_if:
        {
            hash ^= stmt_if;
            stmt_if_t* ifStmt = cast(stmt_if_t*) stmt;
            sema_stmt_if_t* semaIfStmt =
                AllocNewSemaStmt(self, stmt_if, &result);

            semaIfStmt->IfCond =
                MetaCSemantic_doExprSemantic(self, ifStmt->IfCond, 0);
            hash = CRC32C_VALUE(hash, ifStmt->IfCond->Hash);

            if (METAC_NODE(ifStmt->IfBody) != emptyNode)
            {
                semaIfStmt->IfBody =
                    MetaCSemantic_doStmtSemantic(self, ifStmt->IfBody);
                hash = CRC32C_VALUE(hash, semaIfStmt->IfBody->Hash);
            }
            else
            {
                METAC_NODE(semaIfStmt->IfBody) = emptyNode;
            }

            if (METAC_NODE(ifStmt->ElseBody) != emptyNode)
            {
                semaIfStmt->ElseBody =
                    MetaCSemantic_doStmtSemantic(self, ifStmt->ElseBody);
                hash = CRC32C_VALUE(hash, semaIfStmt->ElseBody->Hash);
            }
            else
            {
                METAC_NODE(semaIfStmt->ElseBody) = emptyNode;
            }
        } break;

        case stmt_case:
        {
            stmt_case_t* caseStmt = (stmt_case_t*) stmt;
            sema_stmt_case_t* semaCaseStmt =
                AllocNewSemaStmt(self, stmt_case, &result);

            assert(self->SwitchStackSize != 0);
            // the default statement doesn't have a caseExp
            // therefore we check it here so we can skip it.
            if (cast(metac_node_t)caseStmt->CaseExp != emptyNode)
            {
                semaCaseStmt->CaseExp =
                    MetaCSemantic_doExprSemantic(self, caseStmt->CaseExp, 0);
                hash = CRC32C_VALUE(hash, semaCaseStmt->CaseExp->Hash);
            }
            else
            {
                semaCaseStmt->CaseExp = (metac_sema_expr_t*)emptyNode;
            }
            semaCaseStmt->CaseBody =
                MetaCSemantic_doCaseBodySemantic(self, caseStmt);
            if (METAC_NODE(semaCaseStmt->CaseBody) != emptyNode)
            {
                hash = CRC32C_VALUE(hash, semaCaseStmt->CaseBody->Hash);
            }
        } break;

        case stmt_block:
        {
            hash ^= stmt_block;
            stmt_block_t* blockStmt = (stmt_block_t*) stmt;
            uint32_t stmtCount = blockStmt->StmtCount;
            sema_stmt_block_t* semaBlockStmt =
                AllocNewSemaBlockStmt(self, 0, stmtCount, cast(void**)&result);

            metac_scope_owner_t parent = {SCOPE_OWNER_V(scope_owner_statement,
                                           BlockStmtIndex(self, semaBlockStmt))};

            MetaCSemantic_PushNewScope(self,
                                       scope_owner_block,
                                       (metac_node_t)semaBlockStmt);

            metac_stmt_t* currentStmt = blockStmt->Body;
            for(uint32_t i = 0;
                i < stmtCount;
                i++)
            {

                semaBlockStmt->Body[i] =
                    MetaCSemantic_doStmtSemantic(self, currentStmt);

                if (METAC_NODE(semaBlockStmt->Body[i]) != emptyPointer)
                {
                    hash = CRC32C_VALUE(hash, semaBlockStmt->Body[i]->Hash);
                }

                currentStmt = currentStmt->Next;
            }
            //TODO this doesn't handle inject statements
            semaBlockStmt->StmtCount = stmtCount;

            MetaCSemantic_PopScope(self);
        } break;

        case stmt_for:
        {
            hash ^= stmt_for;

            stmt_for_t* for_ = (stmt_for_t*) stmt;
            sema_stmt_for_t* semaFor =
                AllocNewSemaStmt(self, stmt_for, &result);

            metac_scope_t* forScope = semaFor->Scope =
                MetaCSemantic_PushNewScope(self,
                                           scope_owner_statement,
                                           METAC_NODE(semaFor));

            if (METAC_NODE(for_->ForInit) != emptyNode)
            {
                if (IsExprNode(for_->ForInit->Kind))
                {

                    semaFor->ForInit = cast(metac_node_t)
                        MetaCSemantic_doExprSemantic(self,
                            (cast(metac_expr_t*)for_->ForInit), 0);
                }
                else
                {
                    semaFor->ForInit = cast(metac_node_t)
                        MetaCSemantic_doDeclSemantic(self,
                            (cast(metac_decl_t*)for_->ForInit));
                }
                hash = CRC32C_VALUE(hash, semaFor->ForInit->Hash);
            }

            if (METAC_NODE(for_->ForCond) != emptyNode)
            {
                semaFor->ForCond =
                    MetaCSemantic_doExprSemantic(self, for_->ForCond, 0);
                hash = CRC32C_VALUE(hash, semaFor->ForCond->Hash);
            }
            else
            {
                METAC_NODE(semaFor->ForCond) = emptyNode;
            }

            if (METAC_NODE(for_->ForPostLoop) != emptyNode)
            {
                semaFor->ForPostLoop =
                    MetaCSemantic_doExprSemantic(self, for_->ForPostLoop, 0);
                hash = CRC32C_VALUE(hash, semaFor->ForPostLoop->Hash);
            }
            else
            {
                METAC_NODE(semaFor->ForPostLoop) = emptyNode;
            }

            if (METAC_NODE(for_->ForBody) != emptyNode)
            {
                metac_sema_stmt_t* forBody =
                    MetaCSemantic_doStmtSemantic(self, for_->ForBody);
                semaFor->ForBody = forBody;
                hash = CRC32C_VALUE(hash, semaFor->ForBody->Hash);
            }
            else
            {
                METAC_NODE(semaFor->ForBody) = emptyNode;
            }

            // Pop the forScope
            MetaCSemantic_PopScope(self);
        } break;

        //TODO for now stmt_yield and stmt_return
        // can share the same code as the layout is the same
        case stmt_yield:
        {
            hash ^= stmt_yield;

            stmt_yield_t* yieldStmt = (stmt_yield_t*) stmt;
            sema_stmt_yield_t* semaYieldStmt =
                AllocNewSemaStmt(self, stmt_yield, &result);

            metac_sema_expr_t* yieldValue =
                MetaCSemantic_doExprSemantic(self, yieldStmt->YieldExp, 0);
            semaYieldStmt->YieldExp = yieldValue;

            if (cast(metac_node_t)yieldValue != emptyNode)
            {
                hash = CRC32C_VALUE(hash, yieldValue->Hash);
            }
        } break;

        case stmt_return:
        {
            hash ^= stmt_return;

            stmt_return_t* returnStmt = (stmt_return_t*) stmt;
            sema_stmt_return_t* semaReturnStmt =
                AllocNewSemaStmt(self, stmt_return, &result);

            metac_sema_expr_t* returnValue =
                MetaCSemantic_doExprSemantic(self, returnStmt->ReturnExp, 0);
            semaReturnStmt->ReturnExp = returnValue;

            if (cast(metac_node_t)returnValue != emptyNode)
            {
                hash = CRC32C_VALUE(hash, returnValue->Hash);
            }
        } break;

        case stmt_switch:
        {
            stmt_switch_t* switchStmt = cast(stmt_switch_t*) stmt;

            result = cast(metac_sema_stmt_t*)
                MetaCSemantic_doSwitchSemantic(self, switchStmt);
            hash = CRC32C_VALUE(hash, result->Hash);
        } break;

        case stmt_break:
        {
            AllocNewSemaStmt(self, stmt_break, &result);
            hash = break_key;
        } break;

        case stmt_continue:
        {
            AllocNewSemaStmt(self, stmt_continue, &result);
            hash = continue_key;
        } break;
    }

    assert(result->Serial != 0);
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

void MetaCSemantic_ClearScope(metac_sema_state_t* self)
{

}

scope_insert_error_t MetaCSemantic_RegisterInScope(metac_sema_state_t* self,
                                                   metac_identifier_ptr_t idPtr,
                                                   metac_node_t node)
{
    const char* idChars = IdentifierPtrToCharPtr(self->ParserIdentifierTable, idPtr);
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
    {
        result = MetaCScope_RegisterIdentifier(self->CurrentScope, idPtr, node);
    }
    else
    {
        assert(!"RegisterInScope: CurrentScope is null");
    }
#ifndef NO_FIBERS
    /* At some point we want to emit a wake-up signal to waiters
       rather than doing an explicit loop here.
       it could look like
       EmitSignal(MetaCScope_RegisterIdentifier, idPtr);
     */
    //RLOCK(&self->Waiters.WaiterLock);
    for(uint32_t i = 0; i < self->Waiters.WaiterCount; i++)
    {
        metac_semantic_waiter_t *waiter = &self->Waiters.Waiters[i];
        if (waiter->FuncHash == CRC32C_S("MetaCSemantic_LookupIdentifier")
         && waiter->NodeHash == CRC32C_VALUE(~0, idPtr))
        {
            task_t* waitingTask = cast(task_t*)waiter->Continuation->arg;
            // assert(waitingTask->TaskFlags == Task_Waiting);
            printf("Found matching waiter\n");
            U32(waitingTask->TaskFlags) &= (~Task_Waiting);
            U32(waitingTask->TaskFlags) |= Task_Resumable;
            // Worker_EnqueueTask(worker, waitingTask);
            //RWLOCK(&self->Waiters.WaiterLock);
            {
                *waiter = self->Waiters.Waiters[--self->Waiters.WaiterCount];
            }
            break;
            //RWUNLOCK(&self->Waiters.WaiterLock);
        }
    }
    //RUNLOCK(&self->Waiters.WaiterLock);
#endif
    return result;
}


sema_decl_function_t* MetaCSemantic_doFunctionSemantic(metac_sema_state_t* self,
                                                       decl_function_t* func)
{
    // one cannot do nested function semantic at this point
    // assert(self->CurrentDeclState == 0);
    metac_sema_decl_state_t declState = {0};
    self->CurrentDeclState = &declState;

    sema_decl_function_t* f = AllocNewSemaFunction(self, func);
    // for now we don't nest functions.
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
        if (METAC_NODE(paramVar->VarInitExpr) != emptyNode)
        {
            f->Parameters[i].VarInitExpr =
                MetaCSemantic_doExprSemantic(self, paramVar->VarInitExpr, 0);
            assert(f->Parameters[i].Storage.Kind == storage_parameter);
        }
        else
        {
            METAC_NODE(f->Parameters[i].VarInitExpr) = emptyNode;
        }
        metac_type_index_t idx;
        idx = f->Parameters[i].TypeIndex =
            MetaCSemantic_doTypeSemantic(self,
                                         currentParam->Parameter->VarType);
        uint32_t hash = f->Parameters[i].TypeIndex.v;
        hash = CRC32C_VALUE(hash, i);
        f->Parameters[i].Hash = hash;
        currentParam = currentParam->Next;
    }
    // now we should know the sizes
    assert(currentParam == emptyPointer);

    if (func->FunctionBody == emptyPointer)
    {
        return f;
    }

    metac_scope_owner_t Parent = {SCOPE_OWNER_V(scope_owner_function, FunctionIndex(self, f))};

    f->Scope = MetaCSemantic_PushNewScope(self, scope_owner_function, (metac_node_t)f);
    // now we compute the position on the stack and Register them in the scope.

    sema_decl_function_t* parentFunc = cast(sema_decl_function_t*) emptyNode;

	uint32_t frameOffset = ((parentFunc != cast(sema_decl_function_t*)emptyNode)
                           ? parentFunc->FrameOffset : 0);

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
        decl_variable_t* var = cast(decl_variable_t*)(f->Parameters + i);
       // (XXX) here we tried to force the __cdecl calling convention
       // this is commented out for now
       // params[i].Storage.v = STORAGE_V(storage_stack, frameOffset);
        frameOffset += Align(MetaCSemantic_GetTypeSize(self, params[i].TypeIndex), 4);
        // We will reserve stack space though as we had pushed it on the stack
        // as we will need that space when we yield

        scope_insert_error_t result =
            MetaCScope_RegisterIdentifier(f->Scope, params[i].VarIdentifier,
                                          cast(metac_node_t)var);
    }
    f->FrameOffset = frameOffset;
    f->FunctionBody = cast(sema_stmt_block_t*)
        MetaCSemantic_doStmtSemantic(self, func->FunctionBody);

    MetaCSemantic_PopScope(self);

    MetaCSemantic_RegisterInScope(self, f->Identifier, METAC_NODE(f));

    return f;
}

const static sema_decl_type_t basicTypes[type_max] = {
    {decl_type, 0, 0, 0, storageclass_volatile, 0, TYPE_INDEX_V(type_index_invalid, 0u)},

    {decl_type, 0, 0, 0, storageclass_none, 0, TYPE_INDEX_V(type_index_basic, type_struct)},
    {decl_type, 0, 0, 0, storageclass_none, 0, TYPE_INDEX_V(type_index_basic, type_union)},
    {decl_type, 0, 0, 0, storageclass_none, 0, TYPE_INDEX_V(type_index_basic, type_class)},
    {decl_type, 0, 0, 0, storageclass_none, 0, TYPE_INDEX_V(type_index_basic, type_enum)},
    {decl_type, 0, 0, 0, storageclass_none, 0, TYPE_INDEX_V(type_index_basic, type_typedef)},
    {decl_type, 0, 0, 0, storageclass_none, 0, TYPE_INDEX_V(type_index_basic, type_functiontype)},

    {decl_type, 0, 0, 0, storageclass_none, 0, TYPE_INDEX_V(type_index_basic, type_auto)},
// DO NOT CHANGE THE ORDER FROM HERE
// XXX: Order needs to be in sync with the type tokens in metac_lexer.h
    {decl_type, 0, 0, 0, storageclass_none, 0, TYPE_INDEX_V(type_index_basic, type_void)},
    {decl_type, 0, 0, 0, storageclass_none, 0, TYPE_INDEX_V(type_index_basic, type_bool)},
    {decl_type, 0, 0, 0, storageclass_none, 0, TYPE_INDEX_V(type_index_basic, type_char)},
    {decl_type, 0, 0, 0, storageclass_none, 0, TYPE_INDEX_V(type_index_basic, type_short)},
    {decl_type, 0, 0, 0, storageclass_none, 0, TYPE_INDEX_V(type_index_basic, type_int)},
    {decl_type, 0, 0, 0, storageclass_none, 0, TYPE_INDEX_V(type_index_basic, type_long)},
    {decl_type, 0, 0, 0, storageclass_none, 0, TYPE_INDEX_V(type_index_basic, type_size_t)},

    {decl_type, 0, 0, 0, storageclass_none, 0, TYPE_INDEX_V(type_index_basic, type_float)},
    {decl_type, 0, 0, 0, storageclass_none, 0, TYPE_INDEX_V(type_index_basic, type_double)},
//TO HERE
    {decl_type, 0, 0, 0, storageclass_none, 0, TYPE_INDEX_V(type_index_basic, type_long_long)},
    {decl_type, 0, 0, 0, storageclass_none, 0, TYPE_INDEX_V(type_index_basic, type_long_double)},
// ALSO DON'T CHANGE ANYTHING FROM HERE
    {decl_type, 0, 0, 0, storageclass_none, 0, TYPE_INDEX_V(type_index_basic, type_unsigned_char)},
    {decl_type, 0, 0, 0, storageclass_none, 0, TYPE_INDEX_V(type_index_basic, type_unsigned_short)},
    {decl_type, 0, 0, 0, storageclass_none, 0, TYPE_INDEX_V(type_index_basic, type_unsigned_int)},
    {decl_type, 0, 0, 0, storageclass_none, 0, TYPE_INDEX_V(type_index_basic, type_unsigned_long)},
//TO HERE
    {decl_type, 0, 0, 0, storageclass_none, 0, TYPE_INDEX_V(type_index_basic, type_unsigned_long_long)},

    {decl_type, 0, 0, 0, storageclass_none, 0, TYPE_INDEX_V(type_index_basic, type_type)},
    {decl_type, 0, 0, 0, storageclass_none, 0, TYPE_INDEX_V(type_index_basic, type_identifier)},

    {decl_type, 0, 0, 0, storageclass_none, 0, TYPE_INDEX_V(type_index_basic, type_ptr)},
    {decl_type, 0, 0, 0, storageclass_none, 0, TYPE_INDEX_V(type_index_basic, type_array)},

    {decl_type, 0, 0, 0, storageclass_none, 0, TYPE_INDEX_V(type_index_basic, type_map)},

    {decl_type, 0, 0, 0, storageclass_none, 0, TYPE_INDEX_V(type_index_basic, type_tuple)},

    {decl_type, 0, 0, 0, storageclass_none, 0, TYPE_INDEX_V(type_index_basic, type_modifiers)}
};

sema_decl_type_t* TypeBasicPtr(metac_type_index_t basicTypeIdx)
{
    assert(basicTypeIdx.Kind == type_index_basic);
    return cast(sema_decl_type_t*) &basicTypes[basicTypeIdx.Index];
}

void SetTypeIndex(metac_type_t typeNode,
                  metac_type_index_t typeIndex)
{
    const uint32_t index = TYPE_INDEX_INDEX(typeIndex);
    switch(TYPE_INDEX_KIND(typeIndex))
    {
        case type_index_struct:
            (cast(metac_type_aggregate_t*) typeNode)->TypeIndex = typeIndex;
            break;
        case type_index_union:
            (cast(metac_type_aggregate_t*) typeNode)->TypeIndex = typeIndex;
            break;
        case type_index_typedef:
            (cast(metac_type_typedef_t*) typeNode)->TypeIndex = typeIndex;
        break;
        case type_index_enum:
            (cast(metac_type_enum_t*) typeNode)->TypeIndex = typeIndex;
        break;
        case type_index_basic:
            (cast(metac_type_basic_t*)typeNode)->TypeIndex = typeIndex;
        break;
        case type_index_tuple:
            (cast(metac_type_tuple_t*)typeNode)->TypeIndex = typeIndex;
        break;
    }
}

metac_type_t NodeFromTypeIndex(metac_sema_state_t* sema,
                               metac_type_index_t typeIndex)
{
    const uint32_t index = TYPE_INDEX_INDEX(typeIndex);
    switch(TYPE_INDEX_KIND(typeIndex))
    {
        case type_index_ptr:
            return cast(metac_type_t) PtrTypePtr(sema, index);
        case type_index_struct:
            return cast(metac_type_t) StructPtr(sema, index);
        case type_index_union:
            return cast(metac_type_t) UnionPtr(sema, index);
        case type_index_typedef:
            return cast(metac_type_t) TypedefPtr(sema, index);
        case type_index_enum:
            return cast(metac_type_t) EnumTypePtr(sema, index);
        case type_index_basic:
            return cast(metac_type_t) TypeBasicPtr(typeIndex);
        case type_index_tuple:
            return cast(metac_type_t) TupleTypePtr(sema, index);
        case type_index_functiontype:
            return cast(metac_type_t) FunctiontypePtr(sema, index);
        case type_index_array:
            return cast(metac_type_t) ArrayTypePtr(sema, index);
    }

    return 0;
}
#ifndef NO_FIBERS
typedef struct MetaCSemantic_doDeclSemantic_task_context_t
{
    metac_sema_state_t* Sema;
    metac_decl_t* Decl;
    metac_sema_decl_t* Result;
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
//        MetaCPrinter_PrintDecl(&printer, ctx->Decl);

    sprintf(buffer, "doDeclSemantic {Sema: %p, Decl: %s}\n",
                    ctx->Sema, declPrint);

    return buffer;
}
#endif

metac_sema_decl_t* MetaCSemantic_declSemantic(metac_sema_state_t* self,
                                                     metac_decl_t* decl)
{
    metac_sema_decl_t* result = cast(metac_sema_decl_t*)(intptr_t)0xFEFEFEFE;
    metac_identifier_ptr_t declId = {0};

    switch(decl->Kind)
    {
        case decl_function:
        {
            decl_function_t* f = cast(decl_function_t*) decl;
            result = cast(metac_sema_decl_t*)
                MetaCSemantic_doFunctionSemantic(self, f);

        } break;
        case decl_parameter:
            assert(0);
        case decl_comment:
        {
            result = cast(metac_sema_decl_t*)decl;
        } break;
        case decl_variable:
        {
            decl_variable_t* v = cast(decl_variable_t*) decl;
            sema_decl_variable_t* var = AllocNewSemaVariable(self, v, &result);
            /// XXX FIXME we want to assign a variable serial
            /// after we have determined the storage location ideally.
            /// to keep stuff wokring though we just assign the decl hash

            var->Hash = v->Hash;
            var->TypeIndex = MetaCSemantic_doTypeSemantic(self, v->VarType);

            if (METAC_NODE(v->VarInitExpr) != emptyNode)
            {
                var->VarInitExpr = MetaCSemantic_doExprSemantic(self, v->VarInitExpr, 0);
            }
            else
            {
                METAC_NODE(var->VarInitExpr) = emptyNode;
            }

            //TODO make sure nLocals is reset at the end of a function
            //     also this doesn't deal with static properly
            var->VarIdentifier = v->VarIdentifier;
            if (v->StorageClass == storageclass_local)
            {
                var->Storage.v = STORAGE_V(storage_local, self->nLocals++);
            }
            else if (v->StorageClass == storageclass_global)
            {
                var->Storage.v = STORAGE_V(storage_global, self->GlobalsCount);
                // TODO maybe we have to mark the global as having been inserted.
                ARENA_ARRAY_ADD(self->Globals, cast(metac_sema_decl_t*)var);
            }

            MetaCSemantic_RegisterInScope(self, var->VarIdentifier, METAC_NODE(var));
/*
            Info("Introducing variable: %s\n",
                  IdentifierPtrToCharPtr(self->ParserIdentifierTable, v->VarIdentifier));
*/
        } break;
        case decl_type_typeof:
            goto LdoTypeSemantic;
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
            metac_type_t typeNode =
                NodeFromTypeIndex(self, type_index);
            result = cast(metac_sema_decl_t*)typeNode;
            SetTypeIndex(typeNode, type_index);

            if (declId.v != 0 && declId.v != -1)
            {
                MetaCSemantic_RegisterInScope(self, declId, METAC_NODE(typeNode));
            }
        } break;
    }
    assert(result != cast(metac_sema_decl_t*)0xFEFEFEFE);
    return result;
}
#ifndef NO_FIBERS
void MetaCSemantic_doDeclSemantic_Task(task_t* task)
{
    const char* taskPrint = doDeclSemantic_PrintFunction(task);
    printf("Task: %s\n", taskPrint);
    free(cast(void*)taskPrint);

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
metac_sema_decl_t* MetaCSemantic_doDeclSemantic_(metac_sema_state_t* self,
                                                 metac_decl_t* decl,
                                                 const char* callFile,
                                                 uint32_t callLine)
{
    metac_sema_decl_t* result = 0;
    result = MetaCSemantic_declSemantic(self, decl);
    if (!result)
    {
#ifndef NO_FIBERS
        taskqueue_t* q = &CurrentWorker()->Queue;
        printf("Couldn't do the decl Semantic, yielding to try again\n");
        MetaCSemantic_doDeclSemantic_task_context_t CtxValue = {
            self, decl
        };

        task_t declTask;
        task_t* currentTask = CurrentTask();
        if (currentTask->TaskFunction == MetaCSemantic_doDeclSemantic_Task)
        {
            TracyMessage("pay attention now");
        }
        declTask.TaskFunction = MetaCSemantic_doDeclSemantic_Task;
        declTask.Origin.File = callFile;
        declTask.Origin.Line = callLine;
        declTask.Context = declTask._inlineContext;
        declTask.ContextSize = sizeof(CtxValue);
        assert(sizeof(CtxValue) < sizeof(declTask._inlineContext));
        (*(MetaCSemantic_doDeclSemantic_task_context_t*)declTask.Context) =
            CtxValue;
        MetaCSemantic_doDeclSemantic_task_context_t* CtxValuePtr = &CtxValue;
        printf("We should yield now\n");
        declTask.Continuation = currentTask;
        TaskQueue_Push(q, &declTask);
        U32(currentTask->TaskFlags) |= Task_Waiting;

        YIELD(waiting_for_declSemantic);

        printf("We are back\n");
        result = CtxValuePtr->Result;
#else
        printf("Fibers not enable cannot deal with out-of-order things without it\n");
#endif
    }
    return result;
}

/// retruns an emptyNode in case it couldn't be found in the cache
metac_node_t MetaCSemantic_LRU_LookupIdentifier(metac_sema_state_t* self,
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
        mask &= ~(1 << i);
        if (self->LRU.Slots[i].Ptr.v == idPtr.v)
        {
            result = self->LRU.Slots[i].Node;
            break;
        }
    }

    return result;
}
// Sets the behavior for the case of a name-resolve failing
void MetaCSemantic_PushOnResolveFail(metac_sema_state_t* self,
                                     metac_semantic_on_resolve_fail_t onFail)
{
    ARENA_ARRAY_ADD(self->OnResolveFailStack, onFail);
}

// Resets the behavior for the case of a name-resolve failing
void MetaCSemantic_PopOnResolveFail(metac_sema_state_t* self)
{
    --self->OnResolveFailStackCount;
}

bool IsUnresolved(metac_node_t node)
{
    return node == (metac_node_t)0 || node->Kind == node_expr_unknown_value;
}

/// Returns _emptyNode to signifiy it could not be found
/// a valid node otherwise
metac_node_t MetaCSemantic_LookupIdentifierInScope(metac_scope_t* scope_,
                                                   metac_identifier_ptr_t identifierPtr)
{

    metac_node_t result = emptyNode;
    uint32_t idPtrHash = crc32c_nozero(~0, &identifierPtr.v, sizeof(identifierPtr.v));
    metac_scope_t *currentScope = scope_;

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

/// Returns _emptyNode to signifiy it could not be found
/// a valid node otherwise
metac_node_t MetaCSemantic_LookupIdentifier(metac_sema_state_t* self,
                                            metac_identifier_ptr_t identifierPtr)
{

    metac_node_t result = emptyNode;
    uint32_t idPtrHash = crc32c_nozero(~0, &identifierPtr.v, sizeof(identifierPtr.v));
#if 0
    printf("Looking up: %s\n",
        IdentifierPtrToCharPtr(self->ParserIdentifierTable, identifierPtr));
#endif
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
            assert(currentScope != currentScope->Parent);
            currentScope = currentScope->Parent;
        }
    }

    return result;
}

const char* TypeToChars(metac_sema_state_t* self, metac_type_index_t typeIndex)
{
    const char* result = 0;
    static metac_printer_t printer = {0};
    if (!printer.StringMemory)
        MetaCPrinter_InitSz(&printer, self->ParserIdentifierTable, 0, (metac_alloc_t*)0, 32);
    else
        MetaCPrinter_Reset(&printer);
    TypeToCharsP(self, &printer, typeIndex);
    printer.StringMemory[printer.StringMemoryCount++] = '\0';
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
