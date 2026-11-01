/*
 * conv89 - rewrite C99 constructs towards C89
 *
 *  - pushes declarations out of `for` initializers
 *      for (int i = 0; i < n; i++)  ->  int i; for (i = 0; i < n; i++)
 *  - pushes mid-block variable declarations to the top of their block,
 *    leaving `name = init;` at the original position when the initializer
 *    can be expressed as an assignment (so evaluation order is kept)
 *
 * usage: conv89 <file.c> [more.c ...]
 * converted source is written to stdout
 *
 * note: the underlying parser consumes preprocessor directives, so
 * #include/#define lines do not survive the conversion.
 */
#include <stdio.h>
#include <string.h>

#define NO_FIBERS
#define NO_SEMANTIC
#define OLD_PARSER

#include "../os/compat.h"
#include "../driver/metac_driver.c"
#include "../utils/read_file.c"
#include "../parser/metac_parser_obj.c"

#ifdef DEBUG_SERVER
#  include "../debug/debug_server.c"
#endif

/* parser/metac_parsetree.c #undefs emptyNode after use */
#ifndef emptyNode
#  define emptyNode ((metac_node_t) 0x1)
#endif

static uint32_t conv89_forHoisted;
static uint32_t conv89_declsHoisted;

static inline bool conv89_isEmptyNode(const void* p)
{
    return p == emptyPointer;
}

static metac_expr_t* NewIdentifierExpr(metac_identifier_ptr_t id)
{
    metac_expr_t* result = AllocNewExpr(expr_identifier);
    result->IdentifierPtr = id;
    result->IdentifierKey = id.v;
    result->Hash = id.v;
    return result;
}

static metac_expr_t* NewAssignExpr(metac_expr_t* lhs, metac_expr_t* rhs)
{
    metac_expr_t* result = AllocNewExpr(expr_assign);
    result->E1 = lhs;
    result->E2 = rhs;
    result->Hash = CRC32C_VALUE(lhs->Hash, rhs->Hash);
    return result;
}

static metac_stmt_t* NewExprStmt(metac_expr_t* exp)
{
    metac_stmt_t* result = 0;
    stmt_expr_t* exprStmt;
    AllocNewStmt(stmt_expr, &result);
    exprStmt = (stmt_expr_t*) result;
    exprStmt->Expr = exp;
    result->Hash = exp->Hash;
    return result;
}

static metac_stmt_t* NewDeclStmt(metac_decl_t* decl)
{
    metac_stmt_t* result = 0;
    stmt_decl_t* declStmt;
    AllocNewStmt(stmt_decl, &result);
    declStmt = (stmt_decl_t*) result;
    declStmt->Decl = decl;
    result->Hash = decl->Hash;
    return result;
}

static metac_stmt_t* NewBlockStmt(metac_stmt_t* body)
{
    metac_stmt_t* result = 0;
    stmt_block_t* block;
    AllocNewStmt(stmt_block, &result);
    block = (stmt_block_t*) result;
    block->Body = body;
    block->StmtCount = 0;
    {
        metac_stmt_t* s = body;
        while (!conv89_isEmptyNode(s))
        {
            block->StmtCount++;
            s = s->Next;
        }
    }
    result->Hash = ~0u;
    return result;
}

static void TransformStmt(metac_stmt_t** link, bool canInsertDecl);
static void TransformBlock(stmt_block_t* block);

/* can a decl statement with this initializer be split into
   a bare declaration + an assignment statement? */
static bool CanSplitDecl(decl_variable_t* var)
{
    if (conv89_isEmptyNode(var->VarInitExpr))
        return false;
    /* top-level comma: the parser folds `int a = 1, b = 2` into one
       variable whose "initializer" is the comma expression */
    if (var->VarInitExpr->Kind == expr_comma)
        return false;
    /* brace initializers parse as tuples and cannot become assignments */
    if (var->VarInitExpr->Kind == expr_tuple)
        return false;
    /* array types keep their initializer for the same reason */
    if (!conv89_isEmptyNode(var->VarType) && var->VarType->Kind == decl_type_array)
        return false;
    return true;
}

/* hoist the declaration statements of a block to the front of the block */
static void HoistBlockDecls(stmt_block_t* block)
{
    metac_stmt_t* restHead = (metac_stmt_t*) emptyPointer;
    metac_stmt_t* restTail = 0;
    metac_stmt_t* declHead = (metac_stmt_t*) emptyPointer;
    metac_stmt_t* declTail = 0;
    metac_stmt_t* stmt = block->Body;
    uint32_t added = 0;

    while (!conv89_isEmptyNode(stmt))
    {
        metac_stmt_t* next = stmt->Next;

        if (stmt->Kind == stmt_decl)
        {
            metac_decl_t* d = ((stmt_decl_t*) stmt)->Decl;
            bool split = false;

            if (!conv89_isEmptyNode(d) && d->Kind == decl_variable)
                split = CanSplitDecl((decl_variable_t*) d);

            if (split)
            {
                decl_variable_t* var = (decl_variable_t*) d;
                metac_expr_t* init = var->VarInitExpr;
                metac_stmt_t* assignStmt;

                var->VarInitExpr = (metac_expr_t*) emptyPointer;
                assignStmt = NewExprStmt(
                    NewAssignExpr(NewIdentifierExpr(var->VarIdentifier), init));
                assignStmt->Next = next;
                if (conv89_isEmptyNode(restHead))
                    restHead = restTail = assignStmt;
                else
                    restTail->Next = assignStmt, restTail = assignStmt;
                added++;
            }

            /* move the (possibly stripped) declaration to the front chain */
            stmt->Next = (metac_stmt_t*) emptyPointer;
            if (conv89_isEmptyNode(declHead))
                declHead = declTail = stmt;
            else
                declTail->Next = stmt, declTail = stmt;
        }
        else
        {
            stmt->Next = (metac_stmt_t*) emptyPointer;
            if (conv89_isEmptyNode(restHead))
                restHead = restTail = stmt;
            else
                restTail->Next = stmt, restTail = stmt;
        }

        stmt = next;
    }

    if (!conv89_isEmptyNode(declHead))
    {
        if (declTail)
            declTail->Next = restHead;
        block->Body = declHead;
        conv89_declsHoisted++;
    }
    else
    {
        block->Body = restHead;
    }

    block->StmtCount += added;
}

static void TransformBlock(stmt_block_t* block)
{
    metac_stmt_t** p;

    if (conv89_isEmptyNode(block->Body))
        return;

    /* first rewrite nested constructs; a for-with-decl-init may insert
       a decl statement before itself through *p */
    p = &block->Body;
    while (!conv89_isEmptyNode(*p))
    {
        TransformStmt(p, true);
        p = &(*p)->Next;
    }

    /* then push this block's own declarations to the top */
    HoistBlockDecls(block);
}

/* hoist the C99 declaration out of a for-initializer.
   link points at the statement referencing the for loop. */
static void TransformForDeclInit(stmt_for_t* for_, metac_stmt_t** link, bool canInsertDecl)
{
    metac_decl_t* d = (metac_decl_t*) for_->ForInit;
    decl_variable_t* var;
    metac_stmt_t* declStmt;
    metac_expr_t* init;

    if (conv89_isEmptyNode(d) || d->Kind != decl_variable)
        return;

    var = (decl_variable_t*) d;
    init = var->VarInitExpr;

    declStmt = NewDeclStmt(d);

    if (conv89_isEmptyNode(init))
    {
        /* for (int i; ...) -> int i; for (; ...) */
        for_->ForInit = (metac_node_t) emptyPointer;
    }
    else if (init->Kind == expr_tuple)
    {
        /* for (struct S x = {1, 2}; ...) -> struct S x = {1, 2}; for (; ...) */
        for_->ForInit = (metac_node_t) emptyPointer;
    }
    else if (init->Kind == expr_comma)
    {
        /* multi declarator misparse: keep the whole printed form
           `int j = 1 , k = 2;` as the hoisted declaration and repeat the
           comma expression as the loop initializer */
        for_->ForInit = (metac_node_t) NewAssignExpr(NewIdentifierExpr(var->VarIdentifier), init);
    }
    else
    {
        /* for (int i = f(); ...) -> int i; for (i = f(); ...) */
        var->VarInitExpr = (metac_expr_t*) emptyPointer;
        for_->ForInit = (metac_node_t) NewAssignExpr(NewIdentifierExpr(var->VarIdentifier), init);
    }

    conv89_forHoisted++;

    if (canInsertDecl)
    {
        declStmt->Next = *link;
        *link = declStmt;
    }
    else
    {
        /* no statement list to insert into (unbraced if/while/for body,
           case chain, ...): wrap declaration and loop in a fresh block */
        declStmt->Next = (metac_stmt_t*) for_;
        *link = NewBlockStmt(declStmt);
    }
}

static void TransformStmt(metac_stmt_t** link, bool canInsertDecl)
{
    metac_stmt_t* stmt = *link;

    if (conv89_isEmptyNode(stmt))
        return;

    switch (stmt->Kind)
    {
        case stmt_block:
        {
            TransformBlock((stmt_block_t*) stmt);
        } break;

        case stmt_for:
        {
            stmt_for_t* for_ = (stmt_for_t*) stmt;

            if (!conv89_isEmptyNode(for_->ForBody))
                TransformStmt((metac_stmt_t**) &for_->ForBody, false);

            if (!conv89_isEmptyNode(for_->ForInit)
             && !IsExprNode(for_->ForInit->Kind))
            {
                TransformForDeclInit(for_, link, canInsertDecl);
            }
        } break;

        case stmt_if:
        {
            stmt_if_t* if_ = (stmt_if_t*) stmt;
            if (!conv89_isEmptyNode(if_->IfBody))
                TransformStmt((metac_stmt_t**) &if_->IfBody, false);
            if (!conv89_isEmptyNode(if_->ElseBody))
                TransformStmt((metac_stmt_t**) &if_->ElseBody, false);
        } break;

        case stmt_while:
        {
            stmt_while_t* while_ = (stmt_while_t*) stmt;
            if (!conv89_isEmptyNode(while_->WhileBody))
                TransformStmt((metac_stmt_t**) &while_->WhileBody, false);
        } break;

        case stmt_do_while:
        {
            stmt_do_while_t* doWhile = (stmt_do_while_t*) stmt;
            if (!conv89_isEmptyNode(doWhile->DoWhileBody))
                TransformStmt((metac_stmt_t**) &doWhile->DoWhileBody, false);
        } break;

        case stmt_switch:
        {
            stmt_switch_t* switch_ = (stmt_switch_t*) stmt;
            if (!conv89_isEmptyNode(switch_->SwitchBody))
                TransformBlock(switch_->SwitchBody);
        } break;

        case stmt_case:
        {
            stmt_case_t* case_ = (stmt_case_t*) stmt;
            metac_stmt_t** p = &case_->CaseBody;
            while (!conv89_isEmptyNode(*p))
            {
                TransformStmt(p, false);
                p = &(*p)->Next;
            }
        } break;

        case stmt_run:
        {
            stmt_run_t* run_ = (stmt_run_t*) stmt;
            if (!conv89_isEmptyNode(run_->RunBody))
                TransformStmt((metac_stmt_t**) &run_->RunBody, false);
        } break;

        default:
            /* expressions, return, break, continue, goto, labels, comments... */
            break;
    }
}

static void TransformDecl(metac_decl_t* decl)
{
    if (conv89_isEmptyNode(decl))
        return;

    if (decl->Kind == decl_function)
    {
        decl_function_t* func = (decl_function_t*) decl;
        if (!conv89_isEmptyNode(func->FunctionBody))
            TransformBlock(func->FunctionBody);
    }
}

int main(int argc, const char* argv[])
{
    metac_lpp_t lpp;
    metac_alloc_t alloc;
    int argIdx;

    if (argc < 2)
    {
        printf("Usage: conv89 <file.c> [more.c ...]\n");
        printf("Rewrites C99 for-loop declarations and mid-block declarations\n");
        printf("in a C89 friendly way, the result is printed to stdout.\n");
        return -1;
    }

    for (argIdx = 1; argIdx < argc; argIdx++)
    {
        decl_array_t decls;
        uint32_t i;

        Allocator_Init(&alloc, 0);
        MetaCLPP_Init(&lpp, &alloc, 0);

        decls = ReadLexParse(argv[argIdx], &lpp, 0);

        for (i = 0; i < decls.Length; i++)
        {
            metac_decl_t* decl = decls.Ptr[i];
            if (!decl || decl == (metac_decl_t*) emptyNode)
                continue;
            TransformDecl(decl);
        }

        for (i = 0; i < decls.Length; i++)
        {
            metac_decl_t* decl = decls.Ptr[i];
            if (!decl || decl == (metac_decl_t*) emptyNode)
                continue;

            MetaCPrinter_PrintDecl(&lpp.Parser.DebugPrinter, decl);
            printf("%.*s",
                   (int) lpp.Parser.DebugPrinter.StringMemoryCount,
                   lpp.Parser.DebugPrinter.StringMemory);
            MetaCPrinter_Reset(&lpp.Parser.DebugPrinter);
        }
    }

    fprintf(stderr, "conv89: %u for-loop declarations hoisted, %u blocks had declarations hoisted\n",
            (unsigned) conv89_forHoisted, (unsigned) conv89_declsHoisted);

    return 0;
}
