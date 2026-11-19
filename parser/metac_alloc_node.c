/*
    Node allocation for the parser.

    All parse tree nodes live in chunked arenas.  The arenas never move
    already-allocated memory, therefore every pointer into the parse tree
    stays valid for the lifetime of the arena.

    The previous implementation used three global arrays which were grown
    with realloc().  realloc() is free to move the block, which silently
    invalidated *every* outstanding pointer into the parse tree as soon as
    one of the arrays grew.  Since the parse tree is built out of raw
    pointers (metac_expr_t*, metac_decl_t*, metac_stmt_t*, ...) that bug
    corrupts the tree in ways that are almost impossible to diagnose.
    See os/metac_arena.h for the arena's guarantees.

    The node allocator also no longer over-allocates: previously every
    declaration occupied a full metac_decl_t slot (76 bytes) even when the
    concrete declaration was only a decl_type_t (32 bytes).  Nodes are now
    sized exactly, which additionally makes them properly aligned - the
    old 76 byte stride left every second declaration only 4 byte aligned.
*/

#include "../os/metac_atomic.h"
#include "metac_alloc_node.h"
#include "../os/metac_arena.c"
#include <string.h>
#include <stdlib.h>
#include <stdio.h>

#ifndef EMPTY_POINTER_VALUE
#  define EMPTY_POINTER_VALUE (0x1)
#endif

/*
    The smallest legal node sizes.  Every declaration carries a
    DECL_HEADER and every statement carries a STMT_HEADER, so an
    allocation smaller than this would make the header writes below
    overflow the allocation.
*/
typedef struct metac_decl_header_size_t
{
    DECL_HEADER
} metac_decl_header_size_t;

typedef struct metac_stmt_header_size_t
{
    STMT_HEADER
} metac_stmt_header_size_t;

#define METAC_MIN_DECL_SIZE ((uint32_t)sizeof(metac_decl_header_size_t))
#define METAC_MIN_STMT_SIZE ((uint32_t)sizeof(metac_stmt_header_size_t))

/// Size of the first chunk of each node arena.
#define METAC_NODE_ARENA_CHUNK_SIZE (256 * 1024)

/*
    The arenas are file scope statics, so they live as long as the
    process does.  This mirrors the previous behaviour (the pools were
    static as well and were never freed) and it means that the parse tree
    stays valid for as long as anything refers to it.

    MetaCNodeAlloc_Free() exists for the cases where the whole parse tree
    is known to be dead - but note that the semantic layer keeps pointers
    into the parse tree (e.g. metac_sema_expr_t::UnkownValueOrigin), so
    the semantic state has to be torn down first.
*/
static metac_arena_t g_exprArena = {
    0,
    METAC_NODE_ARENA_CHUNK_SIZE,
    METAC_ARENA_MAX_CHUNK_SIZE,
    0,
    0,
    0
};

static metac_arena_t g_declArena = {
    0,
    METAC_NODE_ARENA_CHUNK_SIZE,
    METAC_ARENA_MAX_CHUNK_SIZE,
    0,
    0,
    0
};

static metac_arena_t g_stmtArena = {
    0,
    METAC_NODE_ARENA_CHUNK_SIZE,
    METAC_ARENA_MAX_CHUNK_SIZE,
    0,
    0,
    0
};

uint32_t g_metacNodeSerialCounter = 1;

metac_expr_t* AllocNewExpr(metac_expr_kind_t kind)
{
    metac_expr_t* result = (metac_expr_t*)Arena_Alloc(&g_exprArena,
                                                       sizeof(metac_expr_t));

    if (!result)
    {
        fprintf(stderr, "Out of memory while allocating an expression node\n");
        abort();
    }

    memset(result, 0, sizeof(metac_expr_t));

    result->Kind = kind;
    result->Serial = INC(g_metacNodeSerialCounter);

    return result;
}

metac_decl_t* AllocNewDecl_(metac_decl_kind_t kind,
                            uint32_t nodeSize,
                            void** result_ptr,
                            uint32_t line)
{
    metac_decl_t* result = 0;

    assert(nodeSize >= METAC_MIN_DECL_SIZE);

    if (nodeSize < METAC_MIN_DECL_SIZE)
    {
        nodeSize = METAC_MIN_DECL_SIZE;
    }

    result = (metac_decl_t*)Arena_Alloc(&g_declArena, nodeSize);

    if (!result)
    {
        fprintf(stderr, "Out of memory while allocating a declaration node\n");
        abort();
    }

    memset(result, 0, nodeSize);

    result->Kind = kind;
    result->Serial = INC(g_metacNodeSerialCounter);

    (*result_ptr) = result;

    return result;
}

metac_stmt_t* AllocNewStmt_(metac_stmt_kind_t kind,
                            uint32_t nodeSize,
                            void** result_ptr)
{
    metac_stmt_t* result = 0;

    assert(nodeSize >= METAC_MIN_STMT_SIZE);

    if (nodeSize < METAC_MIN_STMT_SIZE)
    {
        nodeSize = METAC_MIN_STMT_SIZE;
    }

    result = (metac_stmt_t*)Arena_Alloc(&g_stmtArena, nodeSize);

    if (!result)
    {
        fprintf(stderr, "Out of memory while allocating a statement node\n");
        abort();
    }

    memset(result, 0, nodeSize);

    result->Kind = kind;
    result->Serial = INC(g_metacNodeSerialCounter);
    result->Next = (metac_stmt_t*)EMPTY_POINTER_VALUE;

    (*result_ptr) = result;

    return result;
}

void MetaCNodeAlloc_Free(void)
{
    Arena_Free(&g_exprArena);
    Arena_Free(&g_declArena);
    Arena_Free(&g_stmtArena);

    /*
        Note: the serial counter is deliberately *not* reset.  The
        semantic layer shares it and may still hold nodes, so restarting
        it would produce duplicate serials.
    */
}

metac_node_alloc_stats_t MetaCNodeAlloc_Stats(void)
{
    metac_node_alloc_stats_t result;

    result.ExprBytes = Arena_UsedBytes(&g_exprArena);
    result.DeclBytes = Arena_UsedBytes(&g_declArena);
    result.StmtBytes = Arena_UsedBytes(&g_stmtArena);

    result.ExprChunks = Arena_ChunkCount(&g_exprArena);
    result.DeclChunks = Arena_ChunkCount(&g_declArena);
    result.StmtChunks = Arena_ChunkCount(&g_stmtArena);

    result.TotalBytes = result.ExprBytes + result.DeclBytes + result.StmtBytes;
    result.NodeCount = g_metacNodeSerialCounter - 1;

    return result;
}
