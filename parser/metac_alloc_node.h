#ifndef METAC_ALLOC_NODE_H
#define METAC_ALLOC_NODE_H

#include "../os/compat.h"
#include "metac_parsetree.h"
#include <assert.h>

/*
    Allocation of parse tree nodes.

    All nodes are carved out of chunked arenas which never move already
    allocated memory.  Pointers into the parse tree are therefore stable
    for the lifetime of the process (or until MetaCNodeAlloc_Free() is
    called, which requires that no semantic state referring to the parse
    tree is alive anymore).

    Nodes are zeroed on allocation and carry a unique Serial number.
*/

/*
    Serial counter shared by all node kinds of the compiler.

    The semantic layer allocates nodes as well and uses the same counter
    so that serials are unique across the whole compiler.  When the
    semantic layer is compiled stand alone (without the parser) it
    provides its own definition of this symbol.
*/
extern uint32_t g_metacNodeSerialCounter;

/// Allocate a zeroed expression node of the given kind.
metac_expr_t* AllocNewExpr(metac_expr_kind_t kind);

#define AllocNewDecl(KIND, RESULT_PTR) \
    (KIND ## _t*) AllocNewDecl_(KIND, sizeof(KIND ##_t), ((void**)(RESULT_PTR)), __LINE__)

/// Allocate a zeroed declaration node of exactly nodeSize bytes.
metac_decl_t* AllocNewDecl_(metac_decl_kind_t kind,
                            uint32_t nodeSize,
                            void** result_ptr,
                            uint32_t line);

#define AllocNewStmt(KIND, RESULT_PTR) \
    (KIND ##_t*) AllocNewStmt_(KIND, sizeof(KIND ##_t), ((void**)(RESULT_PTR)))

/// Allocate a zeroed statement node of exactly nodeSize bytes.
metac_stmt_t* AllocNewStmt_(metac_stmt_kind_t kind,
                            uint32_t nodeSize,
                            void** result_ptr);

/// Statistics about the node arenas, mostly useful for tests and for
/// watching the memory behaviour of the compiler.
typedef struct metac_node_alloc_stats_t
{
    uint32_t ExprBytes;
    uint32_t DeclBytes;
    uint32_t StmtBytes;
    uint32_t TotalBytes;

    uint32_t ExprChunks;
    uint32_t DeclChunks;
    uint32_t StmtChunks;

    uint32_t NodeCount;
} metac_node_alloc_stats_t;

metac_node_alloc_stats_t MetaCNodeAlloc_Stats(void);

/*
    Release all node arenas.

    WARNING: every parse tree node becomes invalid, and so does every
    pointer into the parse tree - including the ones the semantic layer
    keeps (metac_sema_expr_t::UnkownValueOrigin and friends).  Only call
    this once the semantic state that refers to the parse tree is gone.
*/
void MetaCNodeAlloc_Free(void);

#endif
