// Currently unfinished WIP dump.
#include "../os/compat.h"
#include "metac_sematree.h"

/*
  2^7−1=127
  2^13−1=8191
  2^17−1=131071
  2^19−1=524287
*/
typedef struct metac_dfs_entry_t
{
    metac_sema_stmt_t* Parent;
} metac_dfs_entry_t;


typedef struct metac_lvn_table_slot_t
{
    metac_node_t Node;
    uint32_t lvn;
} metac_lvn_table_slot_t;

typedef struct metac_lvn_table_t
{
    metac_lvn_table_slot_t* Slots;

    uint32_t SlotCount_Log2;
    uint32_t SlotsUsed;

    uint32_t LengthShift;
    uint32_t MaxDisplacement;
} metac_lvn_table_t; 

#define SLOT_FROM_HASH(HASH, COLLIDER, MASK) \
    (((HASH) ^ (COLLIDER)) & (MASK))

metac_lvn_table_t* MetaCLVNTable_Init(metac_alloc_t* alloc)
{
#define INITIAL_LVN_SLOT_LOG2 7
#define INITIAL_LVN_SLOT_COUNT ((1 << INITIAL_LVN_SLOT_LOG2) - 1)
    const uint32_t initialSize = ALIGN16(sizeof(metac_lvn_table_t))
                               + ALIGN16(sizeof(metac_lvn_table_slot_t) * INITIAL_LVN_SLOT_COUNT);
    metac_lvn_table_t* table = 0;
    arena_ptr_t arenaPtr = AllocateArena(alloc, initialSize);
    if (IsValidArenaPtr(arenaPtr))
    {
        table = cast(metac_lvn_table_t*)
                                   alloc->Arenas[arenaPtr.Index]->Memory;
        metac_lvn_table_slot_t* slots = 
            cast(metac_lvn_table_slot_t*)((cast(char*)table) + ALIGN16(sizeof(table)));
        table->SlotCount_Log2 = INITIAL_LVN_SLOT_LOG2;
    }
    return table;
#undef INITIAL_LVN_SLOT_COUNT
#undef INITIAL_LVN_SLOT_LOG2
}

void MetaCLVNTable_Grow(metac_lvn_table_t* table, uint32_t newSize)
{
    assert(newSize >= (1 << table->SlotCount_Log2));
    
}

uint32_t MetaCLVNTable_Lookup(metac_lvn_table_t* table, metac_node_t node)
{
    const uint32_t mask = (1 << table->SlotCount_Log2) - 1;
    const uint32_t hash = node->Hash;
    memset(self->visited, 0, (1 << table->SlotCount_Log2) >> 3);
    
    uint32_t slotIndex =  SLOT_FROM_HASH(hash, 0, mask);
    // we need the previous slotIndex such that we can robin-hood
    uint32_t previousSlotIndex = slotIndex;
    uint32_t probeCount = 0;
    // only search through a quater of the table at most
    while(probeCount > (mask >> 2))
    {
        metac_lvn_table_slot_t* slot = &table->Slots[slotIndex];
        
    }
}

typedef struct metac_lvn_ctx_t
{
    uint32_t CurrentLVN;
    ARENA_ARRAY(metac_dfs_entry_t, ScopeStack)
} metac_lvn_ctx_t;


#ifndef emptyNode
#define emptyNode \
    ((metac_node_t) 0x1)
#endif
/*
#define NodeGetHash(NODE) ((NODE)->Hash)

DEFINE_TABLE(MetaCLvnTable, metac_node, NodeGetHash, uint32_t, 0)

 
 * */
static inline void PushBranch(metac_lvn_ctx_t* ctx, metac_sema_stmt_t* parent)
{
    metac_dfs_entry_t entry;
    ARENA_ARRAY_ADD(ctx->ScopeStack, entry);
    
}

static inline void PopBranch(metac_lvn_ctx_t* ctx, metac_sema_stmt_t* parent)
{
    metac_dfs_entry_t* top = (ctx->ScopeStack + (ctx->ScopeStackCount - 1));
    // assert(top->Parent == parent);
}

static inline bool IsBranchingStmt(metac_sema_stmt_t* stmt)
{
    switch(stmt->Kind)
    {
        case stmt_if:
        case stmt_switch:
        case stmt_while:
        case stmt_do_while:
        case stmt_for:
            return true;

        default:
            return false;
    }
}

static inline uint32_t MetaCLVN_GetLvn(metac_lvn_ctx_t* self, metac_node_t node)
{
    uint32_t lvn = MetaCLvnTable_Lookup(node);
    return lvn;
}

int MetaCDoLVNWalk(metac_node_t node, void* _ctx)
{
    metac_lvn_ctx_t* ctx = cast(metac_lvn_ctx_t*) _ctx;
    if (MetaCLVN_GetLvn(node) != 0)
    {
        return 1;
    }
    
    switch(node->Kind)
    {
        case expr_add:
        case expr_sub:
        case expr_mul:
        case expr_div:
        case expr_and:
        case expr_andand:
        case expr_or:
        case expr_oror:
        case expr_xor:
            break;
    }
}

metac_lvn_table_t MetaCDoLVN(sema_decl_function_t* func, metac_sema_state_t* sema)
{
    metac_lvn_ctx_t ctx;
    
    if (METAC_NODE(func->FunctionBody) != emptyNode)
    {
        uint32_t stmtIdx;
        for(stmtIdx = 0; stmtIdx < func->FunctionBody->StmtCount; stmtIdx++)
        {
            metac_sema_stmt_t* stmt = &func->FunctionBody->Body[stmtIdx];
            if (IsBranchingStmt(stmt))
            {
                PushBranch(&ctx, stmt);
                MetaCSemaTree_Walk(METAC_NODE(stmt), sema, MetaCDoLVNWalk, &ctx);
                PopBranch(&ctx, stmt);
            }
        }
    }
}
