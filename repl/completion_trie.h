#ifndef COMPLETION_TRIE_H
#define COMPLETION_TRIE_H

#include "../os/compat.h"
#include "../os/metac_alloc.h"

typedef struct node_range_t
{
    uint32_t Begin;
    uint32_t End;
    bool     IsValid;
} node_range_t;

typedef struct completion_trie_node_t {
    char Prefix4[4];
    uint16_t ChildCount;
    uint16_t ChildrenBaseIdx;
    // below are debug field which should be removed asap
    node_range_t* Range;
} completion_trie_node_t;

typedef void (*collect_cb_t) (const char* completionString, uint32_t length, void* ctx);

typedef struct completion_trie_root_t {
    ARENA_ARRAY(completion_trie_node_t, Nodes)
#if TRACK_RANGES
    ARENA_ARRAY(node_range_t, NodeRanges)
#endif
    // How many nodes are currently allocated for the root
    // The root get's trated specially ebcayse it has the most children typically
    // requireing us to use a diffrent growth strategy
    uint32_t RootCapacity;

    uint32_t WordCount;
    uint32_t TotalNodes;
    metac_alloc_t TrieAllocator;
} completion_trie_root_t;

void CompletionTrie_Init (completion_trie_root_t* self, metac_alloc_t* parentAlloc);

void CompletionTrie_Add (completion_trie_root_t* root, const char* word, uint32_t length);

void AddIdentifierToCompletionSet (const char* idStr, uint32_t idKey, void* ctx);

void CompletionTrie_PrintRanges(completion_trie_root_t* self);

void CompletionTrie_PrintStats (completion_trie_root_t* root);

#endif
