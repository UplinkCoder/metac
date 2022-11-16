#ifndef COMPLETION_TRIE_H
#define COMPLETION_TRIE_H

#include "../os/compat.h"
#include "../os/metac_alloc.h"

typedef struct completion_trie_node_t {
    char Prefix4[4];
    uint16_t ChildCount;
    uint16_t ChildrenBaseIdx;
} completion_trie_node_t;

typedef struct completion_trie_root_t {
    ARENA_ARRAY(completion_trie_node_t, Nodes)

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

void CompletionTrie_PrintStats (completion_trie_root_t* root);

#endif
