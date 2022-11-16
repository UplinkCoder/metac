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
    ARENA_ARRAY(char, StringMemory)
    
    uint32_t WordCount;
    metac_alloc_t TrieAllocator;
} completion_trie_root_t;

void CompletionTrie_Init (completion_trie_root_t* self, metac_alloc_t* parentAlloc);

void CompletionTrie_Add (completion_trie_root_t* root, const char* word, uint32_t length);

void AddIdentifierToCompletionSet (const char* idStr, uint32_t idKey, void* ctx);

#endif