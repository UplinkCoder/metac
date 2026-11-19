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

/*
    A node of the trie.

    The children of a node are a singly linked list.  The nodes are
    stored in an array that grows without ever moving the nodes that are
    already in it, so the index of a node stays valid for the lifetime
    of the trie.  Storing the children in a contiguous block instead
    would not work: growing that block copies the children over and
    with them their indices change.
*/
typedef struct completion_trie_node_t {
    char Prefix4[4];
    uint16_t ChildCount;
    uint16_t Reserved;
    /// index of the first child
    uint32_t FirstChild;
    /// index of the last child, so that appending is cheap
    uint32_t LastChild;
    /// index of the next sibling
    uint32_t NextSibling;
    // below are debug field which should be removed asap
    node_range_t* Range;
} completion_trie_node_t;

typedef void (*collect_cb_t) (const char* completionString, uint32_t length, void* ctx);

#define MAX_TRIE_DEPTH 64

bool CompletionTrie_NoPreallocatedCIdentifierChars = false;

typedef struct completion_trie_root_t {
    ARENA_CHUNK_ARRAY(completion_trie_node_t, Nodes)
#if TRACK_RANGES
    ARENA_CHUNK_ARRAY(node_range_t, NodeRanges)
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

void CompletionTrie_PrintStats (completion_trie_root_t* root, uint16_t n);

#endif
