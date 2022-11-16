#include "../repl/completion_trie.h"

void CompletionTrie_Init(completion_trie_root_t* self, metac_alloc_t* parentAlloc)
{
    Allocator_Init(&self->TrieAllocator, parentAlloc);

    ARENA_ARRAY_INIT_SZ(completion_trie_node_t, self->Nodes, &self->TrieAllocator, 512)
    self->NodesCount = 4;

    self->Nodes[0].ChildCount = 0;
    self->Nodes[0].ChildrenBaseIdx = 0;
    self->Nodes[0].Prefix4[0] = 0;

    self->WordCount = 0;
    self->TotalNodes = 1;
}

static int PrefixLen(char prefix4[4])
{
         if (prefix4[0] == '\0')
        return 0;
    else if (prefix4[1] == '\0')
        return 1;
    else if (prefix4[2] == '\0')
        return 2;
    else if (prefix4[3] == '\0')
        return 3;
    else
        return 4;
}


completion_trie_node_t* CompletionTrie_FindLongestMatchingPrefix(completion_trie_root_t* root,
                                                                 const char* word,
                                                                 uint32_t* lengthP)
{
    const completion_trie_node_t const * nodes = root->Nodes;
    const completion_trie_node_t* current = root->Nodes + 0;
    uint32_t length = *lengthP;

    for(;;)
    {
        char c = word[0];
        assert(PrefixLen(nodes->Prefix4) == 0);

        const uint32_t childNodeIdx = (current->ChildrenBaseIdx * 4);
        const uint32_t lastChildNodeIdx =
            (current->ChildrenBaseIdx * 4) + current->ChildCount;

        int bestPrefixLength = 0;
        int bestChild = 0;

        for(uint32_t i = childNodeIdx; i < lastChildNodeIdx; i++)
        {
            if (c == nodes[i].Prefix4[0])
            {
                int len = PrefixLen(nodes[i].Prefix4);
                if (len > length)
                    continue;

                if (memcmp(nodes[i].Prefix4 + 1, word + 1, len - 1) == 0)
                {
                    if (bestPrefixLength < len)
                    {
                        bestChild = i;
                        bestPrefixLength = len;
                    }
                }
            }
        }

        if (bestChild == 0)
        {
            (*lengthP) = length;
            return current;
        }
        else
        {
            current = nodes + bestChild;
            length -= bestPrefixLength;
            word += bestPrefixLength;
        }
    }

    assert(0);
    return 0;
}

// CompletionTrie_AllocateChild

void CompletionTrie_AddChild(completion_trie_root_t* root, completion_trie_node_t* PrefNode,
                             const char* word, uint32_t length)
{
    completion_trie_node_t* child = 0;

    if (root->Nodes == PrefNode)
    {
        // printf("Starting at root\n");
    }

    printf("Going to add %.*s\n", (int)length, word);
    INC(root->WordCount);

    while(length)
    {
        if (PrefNode->ChildCount == 0)
        {
            assert(PrefNode->ChildrenBaseIdx == 0);
            ARENA_ARRAY_ENSURE_SIZE(root->Nodes, root->NodesCount + 4);
            PrefNode->ChildrenBaseIdx = (POST_ADD(root->NodesCount, 4)) / 4;
        }

        uint32_t newChildCount = INC(PrefNode->ChildCount) + 1;
        printf("newChildCount: %u -- (newChildCount & 3): %u\n", newChildCount,
        (newChildCount & 3));
        if ((newChildCount & 3) == 0)
        {
            uint32_t oldChildBaseIdx = PrefNode->ChildrenBaseIdx;
            uint32_t newChildBaseIdx;
            uint32_t newChildCapacity = newChildCount + 3;
            const uint32_t endI = newChildCount - 1;
            ARENA_ARRAY_ENSURE_SIZE(root->Nodes, root->NodesCount + newChildCapacity);
            newChildBaseIdx = POST_ADD(root->NodesCount, newChildCapacity) / 4;
            memcpy(root->Nodes + (newChildBaseIdx * 4),
                   root->Nodes + (oldChildBaseIdx * 4),
                   sizeof(*root->Nodes) * (newChildCount - 1));
            PrefNode->ChildrenBaseIdx = newChildBaseIdx;
        }

        child = root->Nodes + ((PrefNode->ChildrenBaseIdx * 4) + newChildCount - 1);
        {
            // printf("newChild at node: %u\n", child - root->Nodes);
            INC(root->TotalNodes);

            uint32_t copyLen = 4;
            if (length < 4)
            {
                copyLen = length;
                child->Prefix4[length] = '\0';
            }
            memcpy(child->Prefix4, word, copyLen);
            child->ChildrenBaseIdx = 0;
            child->ChildCount = 0;
            length -= copyLen;
            word += copyLen;
            PrefNode = child;
        }
    }

    assert(root->Nodes->ChildCount != 0);
}

void CompletionTrie_Print(completion_trie_root_t* root)
{
    const completion_trie_node_t const * nodes =
        root->Nodes;
    uint32_t i;

    uint32_t childIdxBegin = nodes[0].ChildrenBaseIdx * 4;
    uint32_t childIdxEnd = childIdxBegin + nodes[0].ChildCount;

    for(i = childIdxBegin; i < childIdxEnd; i++)
    {
        printf("Prefix4: %.4s\n", nodes[i].Prefix4);
    }
}

void CompletionTrie_Add(completion_trie_root_t* root, const char* word, uint32_t length)
{
    uint32_t remaning_length = length;
    completion_trie_node_t* PrefNode =
        CompletionTrie_FindLongestMatchingPrefix(root, word, &remaning_length);

    if (remaning_length)
    {
        int posWord = length - remaning_length;
        CompletionTrie_AddChild(root, PrefNode, word + posWord, remaning_length);
    }

/*
#define MINIMUM(A, B) \
    (((A) < (B)) ? (A) : (B))

    completion_trie_node_t* trie = root->Nodes;

    bool found = false;
    uint32_t pInWord = 0;
    char c = word[0];

    for(;(pInWord < length) && trie->ChildCount != 0;)
    {
        uint32_t i;
        uint32_t endI = trie->ChildrenBaseIdx + trie->ChildCount;
        for(i = trie->ChildrenBaseIdx; i < endI; i++)
        {
            uint32_t bestMatch = 0;
            if (root->Nodes[i].Prefix4[0] == c)
            {
                uint32_t j;
                uint32_t jEnd = MINIMUM(3, length - pInWord - 1);
                for(j = 1; i < jEnd; j++)
                {
                    if (word[pInWord + j] != root->Nodes[i].Prefix4[j])
                    {
                        uint32_t lastMatchJ = ((bestMatch & 0xFFFF) >> 1);
                        if (lastMatchJ < j)
                        {
                            bestMatch = (i << 16 | j << 1 | 0);
                        }
                        else if (lastMatchJ == j)
                        {
                            bestMatch |= 1;
                        }
                        goto LconinueChildren;
                    }
                }
                // if we get here we have found a child with matching prefix
                {
                    pInWord +=
                        PrefixLen(root->Nodes[i].Prefix4);
                    trie = root->Nodes + i;
                    i = 0;
                }
            }
        LconinueChildren: continue;
        }
        //
    }
    // when we end up here trie should be the place where we insert ourselfs
    // in the case where tire doesn't have payload we can write ourselfs into it
    if (trie->Prefix4[0] == '\0')
    {
        int32_t lenToWrite = (int32_t)(length - pInWord);

        for(;lenToWrite >= 4; lenToWrite -= 4)
        {
            trie->Prefix4[0] = word[pInWord + 0];
            trie->Prefix4[1] = word[pInWord + 1];
            trie->Prefix4[2] = word[pInWord + 2];
            trie->Prefix4[3] = word[pInWord + 3];
            // if ()
        }

        if(lenToWrite > 4)
        {

        }
    }
    else
    {

    }
#undef MINIMUM
*/
}

void AddIdentifierToCompletionSet(const char* idStr, uint32_t idKey, void* ctx)
{
    completion_trie_node_t* Trie = (completion_trie_node_t*) ctx;

    uint32_t length = LENGTH_FROM_IDENTIFIER_KEY(idKey);
}

void testCompletionTrie(void)
{
    metac_alloc_t rootAlloc;
    Allocator_Init(&rootAlloc, 0);

    completion_trie_root_t root;

    CompletionTrie_Init(&root, &rootAlloc);
}

void CompletionTrie_PrintStats(completion_trie_root_t* self)
{
    printf("UsedNodes: %u\n", self->TotalNodes);
    printf("AllocatedNodes: %u\n", self->NodesCount);
    printf("Words: %u\n", self->WordCount);
    printf("UsedNodesPerWord: %g\n", (float)self->TotalNodes / (float)self->WordCount);
    printf("AllocatedNodesPerWord: %g\n", (float)self->NodesCount / (float)self->WordCount);
}
