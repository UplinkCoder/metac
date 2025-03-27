#include <assert.h>
#include <stdio.h>
#include <string.h>

#if defined(TEST_MAIN)
#  include "../os/os.c"
#  include "../os/metac_alloc.c"
#endif

#include "../os/metac_atomic.h"

#define TRACK_RANGES 0
#include "../repl/completion_trie.h"

#define BASE_IDX_SCALE 1

#define MAXIMUM(a,b) (((a)>(b)) ? (a) : (b))

static inline void InitializeCIdentifierCharacters(completion_trie_root_t* root) {
    static const char cIdentifierChars[] =
        "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_";
    const size_t charCount = sizeof(cIdentifierChars) - 1;

    assert(root->RootCapacity > charCount);
    ARENA_ARRAY_ENSURE_SIZE(root->Nodes, charCount + 1);

    for (size_t i = 0; i < charCount; i++)
    {
        completion_trie_node_t child = {0};

        child.Prefix4[0] = cIdentifierChars[i];
        child.Prefix4[1] = '\0'; // Terminate the string
        child.ChildrenBaseIdx = 0; // it seems like this has to be 0 not i + 1;
        child.ChildCount = 0;
        INC(root->Nodes[0].ChildCount);
        ARENA_ARRAY_ADD(root->Nodes, child);
        root->TotalNodes++;
    }
}

void CompletionTrie_Init(completion_trie_root_t* self, metac_alloc_t* parentAlloc)
{
    Allocator_Init(&self->TrieAllocator, parentAlloc);

    ARENA_ARRAY_INIT_SZ(completion_trie_node_t, self->Nodes, &self->TrieAllocator, 786);
#if TRACK_RANGES
    ARENA_ARRAY_INIT_SZ(node_range_t, self->NodeRanges, parentAlloc, 512);
#endif
    self->NodesCount = 64;

    self->Nodes[0].ChildCount = 0;
    self->Nodes[0].ChildrenBaseIdx = 1;
    self->Nodes[0].Prefix4[0] = 0;
    self->RootCapacity =
        self->NodesCount -
        self->Nodes[0].ChildrenBaseIdx * BASE_IDX_SCALE;

    self->NodesCount = 1;
    // reserve the first node!

    self->WordCount = 0;
    self->TotalNodes = 1;
    if (!CompletionTrie_NoPreallocatedCIdentifierChars)
    {
        InitializeCIdentifierCharacters(self);
    }
#if TRACK_RANGES
    {
        node_range_t range = {0, self->NodesCount, 1};
        ARENA_ARRAY_ADD(self->NodeRanges, range);
    }
#endif
}

static int PrefixLen(const char prefix4[])
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


// calls the provided callback with all the completion suggestions
// please note: the string passed into the callback will not be valid after the callback has returned
void CompletionTrie_Collect(completion_trie_root_t* root,
                            uint32_t startNodeIdx,
                            const char* prefix, uint32_t matchedUntil,
                            void (*collectCb) (_scope const char* completionString, uint32_t length, void* ctx),
                            void* userCtx)
{
    uint32_t parentStack[MAX_TRIE_DEPTH];
    uint16_t childIndexStack[MAX_TRIE_DEPTH];
    uint16_t completionLengthAddStack[MAX_TRIE_DEPTH];
    uint8_t  matchedLengthStack[MAX_TRIE_DEPTH];

    completion_trie_node_t const * nodes = root->Nodes;
    const uint32_t completionLengthBase = matchedUntil - PrefixLen(nodes[startNodeIdx].Prefix4);

    char completionString[MAX_TRIE_DEPTH * 4];
    uint32_t completionLength = completionLengthBase;
    uint32_t currentNodeIdx = startNodeIdx;
    uint32_t currentChildIndex = 0;
    uint32_t currentStackIndex = 0;

    const uint32_t unmatchedPrefixLength = strlen(prefix) - matchedUntil;
    uint32_t currentUnmatchedPrefixLength = unmatchedPrefixLength;
    bool descend = true;

    memcpy(completionString, prefix, matchedUntil);

    for (;;) // until we are back at the root and there are no children left
LSetCurrent:
    {
        const completion_trie_node_t* current = nodes + currentNodeIdx;
        uint32_t childBeginIdx = current->ChildrenBaseIdx * BASE_IDX_SCALE;
        uint32_t childEndIdx = childBeginIdx + current->ChildCount;
        uint32_t prefixLen = PrefixLen(current->Prefix4);

        if (descend)
        {
            memcpy(completionString + completionLength, current->Prefix4, prefixLen);
            completionLength += prefixLen;
        }
        else
        {
            // if there are children left in the node we just ascended to, descend again
            if (currentChildIndex + childBeginIdx < childEndIdx)
            {
                descend = true;
                currentChildIndex++;
            }
        }

        for (uint32_t i = childBeginIdx + currentChildIndex; i < childEndIdx; i++)
        {
            const completion_trie_node_t* child = nodes + i;
            uint32_t checkLength = currentUnmatchedPrefixLength;

            if (currentUnmatchedPrefixLength)
            {
                uint32_t childPrefixLen = PrefixLen(child->Prefix4);
                if (childPrefixLen < checkLength)
                {
                    checkLength = childPrefixLen;
                }
                if (0 != memcmp(matchedUntil + prefix, child->Prefix4, checkLength))
                {
                    // prefix didn't match, keep going
                    continue;
                }
                currentUnmatchedPrefixLength -= checkLength;
                matchedUntil += checkLength;
                assert(currentUnmatchedPrefixLength >= 0 && currentUnmatchedPrefixLength <= 512);
            }

            // we are good to go
            {
                // if this node has no children then we have a completion
                if (child->ChildCount == 0)
                {
                    uint32_t toCopy = PrefixLen(child->Prefix4);
                    memcpy(completionString + completionLength, child->Prefix4, toCopy);
                    completionLength += toCopy;
                    collectCb(completionString, completionLength, userCtx);
                    completionLength -= toCopy;

                    if (currentNodeIdx == startNodeIdx)
                        currentUnmatchedPrefixLength = unmatchedPrefixLength;
                }
                else
                {
                    assert(descend);
                     // Push Completion stacks
                    {
                        parentStack[currentStackIndex] = currentNodeIdx;
                        childIndexStack[currentStackIndex] = i - childBeginIdx;
                        completionLengthAddStack[currentStackIndex] = completionLength - completionLengthBase;
                        matchedLengthStack[currentStackIndex] = checkLength;
                        ++currentStackIndex;
                    }
                   // change relevant iteration state
                    currentNodeIdx = i;
                    currentChildIndex = 0;
                    goto LSetCurrent;
                }
            }
        }
        // The for loop is over ... time to pop the state
        // but we must only pop if we are not descending
        {
            if (!currentStackIndex)
                break;
            --currentStackIndex;
            currentNodeIdx = parentStack[currentStackIndex];
            currentChildIndex = childIndexStack[currentStackIndex];
            completionLength = completionLengthBase + completionLengthAddStack[currentStackIndex];
            currentUnmatchedPrefixLength += matchedLengthStack[currentStackIndex];
            matchedUntil -= matchedLengthStack[currentStackIndex];
            descend = false;
        }
    }
}

completion_trie_node_t* CompletionTrie_FindLongestMatchingPrefix(completion_trie_root_t* root,
                                                                 const char* word,
                                                                 uint32_t* lengthP)
{
    completion_trie_node_t const * nodes = root->Nodes;
    const completion_trie_node_t* current = root->Nodes + 0;
    uint32_t length = *lengthP;

    for(;;)
    {
        char c = word[0];
        assert(PrefixLen(nodes[0].Prefix4) == 0);

        const uint32_t childNodeIdx = (current->ChildrenBaseIdx * BASE_IDX_SCALE);
        const uint32_t lastChildNodeIdx =
            (current->ChildrenBaseIdx * BASE_IDX_SCALE) + current->ChildCount;

        int bestPrefixLength = 0;
        int bestChild = 0;

        for(uint32_t i = childNodeIdx; i < lastChildNodeIdx; i++)
        {
            if (c == nodes[i].Prefix4[0])
            {
                int len = PrefixLen(nodes[i].Prefix4);
                if (len > length || !len)
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

        if (bestChild != 0)
        {
            current = nodes + bestChild;
            length -= bestPrefixLength;
            word += bestPrefixLength;
            continue;
        }
        break;
    }

    (*lengthP) = length;
    return (completion_trie_node_t*)current;
}

completion_trie_node_t*
CompletionTrie_AddChild(completion_trie_root_t* root,
                        completion_trie_node_t* parentNode,
                        const char* word, uint32_t length)
{
    completion_trie_node_t const * nodes = root->Nodes;
    completion_trie_node_t* child = 0;
    uint32_t newChildCount;
#if 1
    // this is true we are a child of the root
    if (root->Nodes == parentNode)
    {
        if (parentNode->ChildCount < root->RootCapacity)
        {
            child = (completion_trie_node_t*)
                nodes +
                (parentNode->ChildrenBaseIdx
               + INC(parentNode->ChildCount));
            goto LGotChild;
        }
    }
#endif
    // check for 0 length to insert a terminal node
    if (length == 0)
    {
        // check if the node already contains a terminal node
        const uint32_t baseIdx = parentNode->ChildrenBaseIdx * BASE_IDX_SCALE;
        const uint32_t endIdx = baseIdx + parentNode->ChildCount;
        for(uint32_t i = baseIdx; i < endIdx; i++)
        {
            if (nodes[i].Prefix4[0] == '\0')
                return (completion_trie_node_t*)nodes + i;
        }
        goto LinsertNode;
    }

    while(length)
    {
        if (parentNode->ChildCount == 0)
        {
            // CAS (PrevNode, 0) == 0
            assert(parentNode->ChildrenBaseIdx == 0 || parentNode == root->Nodes + 0);
            if (parentNode != root->Nodes + 0)
            {
                ARENA_ARRAY_ENSURE_SIZE(root->Nodes, root->NodesCount + BASE_IDX_SCALE);
                parentNode->ChildrenBaseIdx = (POST_ADD(root->NodesCount, BASE_IDX_SCALE)) / BASE_IDX_SCALE;
#if TRACK_RANGES
                {
                    uint32_t NodeRangestart = parentNode->ChildrenBaseIdx * BASE_IDX_SCALE;
                    node_range_t range = {NodeRangestart, NodeRangestart + BASE_IDX_SCALE, 1};
                    ARENA_ARRAY_ADD(root->NodeRanges, range);
                    parentNode->Range = root->NodeRanges + root->NodeRangesCount - 1;
                }
#endif
            }
            else
            {
                assert(!"root node grows via standart method .. shouln't happen");
            }
        }
LinsertNode: {}
        // need lock here.
        newChildCount = INC(parentNode->ChildCount) + 1;
        if ((newChildCount % BASE_IDX_SCALE) == 0)
        {
            uint32_t oldChildBaseIdx = parentNode->ChildrenBaseIdx;
            uint32_t newChildBaseIdx;
            uint32_t newChildCapacity = newChildCount + (BASE_IDX_SCALE - 1);
            const uint32_t endI = newChildCount - 1;
#if TRACK_RANGES
            parentNode->Range->IsValid = 0;
#endif
            ARENA_ARRAY_ENSURE_SIZE(root->Nodes, root->NodesCount + newChildCapacity);
            newChildBaseIdx = POST_ADD(root->NodesCount, newChildCapacity) / BASE_IDX_SCALE;
#if TRACK_RANGES
           {
                uint32_t NodeRangestart = newChildBaseIdx * BASE_IDX_SCALE;
                node_range_t range = {NodeRangestart, NodeRangestart + newChildCapacity, 1};
                ARENA_ARRAY_ADD(root->NodeRanges, range);
                parentNode->Range = root->NodeRanges + root->NodeRangesCount - 1;
            }
#endif
            memcpy(root->Nodes + (newChildBaseIdx * BASE_IDX_SCALE),
                   root->Nodes + (oldChildBaseIdx * BASE_IDX_SCALE),
                   sizeof(*root->Nodes) * (newChildCount - 1));
            parentNode->ChildrenBaseIdx = newChildBaseIdx;
        }

        child = root->Nodes + ((parentNode->ChildrenBaseIdx * BASE_IDX_SCALE) + newChildCount - 1);
LGotChild:
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
            parentNode = child;
        }
    }

    assert (root->Nodes[0].ChildCount != 0);
    if (child->Prefix4[0] == '\0')
    {
        INC(root->WordCount);
    }

    return child;
}

/**
 * Generates a DOT representation of the completion trie starting from the given node.
 * This function recursively traverses the trie and outputs DOT code for visualization.
 *
 * @param root        The root of the completion trie.
 * @param nodeIdx     The index of the current node in the trie.
 * @param rootPrefix  The prefix of the root node.
 * @param f           The file to which the DOT code will be written.
 */
void CompletionTrie_Print(completion_trie_root_t* root, uint32_t nodeIdx, const char* rootPrefix, FILE* f) {
    // Retrieve a pointer to the nodes array for easier access
    completion_trie_node_t const * nodes = root->Nodes;
    uint32_t childIdx;

    // Calculate the range of child indices for the current node
    uint32_t childIdxBegin = nodes[nodeIdx].ChildrenBaseIdx * BASE_IDX_SCALE;
    uint32_t childIdxEnd = childIdxBegin + nodes[nodeIdx].ChildCount;

    // Output DOT code for the edges between the current node and its children
    for(childIdx = childIdxBegin; childIdx < childIdxEnd; childIdx++)
    {
        // Output an edge from the current node to a child node
        fprintf(f, "  \"%d: %.4s\" -> \"%d: %.4s\"\n",
                nodeIdx, rootPrefix,
                childIdx, nodes[childIdx].Prefix4);
    }

    // Output DOT code to ensure that child nodes appear in the same rank
    fprintf(f, "{ rank = same; ");
    for(childIdx = childIdxBegin; childIdx < childIdxEnd; childIdx++)
    {
        // Output nodes in the same rank to maintain their alignment
        fprintf(f, "\"%d: %.4s\" ", childIdx, nodes[childIdx].Prefix4);
    }
    fprintf(f, "}\n");

    // Recursively call CompletionTrie_Print for child nodes with ChildCount
    for(childIdx = childIdxBegin; childIdx < childIdxEnd; childIdx++)
    {
        if (nodes[childIdx].ChildCount)
        {
            // Recursively traverse child nodes with ChildCount
            CompletionTrie_Print(root, childIdx, nodes[childIdx].Prefix4, f);
        }
    }
}

void CompletionTrie_Add(completion_trie_root_t* root, const char* word, uint32_t length)
{
    uint32_t remaining_length = length;
    completion_trie_node_t* parentNode =
        CompletionTrie_FindLongestMatchingPrefix(root, word, &remaining_length);

    if (remaining_length)
    {
        int posWord = length - remaining_length;
        parentNode = CompletionTrie_AddChild(root, parentNode, word + posWord, remaining_length);
    }
    // insert terminal node
    CompletionTrie_AddChild(root, parentNode, "", 0);
}

#if defined(TEST_MAIN)
typedef struct {
    const char** Strings;
    uint32_t nStrings;

    uint32_t nMatches;
} testCb_ctx_t;

void testCollect (_scope const char* completionString, uint32_t length, void* ctx)
{
    testCb_ctx_t* ctxP = (testCb_ctx_t*) ctx;
    const char** strings = ctxP->Strings;
    const uint32_t nStrings = ctxP->nStrings;

    for(uint32_t i = 0; i < nStrings; i++)
    {
        if (0 == strncmp(strings[i], completionString, length))
        {
            ctxP->nMatches++;
            break;
        }
    }
}

void testCompletionTrie(void)
{
    metac_alloc_t rootAlloc;
    Allocator_Init(&rootAlloc, 0);

    completion_trie_root_t root;

    CompletionTrie_Init(&root, &rootAlloc);

    CompletionTrie_Add(&root, "c", strlen("c"));
    CompletionTrie_PrintStats(&root, 1);
    CompletionTrie_Add(&root, "clot", strlen("clot"));
    CompletionTrie_PrintStats(&root, 2);
    CompletionTrie_Add(&root, "compiler", strlen("compiler"));
    CompletionTrie_PrintStats(&root, 3);
    CompletionTrie_Add(&root, "compilerP", strlen("compilerP"));
    CompletionTrie_PrintStats(&root, 4);

    testCb_ctx_t userCtx;

    {
        const char* strings[] = {"clot"};
        userCtx.Strings = strings;
        userCtx.nStrings = ARRAY_SIZE(strings);
        userCtx.nMatches = 0;

        CompletionTrie_Collect(&root, 0, "cl", 0, testCollect, &userCtx);
        assert(userCtx.nMatches == 1);
    }

    {
        const char* strings[] = {"compiler", "compilerP"};
        userCtx.Strings = strings;
        userCtx.nStrings = ARRAY_SIZE(strings);
        userCtx.nMatches = 0;
        uint32_t wordLength = 4;
        completion_trie_node_t* n =
            CompletionTrie_FindLongestMatchingPrefix(&root, "comp", &wordLength);

        CompletionTrie_Collect(&root, n - root.Nodes, "comp", strlen("comp") - wordLength, testCollect, &userCtx);
        assert(userCtx.nMatches == 2);
    }
}

#endif

#if TRACK_RANGES
void CompletionTrie_PrintRanges(completion_trie_root_t* self)
{
    FILE* f = fopen("ranges.txt", "w");
    const uint32_t nodeRangesCount =  self->NodeRangesCount;
    for(uint32_t i = 0; i < nodeRangesCount; i++)
    {
        node_range_t range = self->NodeRanges[i];
        fprintf(f, "%sRange:{%d, %d}\n",
                   (range.IsValid ? "" : "- "), range.Begin, range.End);
    }
    fclose(f);
}
#endif

void CompletionTrie_PrintTrie(completion_trie_root_t* self, uint32_t n)
{
        char fname[32] = "g.dot";
        if (n != 0)
        {
            snprintf(fname, sizeof(fname), "g%u.dot", n);
        }
        ALIGN_STACK();
        FILE* f = fopen(fname, "w");
        fprintf(f, "digraph G {\n");
        fprintf(f, "  node [shape=record headport=n]\n");

        CompletionTrie_Print(self, 0, "", f);

        fprintf(f, "}\n");
        fclose(f);
        RESTORE_STACK();
}

void CompletionTrie_PrintStats(completion_trie_root_t* self, uint32_t n)
{

    CompletionTrie_PrintTrie(self, n);

    printf("UsedNodes: %u\n", self->TotalNodes);
    printf("AllocatedNodes: %u\n", self->NodesCount);
    printf("Words: %u\n", self->WordCount);

    int AllocatedNodesPerWord = ((float)self->NodesCount / (float)self->WordCount) * 100.0f;
    int UsedNodesPerWord = ((float)self->TotalNodes / (float)self->WordCount) * 100.0f;

    printf("UsedNodesPerWord: %d.%d\n", UsedNodesPerWord / 100, UsedNodesPerWord % 100);
    printf("AllocatedNodesPerWord: %d.%d\n", AllocatedNodesPerWord / 100, AllocatedNodesPerWord % 100);
#if TRACK_RANGES
    CompletionTrie_PrintRanges(self);
#endif
}

#ifdef TEST_MAIN
    int main(int argc, const char* argv[])
    {
        CompletionTrie_NoPreallocatedCIdentifierChars = true;
        testCompletionTrie();
        CompletionTrie_NoPreallocatedCIdentifierChars = false;
    }
#endif

#undef MAXIMUM
#undef TRACK_RANGES
#undef BASE_IDX_SCALE
