#include <assert.h>
#include <stdio.h>
#include <string.h>

#if defined(COMP_TRIE_TEST_MAIN)
#  include "../os/os.c"
#  include "../os/metac_alloc.c"
#  include "../os/metac_arena.c"
#endif

#include "../os/metac_atomic.h"

#define TRACK_RANGES 0
#include "../repl/completion_trie.h"

#define BASE_IDX_SCALE 1

/*
    The index of a child that does not exist.
*/
#define COMPLETION_TRIE_NO_CHILD 0xFFFFFFFFu

static uint32_t CompletionTrie_NewNode(completion_trie_root_t* root);

static uint32_t CompletionTrie_AppendChild(completion_trie_root_t* root,
                                            uint32_t parentIdx);

static uint32_t CompletionTrie_FindChild(completion_trie_root_t* root,
                                         uint32_t parentIdx, char c);

#define MAXIMUM(a,b) (((a)>(b)) ? (a) : (b))

typedef struct completion_trie_viz_params_t
{
    uint16_t FrameNumber;
    uint32_t HighlightedNodeIdx;
} completion_trie_viz_params_t;

static inline void InitializeCIdentifierCharacters(completion_trie_root_t* root) {
    static const char cIdentifierChars[] =
        "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_";
    const size_t charCount = sizeof(cIdentifierChars) - 1;

    /*
        The children of the root are the characters an identifier may
        start with.
    */
    for (size_t i = 0; i < charCount; i++)
    {
        uint32_t childIdx = CompletionTrie_AppendChild(root, 0);
        completion_trie_node_t* child =
            ARENA_CHUNK_ARRAY_PTR(root->Nodes, childIdx);

        child->Prefix4[0] = cIdentifierChars[i];
        child->Prefix4[1] = '\0'; // Terminate the string
    }
}

void CompletionTrie_Init(completion_trie_root_t* self, metac_alloc_t* parentAlloc)
{
    Allocator_Init(&self->TrieAllocator, parentAlloc);

    ARENA_CHUNK_ARRAY_INIT_SZ(completion_trie_node_t, self->Nodes,
                              &self->TrieAllocator, 64);
#if TRACK_RANGES
    ARENA_CHUNK_ARRAY_INIT_SZ(node_range_t, self->NodeRanges, parentAlloc, 512);
#endif

    /* the root node is the first one */
    ARENA_CHUNK_ARRAY_RESERVE(self->Nodes, 1);
    {
        completion_trie_node_t* rootNode =
            ARENA_CHUNK_ARRAY_PTR(self->Nodes, 0);

        memset(rootNode, 0, sizeof(*rootNode));
        rootNode->FirstChild = COMPLETION_TRIE_NO_CHILD;
        rootNode->LastChild = COMPLETION_TRIE_NO_CHILD;
        rootNode->NextSibling = COMPLETION_TRIE_NO_CHILD;
    }

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
#if 1
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
#else
static int PrefixLen(const char prefix4[])
{
    uint32_t v = *(uint32_t*)prefix4;
    // __builtin_memcpy(&v, prefix4, 4);
    uint32_t zeroes = ((v - 0x01010101) & ~v & 0x80808080);
    if (zeroes == 0)
        return 4;
    else
        return BSF(zeros) >> 3;
}
#endif

// calls the provided callback with all the completion suggestions
// please note: the string passed into the callback will not be valid after the callback has returned
void CompletionTrie_Collect(completion_trie_root_t* root,
                            uint32_t startNodeIdx,
                            const char* prefix, uint32_t matchedUntil,
                            void (*collectCb) (_scope const char* completionString, uint32_t length, void* ctx),
                            void* userCtx)
{
    uint32_t parentStack[MAX_TRIE_DEPTH];
    uint32_t childIndexStack[MAX_TRIE_DEPTH];
    uint16_t completionLengthAddStack[MAX_TRIE_DEPTH];
    uint8_t  matchedLengthStack[MAX_TRIE_DEPTH];

    const uint32_t completionLengthBase =
        matchedUntil - PrefixLen(ARENA_CHUNK_ARRAY_AT(root->Nodes, startNodeIdx).Prefix4);

    char completionString[MAX_TRIE_DEPTH * 4];
    uint32_t completionLength = completionLengthBase;
    uint32_t currentNodeIdx = startNodeIdx;
    uint32_t currentChildIdx =
        ARENA_CHUNK_ARRAY_AT(root->Nodes, startNodeIdx).FirstChild;
    uint32_t currentStackIndex = 0;

    const uint32_t unmatchedPrefixLength = strlen(prefix) - matchedUntil;
    uint32_t currentUnmatchedPrefixLength = unmatchedPrefixLength;
    /*
        The prefix of the current node still has to be added to the
        completion string.  It is added when we move to a node and not
        when we come back to it, because the length of the completion is
        restored from the stack in that case.
    */
    bool prefixPending = true;

    memcpy(completionString, prefix, matchedUntil);

    for (;;) // until we are back at the root and there are no children left
LSetCurrent:
    {
        const completion_trie_node_t* current =
            ARENA_CHUNK_ARRAY_PTR(root->Nodes, currentNodeIdx);
        const uint32_t prefixLen = PrefixLen(current->Prefix4);

        if (prefixPending)
        {
            memcpy(completionString + completionLength, current->Prefix4, prefixLen);
            completionLength += prefixLen;
            prefixPending = false;
        }

        for (uint32_t i = currentChildIdx;
             i != COMPLETION_TRIE_NO_CHILD;
             i = ARENA_CHUNK_ARRAY_AT(root->Nodes, i).NextSibling)
        {
            const completion_trie_node_t* child =
                ARENA_CHUNK_ARRAY_PTR(root->Nodes, i);
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
                    /*
                        A terminal node carries no characters of its own, so
                        it cannot consume the rest of the prefix.  If the
                        prefix is not fully matched the word does not
                        start with it and must not be reported.
                    */
                    if (currentUnmatchedPrefixLength != 0)
                    {
                        continue;
                    }

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
                     // Push Completion stacks
                    {
                        parentStack[currentStackIndex] = currentNodeIdx;
                        childIndexStack[currentStackIndex] = child->NextSibling;
                        completionLengthAddStack[currentStackIndex] = completionLength - completionLengthBase;
                        matchedLengthStack[currentStackIndex] = checkLength;
                        ++currentStackIndex;
                    }
                   // change relevant iteration state
                    currentNodeIdx = i;
                    currentChildIdx = child->FirstChild;
                    prefixPending = true;
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
            currentChildIdx = childIndexStack[currentStackIndex];
            completionLength = completionLengthBase + completionLengthAddStack[currentStackIndex];
            currentUnmatchedPrefixLength += matchedLengthStack[currentStackIndex];
            matchedUntil -= matchedLengthStack[currentStackIndex];
            prefixPending = false;
        }
    }
}

/*
    Find the node with the longest prefix matching the given word and
    return its index.  The remaining length of the word is stored in
    lengthP.
*/
uint32_t CompletionTrie_FindLongestMatchingPrefix(completion_trie_root_t* root,
                                                   const char* word,
                                                   uint32_t* lengthP)
{
    uint32_t currentIdx = 0;
    uint32_t length = *lengthP;

    for(;;)
    {
        char c = word[0];
        assert(PrefixLen(ARENA_CHUNK_ARRAY_AT(root->Nodes, 0).Prefix4) == 0);

        /*
            The children of a node always start with distinct
            characters, so there is at most one candidate.
        */
        const uint32_t childIdx =
            CompletionTrie_FindChild(root, currentIdx, c);

        if (childIdx == COMPLETION_TRIE_NO_CHILD)
        {
            break;
        }

        {
            const completion_trie_node_t* child =
                ARENA_CHUNK_ARRAY_PTR(root->Nodes, childIdx);
            const uint32_t len = PrefixLen(child->Prefix4);

            if (len > length || !len)
            {
                break;
            }

            if (memcmp(child->Prefix4 + 1, word + 1, len - 1) != 0)
            {
                break;
            }

            currentIdx = childIdx;
            length -= len;
            word += len;
        }
    }

    (*lengthP) = length;
    return currentIdx;
}

/*
    Create a new, empty node and return its index.  The index stays
    valid for the lifetime of the trie.
*/
static uint32_t CompletionTrie_NewNode(completion_trie_root_t* root)
{
    uint32_t nodeIdx;

    ARENA_CHUNK_ARRAY_RESERVE(root->Nodes, 1);
    nodeIdx = root->NodesCount - 1;

    {
        completion_trie_node_t* node =
            ARENA_CHUNK_ARRAY_PTR(root->Nodes, nodeIdx);

        memset(node, 0, sizeof(*node));
        node->FirstChild = COMPLETION_TRIE_NO_CHILD;
        node->LastChild = COMPLETION_TRIE_NO_CHILD;
        node->NextSibling = COMPLETION_TRIE_NO_CHILD;
    }

    root->TotalNodes++;

    return nodeIdx;
}

/*
    Append a new (empty) child to the node at parentIdx and return its
    index.
*/
static uint32_t CompletionTrie_AppendChild(completion_trie_root_t* root,
                                            uint32_t parentIdx)
{
    const uint32_t childIdx = CompletionTrie_NewNode(root);
    completion_trie_node_t* parentNode =
        ARENA_CHUNK_ARRAY_PTR(root->Nodes, parentIdx);

    if (parentNode->FirstChild == COMPLETION_TRIE_NO_CHILD)
    {
        parentNode->FirstChild = childIdx;
    }
    else
    {
        ARENA_CHUNK_ARRAY_AT(root->Nodes, parentNode->LastChild).NextSibling =
            childIdx;
    }

    parentNode->LastChild = childIdx;
    parentNode->ChildCount++;

    return childIdx;
}

/*
    Find the child of the node at parentIdx whose prefix starts with the
    given character.  The children of a node always start with distinct
    characters, so there is at most one match.
*/
static uint32_t CompletionTrie_FindChild(completion_trie_root_t* root,
                                         uint32_t parentIdx, char c)
{
    uint32_t childIdx =
        ARENA_CHUNK_ARRAY_AT(root->Nodes, parentIdx).FirstChild;

    while (childIdx != COMPLETION_TRIE_NO_CHILD)
    {
        completion_trie_node_t* child =
            ARENA_CHUNK_ARRAY_PTR(root->Nodes, childIdx);

        if (child->Prefix4[0] == c)
        {
            return childIdx;
        }

        childIdx = child->NextSibling;
    }

    return COMPLETION_TRIE_NO_CHILD;
}

/*
    Split the node at childIdx.

    The first splitAt characters of the node's prefix stay with the
    parent, the remainder moves into a new child of the split node.  The
    split node takes the place of the old child in the parent.

    This is what makes it possible to store words whose prefixes overlap,
    e.g. "cl" and "clot": after the split the trie holds "cl" with the
    children "ot" and the terminal node of "cl".

    Returns the index of the split node.
*/
static uint32_t CompletionTrie_SplitNode(completion_trie_root_t* root,
                                         uint32_t parentIdx,
                                         uint32_t childIdx,
                                         uint32_t splitAt)
{
    completion_trie_node_t* child =
        ARENA_CHUNK_ARRAY_PTR(root->Nodes, childIdx);
    const uint32_t childLen = PrefixLen(child->Prefix4);
    /*
        The child keeps its place in the list of its siblings, so the
        split node does too and the links of the siblings stay valid.
    */
    const uint32_t nextSibling = child->NextSibling;
    const uint32_t childCount = child->ChildCount;
    const uint32_t firstChild = child->FirstChild;
    const uint32_t lastChild = child->LastChild;
    /*
        The split node is the very node that is being split, so its
        prefix has to be saved before it is overwritten.
    */
    char prefixCopy[4];
    uint32_t remainderIdx;
    uint32_t splitIdx = childIdx;

    assert(splitAt > 0 && splitAt < childLen);

    memcpy(prefixCopy, child->Prefix4, sizeof(prefixCopy));

    /* the split node takes the common prefix */
    {
        completion_trie_node_t* splitNode =
            ARENA_CHUNK_ARRAY_PTR(root->Nodes, splitIdx);

        memset(splitNode, 0, sizeof(*splitNode));
        memcpy(splitNode->Prefix4, prefixCopy, splitAt);
        splitNode->Prefix4[splitAt] = '\0';
        splitNode->NextSibling = nextSibling;
    }

    /* the remainder of the old prefix becomes the only child */
    remainderIdx = CompletionTrie_NewNode(root);
    {
        completion_trie_node_t* remainder =
            ARENA_CHUNK_ARRAY_PTR(root->Nodes, remainderIdx);

        memset(remainder, 0, sizeof(*remainder));
        memmove(remainder->Prefix4, prefixCopy + splitAt,
                (childLen - splitAt) + 1);
        remainder->Prefix4[childLen - splitAt] = '\0';
        remainder->ChildCount = (uint16_t)childCount;
        remainder->FirstChild = firstChild;
        remainder->LastChild = lastChild;
        remainder->NextSibling = COMPLETION_TRIE_NO_CHILD;
    }

    {
        completion_trie_node_t* splitNode =
            ARENA_CHUNK_ARRAY_PTR(root->Nodes, splitIdx);

        splitNode->ChildCount = 1;
        splitNode->FirstChild = remainderIdx;
        splitNode->LastChild = remainderIdx;
    }

    return splitIdx;
}

/*
    Insert the remaining word below the node at parentIdx and return the
    index of the node that represents the end of the inserted path.

    Words whose prefixes overlap are handled by splitting the node they
    conflict with.
*/
static uint32_t CompletionTrie_InsertPath(completion_trie_root_t* root,
                                           uint32_t parentIdx,
                                           const char* word, uint32_t length)
{
    while (length > 0)
    {
        const uint32_t childIdx =
            CompletionTrie_FindChild(root, parentIdx, word[0]);

        if (childIdx == COMPLETION_TRIE_NO_CHILD)
        {
            /*
                No child starts with this character, so we can add one
                that carries the next characters of the word.
            */
            const uint32_t copyLen = (length < 4) ? length : 4;
            uint32_t newChildIdx = CompletionTrie_AppendChild(root, parentIdx);
            completion_trie_node_t* child =
                ARENA_CHUNK_ARRAY_PTR(root->Nodes, newChildIdx);

            memcpy(child->Prefix4, word, copyLen);
            if (copyLen < 4)
            {
                child->Prefix4[copyLen] = '\0';
            }

            parentIdx = newChildIdx;
            word += copyLen;
            length -= copyLen;
            continue;
        }

        {
            completion_trie_node_t* child =
                ARENA_CHUNK_ARRAY_PTR(root->Nodes, childIdx);
            const uint32_t childLen = PrefixLen(child->Prefix4);
            uint32_t common = 0;

            while (common < childLen && common < length
                && child->Prefix4[common] == word[common])
            {
                common++;
            }

            if (common == childLen)
            {
                /* the child's prefix is fully matched, descend into it */
                parentIdx = childIdx;
                word += common;
                length -= common;
                continue;
            }

            /*
                The child's prefix and the word only share their first
                common characters, so the child has to be split.  The
                split node carries the common prefix and the word
                continues below it.
            */
            {
                const uint32_t splitIdx =
                    CompletionTrie_SplitNode(root, parentIdx, childIdx, common);

                parentIdx = splitIdx;
                word += common;
                length -= common;
            }
        }
    }

    return parentIdx;
}

/*
    Make sure that the node at parentIdx has a terminal child and return
    its index.  The terminal child marks the end of a word.
*/
static uint32_t CompletionTrie_InsertTerminal(completion_trie_root_t* root,
                                              uint32_t parentIdx)
{
    uint32_t terminalIdx = CompletionTrie_FindChild(root, parentIdx, '\0');

    if (terminalIdx != COMPLETION_TRIE_NO_CHILD)
    {
        return terminalIdx;
    }

    terminalIdx = CompletionTrie_AppendChild(root, parentIdx);
    INC(root->WordCount);

    return terminalIdx;
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
void CompletionTrie_Print(completion_trie_root_t* root,
    uint32_t nodeIdx,
    const char* rootPrefix,
    FILE* f,
    const completion_trie_viz_params_t * params
) {
    uint32_t childIdx =
        ARENA_CHUNK_ARRAY_AT(root->Nodes, nodeIdx).FirstChild;

    // Output DOT code for the edges between the current node and its children
    for(; childIdx != COMPLETION_TRIE_NO_CHILD;
        childIdx = ARENA_CHUNK_ARRAY_AT(root->Nodes, childIdx).NextSibling)
    {
        // Output an edge from the current node to a child node
        fprintf(f, "  \"%d: %.4s\" -> \"%d: %.4s\"\n",
                nodeIdx, rootPrefix,
                childIdx, ARENA_CHUNK_ARRAY_AT(root->Nodes, childIdx).Prefix4);
    }

    // Output highlight if we have params
    if (params && params->HighlightedNodeIdx == nodeIdx)
    {
        fprintf(f, "{ %d: %.4s [color=red style=filled fillcolor=lightpink] }\n",
                nodeIdx, rootPrefix);
    }
    // Output DOT code to ensure that child nodes appear in the same rank
    fprintf(f, "{ rank = same; ");
    for(childIdx = ARENA_CHUNK_ARRAY_AT(root->Nodes, nodeIdx).FirstChild;
        childIdx != COMPLETION_TRIE_NO_CHILD;
        childIdx = ARENA_CHUNK_ARRAY_AT(root->Nodes, childIdx).NextSibling)
    {
        // Output nodes in the same rank to maintain their alignment
        fprintf(f, "\"%d: %.4s\" ", childIdx,
                ARENA_CHUNK_ARRAY_AT(root->Nodes, childIdx).Prefix4);
    }
    fprintf(f, "}\n");

    // Recursively call CompletionTrie_Print for child nodes with ChildCount
    for(childIdx = ARENA_CHUNK_ARRAY_AT(root->Nodes, nodeIdx).FirstChild;
        childIdx != COMPLETION_TRIE_NO_CHILD;
        childIdx = ARENA_CHUNK_ARRAY_AT(root->Nodes, childIdx).NextSibling)
    {
        if (ARENA_CHUNK_ARRAY_AT(root->Nodes, childIdx).ChildCount)
        {
            // Recursively traverse child nodes with ChildCount
            CompletionTrie_Print(root, childIdx,
                                 ARENA_CHUNK_ARRAY_AT(root->Nodes, childIdx).Prefix4,
                                 f, params);
        }
    }
}

void CompletionTrie_Add(completion_trie_root_t* root, const char* word, uint32_t length)
{
    uint32_t remaining_length = length;
    uint32_t parentIdx =
        CompletionTrie_FindLongestMatchingPrefix(root, word, &remaining_length);

    if (remaining_length)
    {
        int posWord = length - remaining_length;

        parentIdx = CompletionTrie_InsertPath(root, parentIdx,
                                              word + posWord, remaining_length);
    }
    // insert terminal node
    CompletionTrie_InsertTerminal(root, parentIdx);
}

#if defined(COMP_TRIE_TEST_MAIN)
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
        uint32_t n =
            CompletionTrie_FindLongestMatchingPrefix(&root, "comp", &wordLength);

        CompletionTrie_Collect(&root, n, "comp", strlen("comp") - wordLength, testCollect, &userCtx);
        assert(userCtx.nMatches == 2);
    }
}

/*
    Words with overlapping prefixes have to be stored by splitting the
    node they conflict with.
*/
static void testSplitting(void)
{
    metac_alloc_t rootAlloc;
    Allocator_Init(&rootAlloc, 0);

    completion_trie_root_t root;
    CompletionTrie_Init(&root, &rootAlloc);

    /* "clot" and "cl" overlap, the node "clot" has to be split */
    CompletionTrie_Add(&root, "clot", 4);
    CompletionTrie_Add(&root, "cl", 2);

    /* both words have to be found again */
    {
        uint32_t len = 2;
        uint32_t n = CompletionTrie_FindLongestMatchingPrefix(&root, "cl", &len);
        assert(len == 0); /* "cl" is a node of its own now */
        assert(n != 0);
    }
    {
        uint32_t len = 4;
        uint32_t n = CompletionTrie_FindLongestMatchingPrefix(&root, "clot", &len);
        assert(len == 0);
        assert(n != 0);
    }

    /* collecting with the prefix "cl" has to find both words */
    {
        testCb_ctx_t userCtx;
        const char* strings[] = {"cl", "clot"};
        userCtx.Strings = strings;
        userCtx.nStrings = ARRAY_SIZE(strings);
        userCtx.nMatches = 0;

        CompletionTrie_Collect(&root, 0, "cl", 0, testCollect, &userCtx);
        assert(userCtx.nMatches == 2);
    }

    /* words that diverge in the middle of a node */
    CompletionTrie_Add(&root, "abcdef", 6);
    CompletionTrie_Add(&root, "abcxyz", 6);

    {
        uint32_t len = 6;
        uint32_t n = CompletionTrie_FindLongestMatchingPrefix(&root, "abcdef", &len);
        assert(len == 0);
        assert(n != 0);
    }
    {
        uint32_t len = 6;
        uint32_t n = CompletionTrie_FindLongestMatchingPrefix(&root, "abcxyz", &len);
        assert(len == 0);
        assert(n != 0);
    }

    {
        testCb_ctx_t userCtx;
        const char* strings[] = {"abcdef", "abcxyz"};
        userCtx.Strings = strings;
        userCtx.nStrings = ARRAY_SIZE(strings);
        userCtx.nMatches = 0;

        CompletionTrie_Collect(&root, 0, "abc", 0, testCollect, &userCtx);
        assert(userCtx.nMatches == 2);
    }

    /* a word that is a prefix of another one */
    CompletionTrie_Add(&root, "prefix", 6);
    CompletionTrie_Add(&root, "prefixsuffix", 12);

    {
        uint32_t len = 6;
        uint32_t n = CompletionTrie_FindLongestMatchingPrefix(&root, "prefix", &len);
        assert(len == 0);
        assert(n != 0);
    }
    {
        uint32_t len = 12;
        uint32_t n = CompletionTrie_FindLongestMatchingPrefix(&root, "prefixsuffix", &len);
        assert(len == 0);
        assert(n != 0);
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
        node_range_t range = ARENA_CHUNK_ARRAY_AT(self->NodeRanges, i);
        fprintf(f, "%sRange:{%d, %d}\n",
                   (range.IsValid ? "" : "- "), range.Begin, range.End);
    }
    fclose(f);
}
#endif

void CompletionTrie_PrintTrie(completion_trie_root_t* self, const completion_trie_viz_params_t params)
{
        char fname[32] = "g.dot";
        int n = params.FrameNumber;
        if (n != 0)
        {
            snprintf(fname, sizeof(fname), "g%u.dot", n);
        }
        ALIGN_STACK();
        FILE* f = fopen(fname, "w");
        fprintf(f, "digraph G {\n");
        fprintf(f, "  node [shape=record headport=n]\n");

        CompletionTrie_Print(self, 0, "", f, &params);

        fprintf(f, "}\n");
        fclose(f);
        RESTORE_STACK();
}

void CompletionTrie_PrintStats(completion_trie_root_t* self, uint16_t n)
{

    completion_trie_viz_params_t params = {.FrameNumber = n};
    CompletionTrie_PrintTrie(self, params);

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

#if defined(COMP_TRIE_TEST_MAIN)
    int main(int argc, const char* argv[])
    {
        CompletionTrie_NoPreallocatedCIdentifierChars = true;
        testCompletionTrie();
        CompletionTrie_NoPreallocatedCIdentifierChars = false;
        testSplitting();
    }
#endif

#undef MAXIMUM
#undef TRACK_RANGES
#undef BASE_IDX_SCALE
