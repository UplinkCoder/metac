#ifndef _METAC_IDENTIFIER_TREE_C_
#define _METAC_IDENTIFIER_TREE_C_
#pragma once

#include "metac_identifier_tree.h"

#include "compat.h"
#include <assert.h>
#include <stdlib.h>
#include "metac_lexer.h"
#include <string.h>

#include "3rd_party/tracy/TracyC.h"

#define ALIGN4(N) \
    (((N) + 3) & ~3)

void IdentifierTreeInit(metac_identifier_tree_t* tree)
{
    const uint32_t maxNodes = (1 << 16);
    tree->Root = (metac_identfier_tree_node_t*) calloc(maxNodes, sizeof(metac_identfier_tree_node_t));
    tree->NodesSize = 1;
    tree->NodesCapacity = maxNodes;

    tree->StringMemory = (char*)malloc(32768 * 8);
    tree->StringMemoryCapacity = 32768 * 8;
    tree->StringMemorySize = 0;
}

const char* IdentifierPtrToCharPtr(metac_identifier_tree_t* tree,
                                   metac_identifier_ptr_t ptr)
{
    return ptr.v + 4 + tree->StringMemory;
}

metac_identifier_ptr_t GetOrAddIdentifier(metac_identifier_tree_t* tree,
                                          uint32_t identifierKey,
                                          const char* identifier, uint32_t length)
{
    metac_identifier_ptr_t result = {0};
    TracyCZone(ctx, true);
    if (!length)
        return result;

    for(metac_identfier_tree_node_t* currentBranch = tree->Root;
        currentBranch;)
    {
        int cmp_result = identifierKey - currentBranch->IdentifierKey;
        if (!cmp_result)
        {
            const char* cached_name = (currentBranch->Ptr.v - 4)
                                    + tree->StringMemory;
            if ((cmp_result = memcmp(identifier, cached_name, length)) == 0)
            {
                static uint32_t hits = 0;

                TracyCPlot("Hits", hits++);
                result = currentBranch->Ptr;
                break;
            }
        }
        // when we end up here we need to branch
        {
            metac_identfier_tree_node_t* nextBranch = currentBranch +
                                            ((cmp_result < 0)
                                              ? currentBranch->Left
                                              : currentBranch->Right);
            if (nextBranch != currentBranch)
            {
                currentBranch = nextBranch;
                continue;
            }

            // here we need to insert a new node
            {
                assert(tree->NodesSize
                       < tree->NodesCapacity);
                nextBranch = (tree->NodesSize++
                           + tree->Root);
                if (cmp_result < 0)
                {
                    currentBranch->Left = nextBranch - currentBranch;
                }
                else
                {
                    currentBranch->Right = nextBranch - currentBranch;
                }

                assert(tree->StringMemorySize
                       < tree->StringMemoryCapacity);
                char* cached_name =
                    tree->StringMemorySize + tree->StringMemory;
                memcpy(cached_name, identifier, length);
                *(cached_name + length) = '\0';

                metac_identifier_ptr_t Ptr = { tree->StringMemorySize + 4 };
                tree->StringMemorySize += ALIGN4(length + 1);

                nextBranch->IdentifierKey = identifierKey;
                nextBranch->Left = 0;
                nextBranch->Right = 0;
                result = nextBranch->Ptr = Ptr;
                static uint32_t insertions = 0;
                TracyCPlot("Insertions", insertions++)
                break;
            }
        }
    }

    TracyCZoneEnd(ctx);
    assert(result.v != 0);

    return result;
}
#endif
