#ifndef _METAC_IDENTIFIER_TREE_H_
#define _METAC_IDENTIFIER_TREE_H_
#pragma once

typedef struct metac_identifier_ptr_t
{
    uint32_t v;
} metac_identifier_ptr_t;


typedef struct metac_identfier_tree_node_t
{
    uint32_t IdentifierKey;
    int32_t Left;
    int32_t Right;

    metac_identifier_ptr_t Ptr;
} metac_identfier_tree_node_t;

typedef struct metac_identifier_tree_t
{
    struct metac_identfier_tree_node_t* Root;
    char*    StringMemory;

    uint32_t NodesCapacity;
    uint32_t NodesSize;

    uint32_t StringMemorySize;
    uint32_t StringMemoryCapacity;
} metac_identifier_tree_t;

void IdentifierTreeInit(metac_identifier_tree_t* table);

metac_identifier_ptr_t GetOrAddIdentifier(metac_identifier_tree_t* tree,
                                          uint32_t identifierKey,
                                          const char* identifier, uint32_t length);

const char* IdentifierPtrToCharPtr(metac_identifier_tree_t* table,
                                   metac_identifier_ptr_t ptr);

#endif
