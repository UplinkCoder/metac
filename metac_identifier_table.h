#ifndef _METAC_IDENTIFIER_TABLE_H_
#define _METAC_IDENTIFIER_TABLE_H_
#pragma once

typedef struct metac_identifier_ptr_t
{
    uint32_t v;
} metac_identifier_ptr_t;

typedef struct metac_identifier_table_slot_t
{
    uint32_t HashKey;
    metac_identifier_ptr_t Ptr;
} metac_identifier_table_slot_t;


typedef struct metac_identifier_table_t
{
    char*    StringMemory;
    uint32_t StringMemorySize;
    uint32_t StringMemoryCapacity;

    uint32_t SlotCount_Log2;
    metac_identifier_table_slot_t* Slots;

    metac_identifier_table_slot_t inlineSlots[256];
} metac_identifier_table_t;

void InitIdentifierTable(metac_identifier_table_t* table);

metac_identifier_ptr_t GetOrAddIdentifier(metac_identifier_table_t* table,
                                          const char* identifier,
                                          uint32_t identifierKey);

const char* IdentifierPtrToCharPtr(metac_identifier_table_t* table,
                                   metac_identifier_ptr_t ptr);
#endif
