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
#ifdef REFCOUNT
    uint32_t RefCount;
    uint32_t Displacement;
#endif
} metac_identifier_table_slot_t;


typedef struct metac_identifier_table_t
{
    metac_identifier_table_slot_t* Slots;
    char*    StringMemory;

    uint32_t StringMemorySize;
    uint32_t StringMemoryCapacity;
    uint32_t SlotCount_Log2;
    uint32_t SlotsUsed;

    uint32_t MaxDisplacement;
} metac_identifier_table_t;

#pragma pack(push, 1)
typedef struct identifier_table_file_header_t
{
    uint32_t NumberOfSlots;
    uint32_t SizeofSlot;
    uint32_t OffsetSlots;
    uint32_t OffsetStrings;

    uint32_t StringMemorySize;
    uint32_t Version;
    uint32_t HeaderCommentSize;
    uint32_t LengthShift;
} identifier_table_file_header_t;
#pragma pack(pop)

void IdentifierTableInit(metac_identifier_table_t* table);

metac_identifier_ptr_t GetOrAddIdentifier(metac_identifier_table_t* table,
                                          uint32_t identifierKey,
                                          const char* identifier, uint32_t length);
bool IsInTable(const metac_identifier_table_t* table,
               uint32_t key, metac_identifier_ptr_t value);

const char* IdentifierPtrToCharPtr(metac_identifier_table_t* table,
                                   metac_identifier_ptr_t ptr);

metac_identifier_table_t ReadTable(const char* filename);
void WriteTable(metac_identifier_table_t* table, const char* filename, uint32_t lengthShift, const char* comment);
metac_identifier_table_slot_t* findFirstEntry(metac_identifier_table_t* table);

#endif