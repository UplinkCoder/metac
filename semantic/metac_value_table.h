#ifndef _METAC_VALUE_TABLE_H_
#define _METAC_VALUE_TABLE_H_
#include "../parser/metac_node.h"

typedef struct metac_alloc_t metac_alloc_t;

// Define a generic metac_node_ptr_t structure to hold metac_node_t mappings
typedef struct metac_node_ptr_t
{
    uint32_t v;
} metac_node_ptr_t;

extern const metac_node_ptr_t empty_node;

// The slot structure stores the mapping from one metac_node_t to another
typedef struct metac_value_table_slot_t
{
    metac_node_t KeyNode;   // The key node in the mapping
    metac_node_t ValueNode; // The value node it maps to

#ifdef REFCOUNT
    uint32_t RefCount;          // Optional reference counting
    uint32_t Displacement;      // Displacement for handling hash collisions
#endif
} metac_value_table_slot_t;

// The main table structure that holds the slots and manages memory
typedef struct metac_value_table_t
{
    metac_value_table_slot_t* Slots;   // Array of slots for storing mappings
    uint32_t SlotCount_Log2;           // Log2 of the number of slots (for hashing)
    uint32_t SlotsUsed;                // Number of slots currently used

    metac_alloc_t* Allocator;          // Allocator used for managing table memory
} metac_value_table_t;

#pragma pack(push, 1)
// File header structure to represent the value table in a serialized form
typedef struct value_table_file_header_t
{
    uint32_t NumberOfSlots;       // Total number of slots
    uint32_t SizeofSlot;          // Size of each slot
    uint32_t OffsetSlots;         // Offset to the slots in the file
    uint32_t OffsetValues;        // Offset to the value nodes in the file

    uint32_t Version;             // Version of the serialized format
    uint32_t LengthShift;         // Shift used to determine table growth
    uint32_t HeaderCommentSize;   // Size of any comments or metadata in the header
} value_table_file_header_t;
#pragma pack(pop)
#endif