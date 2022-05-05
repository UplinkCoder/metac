#ifndef _METAC_DOT_PRINTER_H_
#define _METAC_DOT_PRINTER_H_
#pragma once

#include "compat.h"
#include "metac_identifier_table.h"
#include "metac_printer.h"
#include "metac_parsetree.h"


#define CHUNK_SIZE 4096
#define INITIAL_SIZE (16 * 4096)
#define GROWTH_FACTOR 1.3f

typedef struct metac_dot_printer_label_t
{
    char* LabelMemory;
    uint32_t LabelMemorySize;
    uint32_t LabelMemoryCapacity;
} metac_dot_printer_label_t;


typedef struct metac_dot_printer_t
{
    char* StringMemory;
    uint32_t StringMemorySize;
    uint32_t StringMemoryCapacity;

    char* SnapshotMemory;
    uint32_t SnapshotMemoryCapacity;

    metac_identifier_table_t* IdTable;

    metac_dot_printer_label_t* CurrentLabel;
} metac_dot_printer_t;




void MetaCDotPrinter_Init(metac_dot_printer_t* self,
                          metac_identifier_table_t* idTable);
const char* MetaCDotPrinter_Snapshot(metac_dot_printer_t* self);

void MetaCDotPrinter_Reset(metac_dot_printer_t* self);

metac_dot_printer_label_t* MetaCDotPrinter_BeginLabel(metac_dot_printer_t* self);
void MetaCDotPrinter_EndLabel(metac_dot_printer_t* self, metac_dot_printer_label_t* label);

#endif // _METAC_PRINTER_H_
