#ifndef _METAC_PRINTER_H_
#define _METAC_PRINTER_H_
#pragma once

#include "../os/compat.h"
#include "../parser/metac_identifier_table.h"
#include "../parser/metac_parsetree.h"

#ifndef NO_SEMANTIC
#  include "../semantic/metac_sematree.h"
#endif

#define CHUNK_SIZE 4096
#define INITIAL_SIZE (16 * 4096)
#define GROWTH_FACTOR 1.3f

typedef struct metac_sema_state_t metac_sema_state_t;

typedef struct metac_printer_t
{
    char* StringMemory;
    uint32_t StringMemorySize;
    uint32_t StringMemoryCapacity;

    metac_identifier_table_t* IdentifierTable;
    metac_identifier_table_t* StringTable;

    uint16_t IndentLevel;
    uint16_t CurrentColumn;

    uint16_t StartColumn;
    bool SuppressNewlineAfterDecl;
    bool AsType;
    bool ForTypedef;
    int32_t ForAnonymousField;
} metac_printer_t;


void MetaCPrinter_Init(metac_printer_t* self,
                       metac_identifier_table_t* identifierTable,
                       metac_identifier_table_t* stringTable);
void MetaCPRinter_Free(metac_printer_t* self);

void MetaCPrinter_InitSz(metac_printer_t* self,
                         metac_identifier_table_t* identifierTable,
                         metac_identifier_table_t* stringTable,
                         uint32_t initializeSize);

const char* MetaCPrinter_PrintExpr(metac_printer_t* self, metac_expr_t* exp);
const char* MetaCPrinter_PrintDecl(metac_printer_t* self, metac_decl_t* decl);
const char* MetaCPrinter_PrintStmt(metac_printer_t* self, metac_stmt_t* stmt);
const char* MetaCPrinter_PrintNode(metac_printer_t* self, metac_node_t node, uint32_t level);

const char* StmtKind_toChars(metac_stmt_kind_t kind);

void MetacPrinter_PrintI64(metac_printer_t* self, const int64_t value);
void MetacPrinter_PrintStringLiteral(metac_printer_t* self, const char* str);

void MetaCPrinter_Reset(metac_printer_t* self);

#ifndef NO_SEMANTIC

const char* MetaCPrinter_PrintSemaNode(metac_printer_t* self,
                                       metac_sema_state_t* sema,
                                       metac_node_t node);

#endif // NO_SEMANTIC

#endif // _METAC_PRINTER_H_
