// metac-dot-printer.c
#include "metac_dot_printer.h"
#include "metac_lexer.h"
#include "int_to_str.c"
#include <assert.h>
#include <string.h>
#include <stdlib.h>

#define abort()

static inline void Dot_PrintSpace(metac_dot_printer_t* self)
{
    self->StringMemorySize++;
}

static inline void Dot_PrintChar(metac_dot_printer_t* self, char c)
{
    assert(c != ' ' && c != '\n');
    self->StringMemory[self->StringMemorySize++] = c;
}

static inline void Dot_PrintNewline(metac_dot_printer_t* self)
{
    self->StringMemory[self->StringMemorySize++] = '\n';
}

static inline void Dot_CheckAndRellocIfNeeded(metac_dot_printer_t* self,
                                              uint32_t length)
{
    int32_t underflow =
        self->StringMemoryCapacity -
        (self->StringMemorySize + length + 1024);
    if (underflow < 0)
    {
        uint32_t newCapa = (uint32_t)(self->StringMemoryCapacity * 1.3);
        newCapa = ((newCapa + 4095) & ~4095);
        self->StringMemory = (char*)realloc(self->StringMemory, newCapa);
        if (self->StringMemory == 0)
        {
            abort();
        }
    }
}

metac_dot_printer_label_t* MetaCDotPrinter_BeginLabel(metac_dot_printer_t* self)
{
    assert(self->CurrentLabel == 0);
    self->CurrentLabel = (metac_dot_printer_label_t*)
        malloc(sizeof(metac_dot_printer_label_t) + 128);
    self->CurrentLabel->LabelMemoryCapacity = 128;
    self->CurrentLabel->LabelMemory =
        ((char*) self->CurrentLabel) + sizeof(metac_dot_printer_label_t);

    return self->CurrentLabel;
}

static inline bool NeedsEscape(char c)
{
    switch (c)
    {
        case '\n':
        case '\t':
        case '\'':
        case '\"':
            return true;
    }
    return false;
}

static inline char AfterEscape(char c)
{
    switch (c)
    {
        case '\n':
            return 'n';
        case '\t':
            return 't';
    }

    return c;
}

void MetaCDotPrinterLabel_PrintChar(metac_dot_printer_label_t* self, char c)
{
    if (NeedsEscape(c))
    {
        self->LabelMemory[self->LabelMemorySize++] = '\\';
        self->LabelMemory[self->LabelMemorySize++] = AfterEscape(c);
    }
    else
    {
        self->LabelMemory[self->LabelMemorySize++] = c;
    }
}

#ifndef ALIGN4
#  define ALIGN4(N) \
      (((N) + 3) & ~3)
#endif

void MetaCDotPrinter_EndLabel(metac_dot_printer_t* self, metac_dot_printer_label_t* label)
{
    assert(self->CurrentLabel == label);
    uint32_t neededSize = self->CurrentLabel->LabelMemorySize + sizeof("label=\"\"");
    uint32_t newCapa = ALIGN4(self->StringMemorySize + neededSize);

    if (newCapa > self->StringMemoryCapacity)
    {
        self->StringMemory = (char*)realloc(self->StringMemory, newCapa);
        self->StringMemoryCapacity = newCapa;
    }
    Dot_PrintString(self, "label=\"");

    memcpy(self->StringMemory + self->StringMemorySize,
        self->CurrentLabel->LabelMemory,
        self->CurrentLabel->LabelMemorySize);

    Dot_PrintChar(self, '\"');
}

void Dot_PrintString(metac_dot_printer_t* self,
                     const char* string)
{
    char c;

    while((c = *string++))
    {
        self->StringMemory[self->StringMemorySize++] = c;
    }
}

static inline void Dot_PrintIdentifier(metac_dot_printer_t* self,
                                       metac_identifier_ptr_t idPtr)
{
    if (idPtr.v == empty_identifier.v)
        assert(0); // One is not supposed to print the empty identifier
    const char* ident = IdentifierPtrToCharPtr(self->IdTable, idPtr);

    Dot_PrintString(self, ident);
}


static inline void Dot_PrintU64(metac_dot_printer_t* self, uint64_t value)
{
    char u64Buffer[21];

    char* result = u64tostr(value, u64Buffer);
    int32_t length = (u64Buffer + 20) - result;
    // assert((length > 0) && (length <= 20));
    result[length] = '\0';
    Dot_PrintString(self, result);
}

static inline void Dot_PrintI64(metac_dot_printer_t* self, int64_t value)
{
    char i64Buffer[22];

    char* result = i64tostr(value, i64Buffer);
    int32_t length = (i64Buffer + 20) - result;
    // assert((length > 0) && (length <= 20));
    result[length] = '\0';
    Dot_PrintString(self, result);
}


void MetaCDotPrinter_Reset(metac_dot_printer_t* self)
{
    memset(self->StringMemory, ' ', self->StringMemorySize);

    self->StringMemorySize = 0;
}

const char* MetaCDotPrinter_Snapshot(metac_dot_printer_t* self)
{
    if (self->StringMemorySize > self->SnapshotMemoryCapacity)
    {
        uint32_t newCapa  = ALIGN4(self->StringMemorySize + 1);
        self->SnapshotMemory = (char*)realloc(self->SnapshotMemory, newCapa);
        self->StringMemoryCapacity = newCapa;
    }

    memcpy(self->SnapshotMemory, self->StringMemory, self->StringMemorySize);
    self->SnapshotMemory[self->StringMemorySize] = '\0';
    return self->SnapshotMemory;
}

void MetaCDotPrinter_Init(metac_dot_printer_t* self,
                          metac_identifier_table_t* idTable)
{
    self->StringMemoryCapacity = INITIAL_SIZE;
    self->StringMemory = (char*)malloc(self->StringMemoryCapacity);
    self->StringMemorySize = self->StringMemoryCapacity;

    self->SnapshotMemoryCapacity = INITIAL_SIZE;
    self->SnapshotMemory = (char*)malloc(self->SnapshotMemoryCapacity);
    self->IdTable = idTable;

    MetaCDotPrinter_Reset(self);
}
