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

static inline void Dot_PrintString(metac_dot_printer_t* self,
                 const char* string, uint32_t length)
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

    Dot_PrintString(self, ident, strlen(ident));
}


static inline void Dot_PrintU64(metac_dot_printer_t* self, uint64_t value)
{
    char u64Buffer[21];

    const char* result = u64tostr(value, u64Buffer);
    int32_t length = (u64Buffer + 20) - result;
    // assert((length > 0) && (length <= 20));
    Dot_PrintString(self, result, length);
}

static inline void Dot_PrintI64(metac_dot_printer_t* self, int64_t value)
{
    char i64Buffer[22];

    const char* result = i64tostr(value, i64Buffer);
    int32_t length = (i64Buffer + 20) - result;
    // assert((length > 0) && (length <= 20));
    Dot_PrintString(self, result, length);
}


void MetaCDotPrinter_Reset(metac_dot_printer_t* self)
{
    memset(self->StringMemory, ' ', self->StringMemorySize);

    self->StringMemorySize = 0;
}

void MetaCDotPrinter_Init(metac_dot_printer_t* self,
                          metac_identifier_table_t* idTable)
{
    self->StringMemoryCapacity = INITIAL_SIZE;
    self->StringMemory = (char*)malloc(self->StringMemoryCapacity);
    self->StringMemorySize = self->StringMemoryCapacity;
    self->IdTable = idTable;

    MetaCDotPrinter_Reset(self);
}
