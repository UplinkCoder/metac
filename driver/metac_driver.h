#ifndef _METAC_DRIVER_H_
#define _METAC_DRIVER_H_

#include "../os/compat.h"
#include "../parser/metac_lexer.h"
#include "../parser/metac_parser.h"

typedef struct DeclarationArray
{
    metac_declaration_t** Ptr;
    uint32_t Length;
    uint32_t Capacity;
} DeclarationArray;

void LexFile(metac_lexer_t* lexer,
             const char* path,
             const char* text, int32_t length);

void ParseFile(metac_parser_t* parser,
               const char* path,
               DeclarationArray* result);

extern bool errored;

#endif //_METAC_DRIVER_H_
