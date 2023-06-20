#ifndef _METAC_DRIVER_H_
#define _METAC_DRIVER_H_

#include "../os/compat.h"
#include "../parser/metac_lexer.h"
#include "../parser/metac_parser.h"

typedef struct decl_array_t
{
    metac_decl_t** Ptr;
    uint32_t Length;
    uint32_t Capacity;
} decl_array_t;

void LexFile(metac_lexer_t* lexer,
             const char* path,
             const char* text, int32_t length);

void ParseFile(metac_parser_t* parser,
               const char* path,
               decl_array_t* result);

metac_decl_t* FindDecl(decl_array_t decls,
                       metac_parser_t* parser,
                       const char* name);


extern bool errored;

#endif //_METAC_DRIVER_H_
