#define error_key 0x5a01b4
#define warning_key 0x72b1fc
#define undef_key 0x5cabf4
#define elif_key 0x4f8f4e
#define ifdef_key 0x581ce0
#define ifndef_key 0x634e0c
#define endif_key 0x506843
#define line_key 0x4c4ac5
#define pargma_key 0x6a6e5b
#define include_key 0x7e87f0
#define define_key 0x6a491b


#ifndef _METAC_PREPROC_H_
#define _METAC_PREPROC_H_

#include "compat.h"
#include "metac_identifier_table.h"
#include "metac_file.h"
#include "metac_parser.h"

#define defined_key 0x7d9260
#define eval_key 0x45758c

typedef enum metac_preprocessor_directive_t
{
    pp_invalid,

    pp_error,
    pp_warning,

    pp_undef,
    pp_if,
    pp_elif,
    pp_else,
    pp_endif,
    pp_ifdef,
    pp_ifndef,

    pp_line,
    pp_pragma,
    pp_inline,
    pp_define,
    pp_include,

    // not a standart directive
    pp_eval
} metac_preprocessor_directive_t;

typedef struct metac_preprocessor_t
{
    metac_file_storage_t* FileStorage;
    metac_lexer_t* Lexer;
    metac_identifier_table_t DefineTable;
    struct metac_preprocessor_t* Parent;

    /// the file we are running the preprocessor on
    metac_file_ptr_t File;
} metac_preprocessor_t;


void MetaCPreProcessor_Init(metac_preprocessor_t *self, metac_lexer_t* lexer,
                            metac_file_storage_t* fs, const char* filepath);

bool MetaCPreProcessor_IsDefine(metac_preprocessor_t* self,
                                uint32_t identifierKey, const char* identifier);

#endif
