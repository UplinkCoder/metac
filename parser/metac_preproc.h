#define error_key   0x5a01b4
#define warning_key 0x72b1fc
#define undef_key   0x5cabf4
#define elif_key    0x4f8f4e
#define ifdef_key   0x581ce0
#define ifndef_key  0x634e0c
#define endif_key   0x506843
#define line_key    0x4c4ac5
#define pragma_key  0x6a6e5b
#define include_key 0x7e87f0
#define define_key  0x6a491b

#define defined_key 0x7d9260
#define eval_key    0x45758c
#define va_args_key 0xbc18fc

#define pack_key 0x4b121c
#define push_key 0x42c9f9

#ifndef _METAC_PREPROC_H_
#define _METAC_PREPROC_H_

#include "../os/compat.h"
#include "metac_identifier_table.h"
#include "../os/metac_file.h"
#include "metac_array.h"
#include <string.h>

#define FOREACH_PREPROC_DIRECTIVE(M) \
    M(pp_invalid) \
    \
    M(pp_error) \
    M(pp_warning) \
    \
    M(pp_undef) \
    M(pp_if) \
    M(pp_elif) \
    M(pp_else) \
    M(pp_endif) \
    M(pp_ifdef) \
    M(pp_ifndef) \
    \
    M(pp_line) \
    M(pp_pragma) \
    M(pp_inline) \
    M(pp_define) \
    M(pp_include) \
    \
    M(pp_eval) \
    \
    M(pp_source_indicator)

#define WITH_COMMA(TOK) \
    TOK,

typedef enum metac_preprocessor_directive_t
{
    FOREACH_PREPROC_DIRECTIVE(WITH_COMMA)
    pp_max
} metac_preprocessor_directive_t;

static const char* Preprocessor_Directive_toChars(metac_preprocessor_directive_t directive)
{
    const char* result = 0;

    switch(directive)
    {
#define CASE(DIRC) \
        case DIRC: result = #DIRC; break;
    FOREACH_PREPROC_DIRECTIVE(CASE)
#undef CASE
    }

    return result;
}

#define FOREACH_LINEMARKER_FLAG(M) \
    M(linemarker_none, 0) \
    M(linemarker_newfile, 1) \
    M(linemarker_returntofile, 2) \
    M(linemarker_systemheader, 3) \
    M(linemarker_externc, 4)

#define DEFINE_LINEMARKER(NAME, VALUE) \
    NAME = LINEMARKER_VALUE(VALUE),

#define LINEMARKER_VALUE(VALUE) \
    ((VALUE != 0) ? (1 << VALUE) : 0)

#define LINEMARKER_MAX(_, VALUE) \
    LINEMARKER_VALUE(VALUE) |

#define LINEMARKER_MAX_STRING_LENGTH_(NAME, _) \
    sizeof(#NAME) +

#define LINEMARKER_MAX_STRING_LENGTH \
    (FOREACH_LINEMARKER_FLAG(LINEMARKER_MAX_STRING_LENGTH_) 1)

typedef enum metac_preprocessor_linemarker_flag_t
{
    FOREACH_LINEMARKER_FLAG(DEFINE_LINEMARKER)
    linemarker_max = (FOREACH_LINEMARKER_FLAG(LINEMARKER_MAX) 0)
} metac_preprocessor_linemarker_flag_t;

static const char* Preprocessor_LinemarkerFlag_toChars(metac_preprocessor_linemarker_flag_t flag)
{
    static char result[LINEMARKER_MAX_STRING_LENGTH];

    uint32_t currentFlag = BSF(flag);
    while(flag)
    {
        flag &= (~(1 << currentFlag));
        switch(currentFlag)
        {
// strcat can be used safely here since the max string length
// is computed at compile_time
#define CASE(NAME,VALUE) \
            case VALUE: strcat(result, (#NAME "|")); break;

        FOREACH_LINEMARKER_FLAG(CASE)
#undef CASE
        }
    }

    return result;
}

#define FOREACH_KNOWN_PRAGMA(M) \
    M(metac_pragma_unknown) \
    M(metac_pragma_pack) \
    M(metac_pragma_once)

typedef enum metac_preprocessor_pragma_enum_t
{
    FOREACH_KNOWN_PRAGMA(WITH_COMMA)
    metac_pragma_max
} metac_preprocessor_pragma_enum_t;

#undef WITH_COMMA


typedef struct metac_preprocessor_pragma_t
{
    metac_preprocessor_pragma_enum_t KnownPragma;
} metac_preprocessor_pragma_t;

typedef struct metac_preprocessor_define_ptr_t
{
    uint32_t v;
} metac_preprocessor_define_ptr_t;

typedef struct metac_define_table_slot_t
{
    uint32_t HashKey;
    metac_preprocessor_define_ptr_t DefinePtr;

#ifdef REFCOUNT
    uint32_t RefCount;
    uint32_t Displacement;
#endif
} metac_define_table_slot_t;


typedef struct metac_preprocessor_define_t
{
    metac_location_t loc;

    metac_identifier_ptr_t DefineName;
    uint32_t ParameterCount : 30;
    bool IsVariadic : 1;
    bool HasPaste : 1;

    uint32_t TokensOffset;
    uint32_t TokenCount;
} metac_preprocessor_define_t;

typedef struct metac_define_table_t
{
    metac_define_table_slot_t* Slots;
    struct metac_preprocessor_t* Preproc;

    metac_token_t* TokenMemory;
    uint32_t TokenMemorySize;
    uint32_t TokenMemoryCapacity;

    uint32_t SlotCount_Log2;
    uint32_t SlotsUsed;
    uint32_t LengthShift;
    uint32_t MaxDisplacement;

    metac_preprocessor_define_t* DefineMemory;
    uint32_t DefineMemorySize;
    uint32_t DefineMemoryCapacity;
} metac_define_table_t;

typedef struct metac_preprocessor_source_indicator_t
{
    uint32_t LineNumber;
    metac_identifier_ptr_t FileNameString;
    metac_preprocessor_linemarker_flag_t Flags;
} metac_preprocessor_source_indicator_t;

typedef struct metac_preprocessor_t
{
    metac_file_storage_t* FileStorage;
    metac_identifier_table_t StringTable;
    metac_identifier_table_t IdentifierTable;

    metac_define_table_t DefineTable;
    metac_identifier_table_t DefineIdentifierTable;
    struct metac_preprocessor_t* Parent;

    metac_token_t_array DefineTokenStack[16];
    uint32_t DefineTokenIndexStack[16];
    uint32_t DefineTokenStackCount;

    metac_token_t* TokenMemory;
    uint32_t TokenMemorySize;
    uint32_t TokenMemoryCapacity;

    /// the file we are running the preprocessor on
    metac_file_ptr_t File;

    metac_alloc_t Allocator;
} metac_preprocessor_t;

struct metac_parser_t;

void MetaCPreProcessor_Init(metac_preprocessor_t *self, metac_lexer_t* lexer,
                            metac_alloc_t* alloc,
                            metac_file_storage_t* fs, const char* filepath);

metac_preprocessor_define_ptr_t
MetaCPreProcessor_ParseDefine(metac_preprocessor_t *self,
                              struct metac_parser_t* parser);

metac_preprocessor_source_indicator_t
MetaCPreProcessor_ParseSourceIndicator(metac_preprocessor_t *self,
                                       struct metac_parser_t* parser);

metac_preprocessor_pragma_t
MetaCPreProcessor_ParsePragma(metac_preprocessor_t* self,
                              struct metac_parser_t* parser);

metac_preprocessor_define_ptr_t MetaCPreProcessor_GetDefine(metac_preprocessor_t* self,
                                                            uint32_t identifierKey,
                                                            const char* identifier);

void MetaCPreProcessor_Include(metac_preprocessor_t *self, struct metac_parser_t* parser);

uint32_t MetaCPreProcessor_PushDefine(metac_preprocessor_t* self,
                                      metac_preprocessor_define_t* define,
                                      metac_token_t_array_array parameters);
#endif
