#define error_key   0x5a01b4
#define warning_key 0x72b1fc
#define undef_key   0x5cabf4
#define elif_key    0x4f8f4e
#define ifdef_key   0x581ce0
#define ifndef_key  0x634e0c
#define endif_key   0x506843
#define line_key    0x4c4ac5
#define pargma_key  0x6a6e5b
#define include_key 0x7e87f0
#define define_key  0x6a491b

#define defined_key 0x7d9260
#define eval_key    0x45758c
#define va_args_key 0xbc18fc

#ifndef _METAC_PREPROC_H_
#define _METAC_PREPROC_H_

#include "../os/compat.h"
#include "metac_identifier_table.h"
#include "../os/metac_file.h"
#include "metac_parser.h"
#include "metac_array.h"

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
} metac_preprocessor_directive_t;

#undef WITH_COMMA

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

void MetaCPreProcessor_Init(metac_preprocessor_t *self, metac_lexer_t* lexer,
                            metac_alloc_t* alloc,
                            metac_file_storage_t* fs, const char* filepath);

metac_preprocessor_define_ptr_t MetaCPreProcessor_GetDefine(metac_preprocessor_t* self,
                                                            uint32_t identifierKey,
                                                            const char* identifier);

uint32_t MetaCPreProcessor_PushDefine(metac_preprocessor_t* self,
                                      metac_preprocessor_define_t* define,
                                      metac_token_t_array_array parameters);
#endif
