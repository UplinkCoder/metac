#ifndef _METAC_REPL_H_
#define _METAC_REPL_H_

typedef enum parse_mode_t
{
    parse_mode_token = 0,

    parse_mode_decl,
    parse_mode_stmt,
    parse_mode_expr,
    parse_mode_file,
    parse_mode_preproc,

    parse_mode_ds,
    parse_mode_ss,

    parse_mode_ee,
    parse_mode_es,
    parse_mode_setvars,

    parse_mode_max
} parse_mode_t;

typedef struct repl_state_t
{
    parse_mode_t ParseMode;

    metac_lpp_t LPP;

    metac_type_aggregate_t* CompilerInterface;
    metac_semantic_state_t SemanticState;

    metac_lexer_t FileLexer;


    const char* Line;

    const char* Promt;

    char* SrcBuffer;
    void* FreePtr;

    int32_t LineSz;
    int32_t SrcBufferLength;

    metac_printer_t printer;
    variable_store_t vstore;
} repl_state_t;

// Initializes the repl state
void Repl_Init(repl_state_t* self);

// Switches self->Promt to the correct string
void Repl_SwtichMode(repl_state_t* self);

#endif