#ifndef _METAC_REPL_H_
#define _METAC_REPL_H_

#ifndef NO_FIBERS
#  ifdef HAS_TLS
    extern __thread worker_context_t *threadContext;
#  else
    extern worker_context_t *threadContext;
#  endif
#endif

typedef enum repl_mode_t
{
    repl_mode_token = 0,

    repl_mode_decl,
    repl_mode_stmt,
    repl_mode_expr,
    repl_mode_lex_file,
    repl_mode_preproc,

    repl_mode_ds,
    repl_mode_ss,

    repl_mode_ee,
    repl_mode_es,
    repl_mode_setvars,

    repl_mode_max
} repl_mode_t;

typedef struct repl_state_t
{
    repl_mode_t ParseMode;

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
    // variable_store_t vstore;
} repl_state_t;

struct ui_state_t;

typedef struct completion_cache_entry_t
{
    const char* Identifier;
    uint16_t IdentifierLength;
    metac_node_kind_t Kind;
} completion_cache_entry_t;

typedef struct ui_interface_t
{
    /// Returns a string of length $(*length) when a input line is
    /// avilable, NULL if no line is ready
    const char* (*GetInputLine)(repl_state_t* repl, struct ui_state_t* state, uint32_t* length);
    /// Regular output that would go to printf otherwise
    void (*Message) (struct ui_state_t* state, const char* fmt, ...);
    /// Query current mode for the repl;
    repl_mode_t (*QueryMode)(struct ui_state_t* state);

    /// [Optional] Extra information that'll go to a diffrent area if possible
    void (*Info) (struct ui_state_t* state, const char* fmt, ...);
    /// [Optional] Returns a string of length $(*length) with partial input data for completion
    const char* (*GetPartialInput)(struct ui_state_t* state, uint32_t* length);
    /// [Optional] Updates local completion cache this is useful for webinterfaces and the like
    /// where a call to the server for completion suggestions might want to be avoided
    uint32_t (*UpdateLocalCompletionCache)(struct ui_state_t* state,
                                           completion_cache_entry_t* entries, uint32_t nEntries);
} ui_interface_t;

typedef struct repl_ui_context_t
{
    ui_interface_t UiInterface;
    struct ui_state_t* UiState;
} repl_ui_context_t;



// Initializes the repl state
void Repl_Init(repl_state_t* self);

// Switches self->Promt to the correct string
void Repl_SwtichMode(repl_state_t* self);

#endif
