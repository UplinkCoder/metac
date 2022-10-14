#define _CRT_SECURE_NO_WARNINGS
#define NO_FIBERS
#include "../3rd_party/linenoise/linenoise-w32.cpp"

#include "../repl/repl.c"

typedef struct ui_state_t
{
    repl_mode_t parseMode;
} ui_state_t;

const char* Linenoise_GetInputLine(ui_state_t* state, repl_state_t* repl, uint32_t* length)
{
    const char* line = linenoise(repl->Promt);

    if (line)
    {
        *length = strlen(line);
        linenoiseHistoryAdd(line);
    }
    else
    {
        *length = 0;
    }

    return line;
}

#include <stdarg.h>
void Linenoise_Message(ui_state_t* state, const char* fmt, ...)
{
    va_list args;
    va_start (args, fmt);
    vprintf (fmt, args);
    va_end (args);
}

void Linenoise_Info(ui_state_t* state, const char* fmt, ...)
{
    printf("Info: ");
    va_list args;
    va_start (args, fmt);
    vprintf (fmt, args);
    va_end (args);
}

static inline
completion_list_t Complete(repl_state_t* repl, const char* input, uint32_t inputLength);



repl_mode_t Linenoise_QueryMode(ui_state_t* uiState)
{
    return uiState->parseMode;
}

const struct ui_interface_t LinenoiseUiInterface =
{
    Linenoise_GetInputLine,
    Linenoise_Message,
    Linenoise_QueryMode,
    Linenoise_Info,
    0
} ;

int main(int argc, const char* argv[])
{
#ifndef NO_FIBERS
    aco_global_init();
#endif
    linenoiseHistoryLoad(".repl_history");
    printf("Please enter :h for help\n");
    repl_state_t repl;
    ui_state_t uiState;

    repl_ui_context_t ctx;

    ctx.UiInterface = LinenoiseUiInterface;
    ctx.UiState = &uiState;

    g_uiContext = &ctx;
#ifndef NO_FIBERS
    worker_context_t replWorkerContext = {0};
    worker_context_t* ctxPtr = &replWorkerContext;
    THREAD_CONTEXT_SET(ctxPtr);
    RunWorkerThread(&replWorkerContext, Repl_Fiber, 0);
#else
    ReplStart(&ctx);
#endif
    linenoiseHistorySave(".repl_history");
    return 1;
}
