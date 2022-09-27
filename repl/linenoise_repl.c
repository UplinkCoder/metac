#define ACCEL ACCEL_TABLE

#ifndef CURSES_UI
#define LINENOISE_UI

#include "repl.c"
#include "../codegen/metac_codegen.c"
#include "../3rd_party/linenoise/linenoise.c"
//#include "../3rd_party/debugbreak/debugbreak.h"

typedef struct ui_state_t
{
    repl_mode_t parseMode;
} ui_state_t;

const char* Linenoise_GetInputLine(repl_state_t* repl, ui_state_t* state, uint32_t* length)
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

repl_mode_t Linenoise_QueryMode(ui_state_t* uiState)
{

}

const struct ui_interface_t LinenoiseUiInterface =
{
    Linenoise_GetInputLine,
    Linenoise_Message,
    Linenoise_QueryMode
} ;

int main(int argc, const char* argv[])
{
#ifndef NO_FIBERS
    aco_global_init();
#endif
    if (bc == 0)
    {
        bc = &BCGen_interface;
    }
    linenoiseHistoryLoad(".repl_history");
    printf("Please enter :h for help\n");
    repl_state_t repl;
    ui_state_t uiState = {0};

    repl_ui_context_t ctx;

    ctx.UiInterface = LinenoiseUiInterface;
    ctx.UiState = cast(void*)&uiState;

    g_uiContext = &ctx;
#ifndef NO_FIBERS
    worker_context_t replWorkerContext = {0};
    threadContext = &replWorkerContext;
    RunWorkerThread(&replWorkerContext, Repl_Fiber, 0);
#else
    ReplStart(&ctx);
#endif
    linenoiseHistorySave(".repl_history");
    return 1;
}

#endif
