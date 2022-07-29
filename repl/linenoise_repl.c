#define ACCEL ACCEL_TABLE

#ifndef CURSES_UI
#define LINENOISE_UI

#include "repl.c"
#include "../3rd_party/linenoise/linenoise.c"
//#include "../3rd_party/debugbreak/debugbreak.h"

typedef struct ui_state_t
{
    parse_mode_t parseMode;
} ui_state_t;

const char* Linenoise_GetInputLine(repl_state_t* repl, ui_state_t* state, uint32_t* length)
{
    const char* line = linenoise(repl->Promt);

    if (line)
    {
        *length = strlen(line);
    }
    else
    {
        *length = 0;
    }

    return line;
}

void Linenoise_Message(ui_state_t* state, const char* fmt, ...)
{

}

parse_mode_t Linenoise_QueryMode(ui_state_t* uiState)
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

    repl_state_t repl;
    Repl_Init(&repl);

    ui_state_t uiState = {0};

    repl_ui_context_t ctx = {
        LinenoiseUiInterface, cast(void*)&uiState
    };
//    ui_state_t uiState;
//    UiState_Init(&uiState);
/*
    printf("Transfered decl: %s\n",
        MetaCPrinter_PrintDeclaration(&printer, compilerStruct));
*/

    // only here can we destroy tmpSema



/*
    metac_dot_printer_t dot_printer;
    MetaCDotPrinter_Init(&dot_printer, &g_lineParser.IdentifierTable);
    g_lineParser.DotPrinter = &dot_printer;
*/
#ifndef NO_FIBERS
    worker_context_t replWorkerContext = {0};
    threadContext = &replWorkerContext;
    RunWorkerThread(&replWorkerContext, ReplStart, &ctx);
#else
    ReplStart(&ctx);
#endif
    return 1;
}

#endif
