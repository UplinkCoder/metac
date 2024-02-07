#define ACCEL ACCEL_TABLE

#ifndef CURSES_UI
#define LINENOISE_UI

#ifdef DEBUG_SERVER
#  include "../debug/debug_server.c"
#endif

#include "repl.c"
#include "../3rd_party/linenoise/linenoise.c"
//#include "../3rd_party/debugbreak/debugbreak.h"


typedef struct ui_state_t
{
    repl_mode_t parseMode;
} ui_state_t;

const char* Linenoise_GetInputLine(ui_state_t* state, repl_state_t* repl, uint32_t* lengthP)
{
    const char* line = "";
    uint32_t length = 0;

    ALIGN_STACK();
    line = linenoise(repl->Promt);
    RESTORE_STACK();

    if (line && *line)
    {
        length = strlen(line);
        linenoiseHistoryAdd(line);
    }

    (*lengthP) = length;
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

void Linenoise_ErrorMessage(ui_state_t* state, const char* fmt, ...)
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

static completion_cb_t s_completionCb;
static linenoiseCompletions s_linenoiseCompletions;
static repl_state_t* s_repl;

void linenoiseCompletionCallbackFn(const char *input, linenoiseCompletions * resultP)
{
    completion_list_t list = s_completionCb(s_repl, input, strlen(input));
    uint32_t beforeLen = list.BeforeCompletionLength;
    char LineBuffer[512];
    // the completion will take the whole line
    // so we have to assemble the previous line with the completion
    linenoiseCompletions result = {0};

    memcpy(LineBuffer, list.BeforeCompletion, beforeLen);

    for(uint32_t i = 0; i < list.CompletionsLength; i++)
    {
        strcpy(LineBuffer + beforeLen, list.Completions[i]);
        linenoiseAddCompletion(&result, LineBuffer);
    }

    (*resultP) = result;
}


void Linenoise_SetCompletionCallback(struct ui_state_t* state, repl_state_t* repl, completion_cb_t completionCb)
{
    s_repl = repl;
    s_completionCb = completionCb;
    linenoiseSetCompletionCallback(linenoiseCompletionCallbackFn);
}


repl_mode_t Linenoise_QueryMode(ui_state_t* uiState)
{
    return uiState->parseMode;
}


void Linenoise_Clear(ui_state_t* uiState)
{
    linenoiseClearScreen();
}


metac_filesystem_t* Linenoise_GetFilesystem(ui_state_t* uiState)
{
    return (metac_filesystem_t*)&NativeFileSystem;
}

const struct ui_interface_t LinenoiseUiInterface =
{
    Linenoise_GetInputLine,
    Linenoise_Message,
    Linenoise_ErrorMessage,
    Linenoise_QueryMode,
    Linenoise_Clear,
    Linenoise_Info,
    Linenoise_SetCompletionCallback,
    Linenoise_GetFilesystem,
} ;

void doUiMessage(const char* msg, void* context)
{
    g_uiContext->UiInterface->Message(g_uiContext->UiState, msg);
}

int main(int argc, const char* argv[])
{
#ifdef DEBUG_SERVER
    debug_server_t dbgSrv = {0};
#endif
    repl_state_t repl;
    ui_state_t uiState = {0};
    repl_ui_context_t ctx;

#ifndef NO_FIBERS
    aco_global_init();
#endif
#ifdef DEBUG_SERVER
    Debug_Init(&dbgSrv, 8180);
#endif
    ctx.UiInterface = &LinenoiseUiInterface;
    ctx.UiState = &uiState;

    linenoiseHistoryLoad(".repl_history");

#ifdef METAC_COMPILER_INTERFACE
    OS.GetTimeStamp(&compiler.StartTimeStamp);
    compiler.Message = doUiMessage;
#endif
    g_uiContext = &ctx;
#if !defined(NO_FIBERS)
    worker_context_t replWorkerContext = {0};
    worker_context_t* ctxPtr = &replWorkerContext;
    THREAD_CONTEXT_SET(ctxPtr);
/*
    {
        uint32_t slaveCount = 4;
        for(uint32_t slaveIdx = 0; slaveIdx < slaveCount; slaveIdx++)
        {

        }
    }
*/
    RunWorkerThread(&replWorkerContext, Repl_Fiber, 0);
#else
    ReplStart(&ctx);
#endif
    linenoiseHistorySave(".repl_history");
    return 1;
}

#endif
