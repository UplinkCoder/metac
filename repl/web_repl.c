#define ACCEL ACCEL_TABLE
#define WEB_REPL_BUILD 1

#ifdef DEBUG_SERVER
#include "../debug/debug_server.c"
#endif

#include "repl.c"

#include <stdarg.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define WEB_REPL_INPUT_CAPACITY 16
#define WEB_REPL_LINE_CAPACITY 4096
#define WEB_REPL_EVENT_CAPACITY 128
#define WEB_REPL_EVENT_TEXT_CAPACITY 2048

typedef struct web_repl_event_t
{
    uint64_t Id;
    const char* Kind;
    char Text[WEB_REPL_EVENT_TEXT_CAPACITY];
} web_repl_event_t;

typedef struct ui_state_t
{
    repl_mode_t ParseMode;
    mtx_t Lock;
    char InputQueue[WEB_REPL_INPUT_CAPACITY][WEB_REPL_LINE_CAPACITY];
    uint32_t InputRead;
    uint32_t InputWrite;
    uint32_t InputCount;
    char InputLine[WEB_REPL_LINE_CAPACITY];
    char Prompt[32];
    web_repl_event_t Events[WEB_REPL_EVENT_CAPACITY];
    uint32_t EventCount;
    uint64_t NextEventId;
} ui_state_t;

typedef struct web_repl_post_t
{
    size_t Length;
    bool TooLarge;
    char Data[WEB_REPL_LINE_CAPACITY];
} web_repl_post_t;

static ui_state_t* g_WebReplState;

static void WebRepl_Record(ui_state_t* state, const char* kind, const char* text)
{
    mtx_lock(&state->Lock);
    uint64_t id = state->NextEventId++;
    web_repl_event_t* event = &state->Events[(id - 1) % WEB_REPL_EVENT_CAPACITY];
    event->Id = id;
    event->Kind = kind;
    ALIGN_STACK();
    snprintf(event->Text, sizeof(event->Text), "%s", text ? text : "");
    RESTORE_STACK();
    if (state->EventCount < WEB_REPL_EVENT_CAPACITY)
        state->EventCount++;
    mtx_unlock(&state->Lock);
}

static void WebRepl_VMessage(ui_state_t* state, const char* kind,
                             const char* format, va_list args)
{
    char text[WEB_REPL_EVENT_TEXT_CAPACITY];
    ALIGN_STACK();
    vsnprintf(text, sizeof(text), format, args);
    RESTORE_STACK();
    WebRepl_Record(state, kind, text);
}

static const char* WebRepl_GetInputLine(ui_state_t* state, repl_state_t* repl,
                                        uint32_t* lengthP)
{
    const char* line = 0;
    *lengthP = 0;

    mtx_lock(&state->Lock);
    state->ParseMode = repl->ParseMode;
    ALIGN_STACK();
    snprintf(state->Prompt, sizeof(state->Prompt), "%s",
             repl->Prompt ? repl->Prompt : "EE>");
    RESTORE_STACK();
    if (state->InputCount)
    {
        ALIGN_STACK();
        snprintf(state->InputLine, sizeof(state->InputLine), "%s",
                 state->InputQueue[state->InputRead]);
        RESTORE_STACK();
        state->InputRead = (state->InputRead + 1) % WEB_REPL_INPUT_CAPACITY;
        state->InputCount--;
        *lengthP = (uint32_t)strlen(state->InputLine);
        line = state->InputLine;
    }
    mtx_unlock(&state->Lock);
    return line;
}

static void WebRepl_Message(ui_state_t* state, const char* format, ...)
{
    va_list args;
    va_start(args, format);
    WebRepl_VMessage(state, "output", format, args);
    va_end(args);
}

static void WebRepl_ErrorMessage(ui_state_t* state, const char* format, ...)
{
    va_list args;
    va_start(args, format);
    WebRepl_VMessage(state, "error", format, args);
    va_end(args);
}

static repl_mode_t WebRepl_QueryMode(ui_state_t* state)
{
    return state->ParseMode;
}

static void WebRepl_Clear(ui_state_t* state)
{
    WebRepl_Record(state, "clear", "");
}

static void WebRepl_Info(ui_state_t* state, const char* format, ...)
{
    va_list args;
    va_start(args, format);
    WebRepl_VMessage(state, "info", format, args);
    va_end(args);
}

static void WebRepl_SetCompletionCallback(ui_state_t* state, repl_state_t* repl,
                                          completion_cb_t completionCb)
{
    (void)state;
    (void)repl;
    (void)completionCb;
}

static metac_filesystem_t* WebRepl_GetFilesystem(ui_state_t* state)
{
    (void)state;
    return (metac_filesystem_t*)&NativeFileSystem;
}

static const ui_interface_t WebReplUiInterface =
{
    WebRepl_GetInputLine,
    WebRepl_Message,
    WebRepl_ErrorMessage,
    WebRepl_QueryMode,
    WebRepl_Clear,
    WebRepl_Info,
    WebRepl_SetCompletionCallback,
    WebRepl_GetFilesystem,
    0
};

static int WebRepl_Respond(struct MHD_Connection* connection, unsigned int status,
                           const char* contentType, const char* body, size_t length)
{
    struct MHD_Response* response = MHD_create_response_from_buffer(
        length, (void*)body, MHD_RESPMEM_MUST_COPY);
    if (!response)
        return MHD_NO;
    MHD_add_response_header(response, MHD_HTTP_HEADER_CONTENT_TYPE, contentType);
    MHD_add_response_header(response, "Cache-Control", "no-store");
    int result = MHD_queue_response(connection, status, response);
    MHD_destroy_response(response);
    return result;
}

static bool WebRepl_OriginMatchesHost(struct MHD_Connection* connection)
{
    const char* origin = MHD_lookup_connection_value(connection, MHD_HEADER_KIND, "Origin");
    const char* host = MHD_lookup_connection_value(connection, MHD_HEADER_KIND, "Host");
    if (!origin)
        return true;
    return host && strncmp(origin, "http://", 7) == 0
        && strcmp(origin + 7, host) == 0;
}

MHD_HANDLER(WebRepl_Page)
{
    (void)cls;
    (void)url;
    (void)method;
    (void)version;
    (void)upload_data;
    (void)upload_data_size;
    (void)con_cls;
    return serveFile("../debug/web_repl.html", "text/html; charset=utf-8", connection);
}

MHD_HANDLER(WebRepl_Events)
{
    (void)cls;
    (void)version;
    (void)upload_data;
    (void)upload_data_size;
    (void)con_cls;
    if (strcmp(method, "GET") != 0)
        return WebRepl_Respond(connection, 405, "text/plain; charset=utf-8", "GET required", 11);

    const char* afterValue = MHD_lookup_connection_value(connection,
        MHD_GET_ARGUMENT_KIND, "after");
    char* end = 0;
    uint64_t after = afterValue ? strtoull(afterValue, &end, 10) : 0;
    if (afterValue && (!end || *end))
        after = 0;

    json_object* root = json_object_new_object();
    json_object* events = json_object_new_array();
    bool dropped = false;

    mtx_lock(&g_WebReplState->Lock);
    uint64_t oldest = g_WebReplState->NextEventId - g_WebReplState->EventCount;
    uint64_t latest = g_WebReplState->NextEventId - 1;
    if (after > latest)
        after = latest;
    if (after + 1 < oldest)
    {
        after = oldest - 1;
        dropped = true;
    }
    for (uint64_t id = after + 1; id <= latest; id++)
    {
        web_repl_event_t* event =
            &g_WebReplState->Events[(id - 1) % WEB_REPL_EVENT_CAPACITY];
        if (event->Id != id)
            continue;
        json_object* item = json_object_new_object();
        json_object_object_add(item, "id", json_object_new_int64((int64_t)event->Id));
        json_object_object_add(item, "kind", json_object_new_string(event->Kind));
        json_object_object_add(item, "text", json_object_new_string(event->Text));
        json_object_array_add(events, item);
    }
    json_object_object_add(root, "prompt", json_object_new_string(g_WebReplState->Prompt));
    mtx_unlock(&g_WebReplState->Lock);

    json_object_object_add(root, "dropped", json_object_new_boolean(dropped));
    json_object_object_add(root, "events", events);
    const char* body = json_object_to_json_string(root);
    int result = WebRepl_Respond(connection, MHD_HTTP_OK,
        "application/json; charset=utf-8", body, strlen(body));
    json_object_put(root);
    return result;
}

MHD_HANDLER(WebRepl_Input)
{
    (void)cls;
    (void)version;
    if (strcmp(method, "POST") != 0)
        return WebRepl_Respond(connection, 405, "text/plain; charset=utf-8", "POST required", 12);
    if (!WebRepl_OriginMatchesHost(connection))
        return WebRepl_Respond(connection, 403, "text/plain; charset=utf-8", "Origin rejected", 15);

    web_repl_post_t* post = (web_repl_post_t*)*con_cls;
    if (!post)
    {
        post = (web_repl_post_t*)calloc(1, sizeof(*post));
        if (!post)
            return MHD_NO;
        *con_cls = post;
        return MHD_YES;
    }

    if (*upload_data_size)
    {
        if (*upload_data_size > sizeof(post->Data) - post->Length - 1)
            post->TooLarge = true;
        else if (!post->TooLarge)
        {
            memcpy(post->Data + post->Length, upload_data, *upload_data_size);
            post->Length += *upload_data_size;
        }
        *upload_data_size = 0;
        return MHD_YES;
    }

    *con_cls = 0;
    if (post->TooLarge)
    {
        free(post);
        return WebRepl_Respond(connection, 413, "text/plain; charset=utf-8",
                              "Input exceeds 4095 bytes", 25);
    }
    if (!post->Length || memchr(post->Data, '\0', post->Length))
    {
        free(post);
        return WebRepl_Respond(connection, 400, "text/plain; charset=utf-8",
                              "Input must be non-empty text", 28);
    }
    post->Data[post->Length] = '\0';

    mtx_lock(&g_WebReplState->Lock);
    if (g_WebReplState->InputCount == WEB_REPL_INPUT_CAPACITY)
    {
        mtx_unlock(&g_WebReplState->Lock);
        free(post);
        return WebRepl_Respond(connection, 429, "text/plain; charset=utf-8",
                              "REPL input queue is full", 24);
    }
    snprintf(g_WebReplState->InputQueue[g_WebReplState->InputWrite],
             WEB_REPL_LINE_CAPACITY, "%s", post->Data);
    g_WebReplState->InputWrite =
        (g_WebReplState->InputWrite + 1) % WEB_REPL_INPUT_CAPACITY;
    g_WebReplState->InputCount++;
    mtx_unlock(&g_WebReplState->Lock);
    free(post);
    return WebRepl_Respond(connection, MHD_HTTP_OK, "text/plain; charset=utf-8", "queued", 6);
}

int main(int argc, const char* argv[])
{
    unsigned long portValue = 8181;
    if (argc > 1)
    {
        char* end = 0;
        portValue = strtoul(argv[1], &end, 10);
        if (!end || *end || portValue == 0 || portValue > 65535)
        {
            fprintf(stderr, "Usage: %s [port 1-65535]\n", argv[0]);
            return 2;
        }
    }

    debug_server_t debugServer = {0};
    ui_state_t uiState = {0};
    repl_ui_context_t context;
    worker_context_t replWorker = {0};

    if (mtx_init(&uiState.Lock, mtx_plain) != thrd_success)
    {
        fprintf(stderr, "Could not initialize web REPL lock\n");
        return 1;
    }
    uiState.NextEventId = 1;
    snprintf(uiState.Prompt, sizeof(uiState.Prompt), "EE>");
    g_WebReplState = &uiState;

    aco_global_init();
    if (Debug_Init(&debugServer, (unsigned short)portValue) != 0)
    {
        mtx_destroy(&uiState.Lock);
        return 1;
    }
    fprintf(stderr, "MetaC Web REPL listening at http://127.0.0.1:%lu/repl\n",
            portValue);
    context.UiInterface = &WebReplUiInterface;
    context.UiState = &uiState;
    g_uiContext = &context;

    THREAD_CONTEXT_SET(&replWorker);
    RunWorkerThread(&replWorker, Repl_Fiber, 0);

    mtx_destroy(&uiState.Lock);
    return 0;
}