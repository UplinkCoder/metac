#ifdef DEBUG_SERVER
#include "../debug/debug_server.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "../os/bsf.h"
#include <json-c/json.h>
#include <assert.h>
#if defined(DEBUG_SERVER_LOOPBACK) && !defined(_WIN32)
#include <netinet/in.h>
#endif

extern uint32_t crc32c_nozero(uint32_t crc, const void* s, const uint32_t len_p);

#if defined(WEB_REPL_BUILD)
MHD_HANDLER(WebRepl_Page);
MHD_HANDLER(WebRepl_Events);
MHD_HANDLER(WebRepl_Input);
#endif

debug_server_t* g_DebugServer = 0;

#ifndef ARRAYSIZE
#define ARRAYSIZE(ARR) \
    (sizeof ((ARR)) / sizeof(*(ARR)) / !(sizeof((ARR)) % sizeof(*(ARR))))
#endif

#ifndef WIN32
int fopen_s (FILE ** fileP, const char *path, const char *mode)
{
    (*fileP) = fopen (path, mode);
    return (*fileP != 0);
}
#endif

int send_html(struct MHD_Connection* connection, const char* text, int len)
{
    struct MHD_Response* response;
    int ret;
    response =
        MHD_create_response_from_buffer(len, cast(char*)text, MHD_RESPMEM_PERSISTENT);
    MHD_add_response_header(response, MHD_HTTP_HEADER_CONTENT_TYPE,
        "text/html");
    ret = MHD_queue_response(connection, MHD_HTTP_OK, response);
    MHD_destroy_response(response);
    return ret;
}

int send_html_free(struct MHD_Connection* connection, const char* text, int len)
{
    struct MHD_Response* response;
    int ret;
    response =
        MHD_create_response_from_buffer(len, cast(char*)text, MHD_RESPMEM_MUST_FREE);
    MHD_add_response_header(response, MHD_HTTP_HEADER_CONTENT_TYPE,
        "text/html");
    ret = MHD_queue_response(connection, MHD_HTTP_OK, response);
    MHD_destroy_response(response);
    return ret;
}

int send_page(struct MHD_Connection* connection, const char* title,
              const char* body, int bodyLen)
{
    static const char* pageFormat =
        "<!doctype html><html lang=\"en\"><head><meta charset=\"utf-8\">"
        "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">"
        "<title>%s</title><link rel=\"stylesheet\" href=\"/theme.css\"></head>"
        "<body><main class=\"page-shell\">%.*s</main></body></html>";
    size_t capacity = strlen(pageFormat) + strlen(title) + bodyLen + 1;
    char* page = (char*)malloc(capacity);
    int len = snprintf(page, capacity, pageFormat, title, bodyLen, body);
    return send_html_free(connection, page, len);
}

MHD_HANDLER(handleTheme)
{
    static const char* css =
        ":root{color-scheme:dark;--bg:#272822;--panel:#1e1f1c;--surface:#33342e;"
        "--border:#49483e;--text:#f8f8f2;--muted:#a6a69c;--yellow:#e6db74;"
        "--green:#a6e22e;--pink:#f92672;--cyan:#66d9ef;--orange:#fd971f;"
        "--purple:#ae81ff}*{box-sizing:border-box}body{margin:0;background:var(--bg);"
        "color:var(--text);font:14px/1.55 'Cascadia Code','SFMono-Regular',Consolas,"
        "monospace}.page-shell{width:min(1120px,100%);margin:0 auto;padding:32px 28px 48px}"
        "h1{margin:0 0 22px;color:var(--yellow);font-size:21px;font-weight:650}"
        "h2,h3{color:var(--cyan);font-size:15px}p{color:var(--muted)}a{color:var(--cyan);"
        "text-decoration:none}a:hover{color:var(--yellow);text-decoration:underline}"
        "table{width:100%;border-collapse:collapse;background:var(--panel);font-size:13px}"
        "th,td{padding:9px 12px;border:1px solid var(--border);text-align:left;"
        "overflow-wrap:anywhere}th{background:var(--surface);color:var(--yellow);font-weight:600}"
        "tr:nth-child(even) td{background:#292a25}"
        ".route-list{display:grid;grid-template-columns:repeat(auto-fit,minmax(180px,1fr));"
        "gap:8px;list-style:none;padding:0}.route-list a{display:block;padding:10px 12px;"
        "background:var(--panel);border:1px solid var(--border)}"
        ".parser-meta{display:grid;grid-template-columns:repeat(auto-fit,minmax(220px,1fr));"
        "gap:10px;margin-bottom:20px}.meta-item,.stack-panel{padding:12px 14px;"
        "background:var(--panel);border:1px solid var(--border)}.meta-label{display:block;"
        "color:var(--muted);font-size:12px;margin-bottom:4px}.meta-value{color:var(--yellow);"
        "overflow-wrap:anywhere}.stack-grid{display:grid;grid-template-columns:repeat(2,minmax(0,1fr));"
        "gap:12px}.stack-panel h2{margin:0 0 10px}.stack-list{display:flex;flex-wrap:wrap;"
        "gap:7px;min-height:38px;list-style:none;margin:0;padding:0}.stack-list li{padding:5px 9px;"
        "border:1px solid var(--border);background:var(--surface);color:var(--text)}"
        ".selected{background:var(--green)!important;color:var(--panel)!important;"
        "border-color:var(--green)!important}.highlight{background:var(--surface);"
        "color:var(--pink);padding:2px 4px}.button-row{display:flex;gap:8px;margin-top:16px}"
        "button{padding:8px 13px;border:1px solid var(--green);background:var(--green);"
        "color:var(--panel);font:inherit;font-weight:600;cursor:pointer}button:hover{"
        "background:var(--yellow);border-color:var(--yellow)}button:focus-visible,a:focus-visible{"
        "outline:2px solid var(--cyan);outline-offset:3px}#holder{width:100%;height:min(62vh,520px);"
        "min-height:320px;background:var(--panel);border:1px solid var(--border)}"
        "#holder text{fill:var(--text)!important}"
        "@media(max-width:640px){.page-shell{padding:22px 14px 32px}.stack-grid{grid-template-columns:1fr}"
        "table{font-size:12px}th,td{padding:7px 8px}#holder{height:380px;min-height:300px}}";
    struct MHD_Response* response = MHD_create_response_from_buffer(
        strlen(css), (void*)css, MHD_RESPMEM_PERSISTENT);
    MHD_add_response_header(response, MHD_HTTP_HEADER_CONTENT_TYPE, "text/css");
    int ret = MHD_queue_response(connection, MHD_HTTP_OK, response);
    MHD_destroy_response(response);
    return ret;
}

int serveFile(const char* filename, const char* contentType, struct MHD_Connection* conn)
{
    FILE* f;
    char pageBuffer[8192];
    int sz;
    char* page;

    struct MHD_Response* response;
    int ret;

    fopen_s(&f, filename, "r");
    if (f)
    {
        fseek(f, 0, SEEK_END);
        sz = ftell(f);
        fseek(f, 0, SEEK_SET);
        if (sz < 8192)
            page = pageBuffer;
        else
            page = (char*)malloc(sz + 1);

        fread(page, 1, sz, f);
        page[sz] = '\0';
        // printf("page: %s\n", page);
        fclose(f);


        response =
            MHD_create_response_from_buffer(sz, (void*)page,
                MHD_RESPMEM_MUST_COPY);
        MHD_add_response_header(response, MHD_HTTP_HEADER_CONTENT_TYPE,
            contentType);
    }
    else
    {
        char msg[512];
        int len = 0;
        len += snprintf(msg, 512, "File %s couldn't be loaded.", filename);
        response =
            MHD_create_response_from_buffer(len, (void*)msg,
                MHD_RESPMEM_MUST_COPY);
    }


    ret = MHD_queue_response(conn, MHD_HTTP_OK, response);
    MHD_destroy_response(response);

    return ret;
}

static uint32_t g_allocated;

MHD_HANDLER (handleCurrentScope)
{
    static char responseString[8192];
    uint32_t responseSize = 0;
    debug_server_t* debugServer = (debug_server_t*) cls;
    if (!debugServer->CurrentScope)
    {
        const char* body = "<h1>Current scope</h1><p>No current scope set.</p>";
        return send_page(connection, "Current scope", body, strlen(body));
    }

    responseSize +=
        snprintf(responseString, ARRAYSIZE(responseString) - responseSize,
            "<h1>Current scope</h1><ul>");
    metac_scope_table_slot_t* slot = debugServer->CurrentScope->ScopeTable.Slots;
    for(uint32_t i = 0; i < debugServer->CurrentScope->ScopeTable.SlotsUsed; i++, slot++)
    {
        if (slot->Hash == 0)
            continue;
        else
        {
            metac_identifier_ptr_t idPtr = slot->Ptr;
            responseSize +=
                snprintf(responseString, ARRAYSIZE(responseString) - responseSize,
                        "<li>%s</li>", IdentifierPtrToCharPtr(
                            debugServer->CurrentIdentifierTable, idPtr));
        }
    }

    responseSize +=
        snprintf(responseString, ARRAYSIZE(responseString) - responseSize,
                "</ul>");
            return send_page(connection, "Current scope", responseString, responseSize);
}

// Sample history data for demonstration purposes
char* parserHistory[] = {
    "{\"tokenStream\": \"+ 3 * ( 4 - 2 )\", \"position\": 0, \"expressionStack\": [\"4\", \"2\"], \"operatorStack\": [\"+\", \"*\"], \"selectedExpressionIndex\": -1, \"selectedOperatorIndex\": -1}",
    "{\"tokenStream\": \"+ 3 * ( 4 - 2 )\", \"position\": 1, \"expressionStack\": [\"4\", \"2\", \"3\"], \"operatorStack\": [\"+\", \"*\"], \"selectedExpressionIndex\": 2, \"selectedOperatorIndex\": -1}",
    "{\"tokenStream\": \"+ 3 * ( 4 - 2 )\", \"position\": 2, \"expressionStack\": [\"6\", \"3\"], \"operatorStack\": [\"+\"], \"selectedExpressionIndex\": 0, \"selectedOperatorIndex\": -1}"
};

MHD_HANDLER(handleHistory)
{
    assert(strcmp(url, "/history") == 0 && strcmp(method, "GET") == 0);

    {
        // Create a JSON array and populate it with the history data
        json_object* historyArray = json_object_new_array();
        int numItems = sizeof(parserHistory) / sizeof(parserHistory[0]);

        for (int i = 0; i < numItems; i++)
        {
            json_object* historyObj = json_tokener_parse(parserHistory[i]);
            json_object_array_add(historyArray, historyObj);
        }

        // Convert the JSON array to a string
        const char* response = json_object_to_json_string(historyArray);

        // Set the HTTP response headers
        struct MHD_Response* mhdResponse = MHD_create_response_from_buffer(strlen(response), (void*)response,
                                                                           MHD_RESPMEM_MUST_COPY);
        MHD_add_response_header(mhdResponse, "Content-Type", "application/json");
        int ret = MHD_queue_response(connection, MHD_HTTP_OK, mhdResponse);

        // Cleanup
        MHD_destroy_response(mhdResponse);
        json_object_put(historyArray);

        return ret;
    }

    // Invalid URL or method
    return MHD_NO;
}

MHD_HANDLER(handleGraphJs)
{
    return serveFile("/home/uplink/dev/metac/debug/graph.js", "application/javascript", connection);
}

MHD_HANDLER(handleThemeCss)
{
    return handleTheme(MHD_HANDLER_PASSTHROUGH);
}

MHD_HANDLER(handleParser)
{
    return serveFile("/home/uplink/dev/metac/debug/parser.html", "text/html", connection);
}

MHD_HANDLER(handleLc)
{
    return serveFile("/home/uplink/dev/metac/debug/linechart.html", "text/html", connection);
}

MHD_HANDLER(handleDataJson)
{
    return serveFile("/home/uplink/dev/metac/debug/data.json", "application/json", connection);
}


MHD_HANDLER(handleGraphs)
{
    static char responseString[8192];
    uint32_t responseSize = 0;
    debug_server_t* debugServer = (debug_server_t*) cls;

}

const char* PrintSize(uint32_t sz)
{
    static char s_buffer[32];
    int suffixIdx = 0;

    static const char suffix[] =
        { 'b','k','m','g' };

    while (sz > 1024)
    {
        sz /= 1024;
        suffixIdx++;
    }

    snprintf(s_buffer, 32, "%u %c", sz, suffix[suffixIdx]);

    return s_buffer;
}

void outAllocRow(char* body, uint32_t sz, uint32_t* pp, metac_alloc_t* alloc)
{
    uint32_t p = *pp;
    uint32_t j;
    uint32_t usedSize = 0;
    uint32_t allocatedSize = 0;
    uint32_t arenasUsed = 0;

    p += snprintf (body + p, sz - p, "<tr>");

    for(j = 0; j < alloc->ArenasCount; j++)
    {
        usedSize += alloc->Arenas[j].Offset;
        // we seem to be double counting allocated size
        allocatedSize += (alloc->Arenas[j].Offset + alloc->Arenas[j].SizeLeft);
        if (alloc->Arenas[j].Offset)
        {
            arenasUsed++;
        }
    }

    {
        p += snprintf (body + p, sz - p,
                       "<td>%p</td>", alloc
        );

        p += snprintf (body + p, sz - p,
                       "<td>%s</td>", (alloc->File ? alloc->File : "(null)")
        );

        p += snprintf (body + p, sz - p,
                       "<td>%u</td>", alloc->Line
        );

        p += snprintf (body + p, sz - p,
                       "<td>%u</td>", alloc->ArenasCount
        );

        p += snprintf (body + p, sz - p,
                       "<td>%u</td>", arenasUsed
        );

        p += snprintf (body + p, sz - p,
                       "<td>%s</td>", PrintSize(usedSize)
        );

        p += snprintf (body + p, sz - p,
                       "<td>%s</td>", PrintSize(allocatedSize)
        );
    }

    p += snprintf(body + p, sz - p, "</tr>");

    (*pp) = p;
}

MHD_HANDLER (handleAllocators)
{
    static char responseString[8192];
    debug_server_t* debugServer = (debug_server_t*) cls;
    int i;
    uint32_t p = 0;
    int len;
    int n_fields = 0;

    char body[8192];
    body[0] = '\0';

    const char* headers[] = {
        "Alloc_Addr",
        "File",
        "Line",
        "ArenaCount",
        "AreansUsed",
        "Used",
        "Allocated"
    };

    p += snprintf(body + p, ARRAYSIZE(body) - p, "<tr>");
    for(i = 0; i < ARRAYSIZE(headers); i++)
    {
        p += snprintf(body + p, ARRAYSIZE(body) - p, "<th>%s</th>", headers[i]);
    }
    p += snprintf(body + p, ARRAYSIZE(body) - p, "</tr>");
    for (i = 0; i < debugServer->AllocatorsCount; i++)
    {
        metac_alloc_t * alloc = debugServer->Allocators[i];
        if (alloc->ArenasCount > 500)
        {
            fprintf(stderr, "Unreasonable ArenaCount for alloc %u\n", i);
            continue;
        }
        outAllocRow(body, ARRAYSIZE(body), &p, alloc);
    }

    len = snprintf (responseString, ARRAYSIZE (responseString),
                          "<h1>Allocators</h1>"
                          "<table id=\"allocators\">%s</table>",
           body);
     return send_page (connection, "Allocators", responseString, len);
}

void outArenaRow(char* body, uint32_t sz, uint32_t* pp, tagged_arena_t* arena)
{
    uint32_t p = *pp;

    p += snprintf (body + p, sz - p, "<tr>");
    {
        p += snprintf (body + p, sz - p,
                       "<td>%p</td>", arena
        );

        p += snprintf (body + p, sz - p,
                       "<td>%s</td>", (arena->File ? arena->File : "(null)")
        );

        p += snprintf (body + p, sz - p,
                       "<td>%u</td>", arena->Line
        );

        p += snprintf (body + p, sz - p,
                       "<td>%s</td>", PrintSize(arena->Offset)
        );

        p += snprintf (body + p, sz - p,
                       "<td>%s</td>", PrintSize(arena->SizeLeft)
        );
    }

    p += snprintf(body + p, sz - p, "</tr>");
    (*pp) = p;
}

MHD_HANDLER(handleArenas)
{
    static char responseBuffer[8192];
    uint32_t p = 0;
    const char* headers[] = {
        "Arena_Addr",
        "File",
        "Line",
        "Offset",
        "SizeLeft",
    };
    char body[8192];

    debug_server_t *debugServer = cast(debug_server_t*) cls;
    const char *allocIdxStr =
        MHD_lookup_connection_value(connection, MHD_GET_ARGUMENT_KIND, "allocIdx");
    uint32_t allocIdx = allocIdxStr ? atoi(allocIdxStr) : 0;
    metac_alloc_t* alloc = ((allocIdx && allocIdx <= (debugServer->AllocatorsCount))
        ? debugServer->Allocators[allocIdx - 1] : 0);

    const uint32_t arenaCount = alloc ? alloc->ArenasCount : 0;

    p += snprintf(body + p, ARRAYSIZE(body) - p, "<tr>");
    for(uint32_t i = 0; i < ARRAYSIZE(headers); i++)
    {
        p += snprintf(body + p, ARRAYSIZE(body) - p, "<th>%s</th>", headers[i]);
    }
    p += snprintf(body + p, ARRAYSIZE(body) - p, "</tr>");


    for(uint32_t i = 0; i < arenaCount; i++)
    {
        tagged_arena_t* arena = &alloc->Arenas[i];
        outArenaRow(body, ARRAYSIZE(body), &p, arena);
    }

    if (!alloc)
        return MHD_NO;

    uint32_t len = snprintf (responseBuffer, ARRAYSIZE (responseBuffer),
                          "<h1>Arenas</h1>"
                          "<table id=\"arenas\">%s</table>",
           body);
     return send_page (connection, "Arenas", responseBuffer, len);
}

#ifndef NO_FIBERS
void outTaskRow(char* body, uint32_t sz, uint32_t* pp, task_t* task)
{
/*
        const char* headers[] = {
        "Task_Addr",
        "Name",
        "File",
        "Line",
        "Yield Reason",
        "Yield Loc",
        "Parent_Addr",
        "Parent_Name",
    };
*/
    uint32_t p = *pp;

    p += snprintf (body + p, sz - p, "<tr>");
    {
        p += snprintf (body + p, sz - p,
                       "<td>%p</td>", task
        );

        p += snprintf (body + p, sz - p,
                       "<td>%p</td>", task->TaskFunction
        );

        p += snprintf (body + p, sz - p,
                       "<td>%s</td>", (task->Origin.File ? task->Origin.File : "(null)")
        );

        p += snprintf (body + p, sz - p,
                       "<td>%u</td>", task->Origin.Line
        );

        p += snprintf (body + p, sz - p,
                       "<td>%s</td>", task->YieldReason
        );

        p += snprintf (body + p, sz - p,
                       "<td>%s</td>", task->YieldLoc
        );

        p += snprintf (body + p, sz - p,
                       "<td>%p</td>", task->Parent
        );

        p += snprintf (body + p, sz - p,
                       "<td>%p</td>", (task->Parent ? task->Parent->TaskFunction : 0)
        );
    }

    p += snprintf(body + p, sz - p, "</tr>");
    (*pp) = p;
}
#endif

MHD_HANDLER(handleLogs)
{
    uint32_t bufferSz = KILOBYTES(256);
    char* body = malloc(bufferSz);
    uint32_t p = 0;
    const char* headers[] = {
        // "Time",
        "Category",
        "Message",
    };

    debug_server_t *debugServer = cast(debug_server_t*) cls;
    p += snprintf (body + p, bufferSz - p, "<h1>Logs</h1>"
              "<table id=\"logs\">"
    );

    p += snprintf(body + p, bufferSz - p, "<tr>");
    for(uint32_t i = 0; i < ARRAYSIZE(headers); i++)
    {
        p += snprintf(body + p, bufferSz - p, "<th>%s</th>", headers[i]);
    }
    p += snprintf(body + p, bufferSz - p, "</tr>");
    {
        uint32_t i;
        for(i = 0; i < debugServer->LogsCount; i++)
        {
            debug_message_t log = debugServer->Logs[i];
            const char* catString = IdentifierPtrToCharPtr(
                &debugServer->CategoryTable,
                log.Category);

            p += snprintf(body + p, bufferSz - p, "<tr>");

            p += snprintf(body + p, bufferSz - p, "<td>%s</td>", catString);
            p += snprintf(body + p, bufferSz - p, "<td>%s</td>", log.Message);

            p += snprintf(body + p, bufferSz - p, "</tr>");
        }
    }

    p += snprintf(body + p, bufferSz - p, "</table>");
        int ret = send_page (connection, "Logs", body, p);
        free(body);
        return ret;
}

MHD_HANDLER(handleTasks)
{
    static char responseBuffer[8192];
    uint32_t p = 0;
    const char* headers[] = {
        "Task_Addr",
        "Name",
        "File",
        "Line",
        "Yield Reason",
        "Yield Location",
        "Parent_Addr",
        "Parent_Name",
    };
    char body[8192];

    debug_server_t *debugServer = cast(debug_server_t*) cls;
#ifndef NO_FIBERS
    const char *workerIdxStr =
        MHD_lookup_connection_value(connection, MHD_GET_ARGUMENT_KIND, "workerIdx");
    uint32_t workerIdx = workerIdxStr ? atoi(workerIdxStr) : 0;
    worker_context_t* worker = ((workerIdx && workerIdx <= (debugServer->WorkersCount))
        ? debugServer->Workers[workerIdx - 1] : 0);

    const uint32_t freeBitfield = worker ? worker->FiberPool->FreeBitfield : ~0;
    const uint32_t inProgress = __builtin_popcount(~freeBitfield);

    p += snprintf(body + p, ARRAYSIZE(body) - p, "<tr>");
    for(uint32_t i = 0; i < ARRAYSIZE(headers); i++)
    {
        p += snprintf(body + p, ARRAYSIZE(body) - p, "<th>%s</th>", headers[i]);
    }
    p += snprintf(body + p, ARRAYSIZE(body) - p, "</tr>");
#define CLEAR_BIT(BF, IDX) \
    BF &= ~(1 << IDX);
#define SET_BIT(BF, IDX) \
    BF |= (1 << IDX);

    {
        uint32_t inProgressBf = freeBitfield;
        uint32_t currentIdx;
        while (inProgressBf != ~0)
        {
            currentIdx = BSF(inProgressBf) - 1;
            task_t* task = &worker->FiberPool->Tasks[currentIdx];
            outTaskRow(body, ARRAYSIZE(body), &p, task);
            SET_BIT(inProgressBf, currentIdx);
        }
    }

    task_t* activeTask = worker ? worker->ActiveTask : 0;

    uint32_t len = snprintf (responseBuffer, ARRAYSIZE (responseBuffer),
                    "<h1>Worker (%u/%u)</h1>"
                    "<h2>Active task: %p '%s'</h2>"
                    "<table id=\"tasks\">%s</table>",
           workerIdx, debugServer->WorkersCount,
           activeTask, activeTask ? activeTask->TaskName : "None",
           body);
#else
    uint32_t len = snprintf (responseBuffer, ARRAYSIZE (responseBuffer),
                    "<h1>Tasks</h1><p>No task support (compiled with NO_FIBERS set).</p>");

#endif
    return send_page (connection, "Tasks", responseBuffer, len);
}

static MHD_HANDLER(debugServerHandler)
{
    debug_server_t *debugServer = cast(debug_server_t*) cls;

    uint32_t i;
    for(i = 0; i < debugServer->RoutesCount;i++)
    {
        debug_server_route_t route = debugServer->Routes[i];
        if (strcmp(route.Url, url) == 0)
        {
            return route.Handler(MHD_HANDLER_PASSTHROUGH);
        }
    }

    {
        static char defaultPage[1024];
        int p = 0;
        p += sprintf(defaultPage + p, "<h1>Debug server</h1><ul class=\"route-list\">");
        {
            int i;
            for(i = 0; i < debugServer->RoutesCount; i++)
            {
                debug_server_route_t route = debugServer->Routes[i];
                const char* url = route.Url;
                p += sprintf(defaultPage + p, "<li><a href=\"%s\">%s</a></li>", url, url);
            }
        }
        p += sprintf(defaultPage + p, "</ul>");
        return send_page(connection, "Debug server", defaultPage, p);
    }

/*
    if (debugServer->Handler)
    {
        return debugServer->Handler(MHD_HANDLER_PASSTHROUGH);
    }

    return MHD_NO;
*/
}

static MHD_COMPLETED_CB (MhdCompletionCallback)
{
    if (con_cls && *con_cls)
    {
        free(*con_cls);
        *con_cls = 0;
    }
}

const char* DebugServer_AddString(debug_server_t* debugServer,
                                  const char* name, uint32_t len)
{
    char* memory = Allocator_Calloc(&debugServer->Allocator, char, len);
    memcpy(memory, name, len);
    return memory;
}


void DebugServer_AddRoute(debug_server_t* debugServer,
                          const char* url,
                          mhd_handler_t handler)
{
    debug_server_route_t route;

    route.Url = url; //DebugServer_AddString(debugServer, url, strlen(url));
    route.Handler = handler;

    ARENA_ARRAY_ADD(debugServer->Routes, route);
}

void debug_init_routes(debug_server_t* debugServer)
{
    DebugServer_AddRoute(debugServer, "/allocators", handleAllocators);
    DebugServer_AddRoute(debugServer, "/arenas", handleArenas);
    DebugServer_AddRoute(debugServer, "/graphs", handleGraphs);
    DebugServer_AddRoute(debugServer, "/graph.js", handleGraphJs);
    DebugServer_AddRoute(debugServer, "/theme.css", handleThemeCss);
    DebugServer_AddRoute(debugServer, "/lc", handleLc);
    DebugServer_AddRoute(debugServer, "/data.json", handleDataJson);
    // DebugServer_AddRoute(debugServer, "/identifier_tables", handleIdentifierTables);
    DebugServer_AddRoute(debugServer, "/parser", handleParser);
    DebugServer_AddRoute(debugServer, "/history", handleHistory);
    DebugServer_AddRoute(debugServer, "/logs", handleLogs);
    DebugServer_AddRoute(debugServer, "/tasks", handleTasks);
#if defined(WEB_REPL_BUILD)
    DebugServer_AddRoute(debugServer, "/repl", WebRepl_Page);
    DebugServer_AddRoute(debugServer, "/repl/events", WebRepl_Events);
    DebugServer_AddRoute(debugServer, "/repl/input", WebRepl_Input);
#endif
}


int Debug_Init(debug_server_t* debugServer, unsigned short port) {
    struct MHD_Daemon *d;
    uint32_t allocatorCapa = 64;
    uint32_t allocationCapa = 256;
    uint32_t graphCapa = 16;
    uint32_t tokenCapa = 128;
    uint32_t identifierTableCapa = 16;

#ifdef WIN32
    WSADATA wd;
    if (WSAStartup (MAKEWORD (2, 2), &wd) != 0)
    {
        fprintf (stderr, "FATAL: failed to initialize Windows Sockets.\n");
        return 1;
    }
#endif    /**/

#if defined(DEBUG_SERVER_LOOPBACK)
        struct sockaddr_in loopbackAddress = {0};
        loopbackAddress.sin_family = AF_INET;
        loopbackAddress.sin_port = htons(port);
        loopbackAddress.sin_addr.s_addr = htonl(INADDR_LOOPBACK);
        d = MHD_start_daemon (MHD_USE_SELECT_INTERNALLY, port,
                                                    NULL, NULL,
                                                    debugServerHandler, debugServer,
                                                    MHD_OPTION_SOCK_ADDR, &loopbackAddress,
                                                    MHD_OPTION_NOTIFY_COMPLETED,
                                                        MhdCompletionCallback, 0,
                                                    MHD_OPTION_END);
#else
        d = MHD_start_daemon (MHD_USE_SELECT_INTERNALLY, port,
                                                    NULL, NULL,
                                                    debugServerHandler, debugServer,
                                                    MHD_OPTION_NOTIFY_COMPLETED,
                                                        MhdCompletionCallback, 0,
                                                    MHD_OPTION_END);
#endif

    if (!d)
    {
        fprintf(stderr, "MicroHTTPD failed to bind port %u\n", port);
        return 1;
    }
    debugServer->Daemon = d;

    assert(!g_DebugServer);
    g_DebugServer = debugServer;

    debugServer->Allocators = (metac_alloc_t**)
        malloc(sizeof(metac_alloc_t*) * allocatorCapa);
    debugServer->AllocatorsCount = 0;
    debugServer->AllocatorsCapacity = allocatorCapa;

    debugServer->Allocations = (debug_allocation_t*)
        malloc(sizeof(debug_allocation_t) * allocationCapa);
    debugServer->AllocationsCount = 0;
    debugServer->AllocationsCapacity = allocationCapa;

    Allocator_Init(&debugServer->Allocator, 0);

    debugServer->Graphs = (debug_graph_t*)
        malloc(sizeof(debug_graph_t) * graphCapa);
    debugServer->GraphsCount = 0;
    debugServer->GraphsCapacity = graphCapa;

    debugServer->IdentifierTables = (metac_identifier_table_t*)
        malloc(sizeof(metac_identifier_table_t) * identifierTableCapa);
    debugServer->IdentifierTableCount = 0;
    debugServer->IdentifierTableCapacity = identifierTableCapa;

    debugServer->TokenStream = (metac_token_t*)
        malloc(sizeof(metac_token_t) * tokenCapa);
    debugServer->TokenStreamCount = 0;
    debugServer->TokenStreamCapacity = tokenCapa;
#ifndef NO_FIBERS
    ARENA_ARRAY_INIT(worker_context_t*, debugServer->Workers, &debugServer->Allocator);
#endif

    ARENA_ARRAY_INIT_SZ(debug_message_t, debugServer->Logs, &debugServer->Allocator, 4096);

    ARENA_ARRAY_INIT(debug_server_route_t, debugServer->Routes, &debugServer->Allocator);

    IdentifierTable_Init(&debugServer->CategoryTable, IDENTIFIER_LENGTH_SHIFT, 6, &debugServer->Allocator);

    debug_init_routes(debugServer);

   return 0;
}

void Debug_Allocator(debug_server_t* debugServer, metac_alloc_t* allocator)
{
    uint32_t count = debugServer->AllocatorsCount;
    if (count < debugServer->AllocationsCapacity)
    {
        debugServer->Allocators[count] = allocator;
    }
    else
    {
        uint32_t newCapa = debugServer->AllocatorsCapacity + 16;
        void* newMem =
            realloc(debugServer->Allocators, newCapa * sizeof(metac_alloc_t*));
        if (newMem)
            debugServer->Allocators = (metac_alloc_t**)newMem;
    }
    debugServer->AllocatorsCount += 1;
    // if (count == 10) { asm ( "int $3" ); }
}

void Debug_RemoveAllocator(debug_server_t* debugServer, metac_alloc_t* allocator)
{
    metac_alloc_t** allocs = debugServer->Allocators;
    uint32_t count = debugServer->AllocatorsCount;
    for(uint32_t i = 0; i < count; i++)
    {
        if (allocs[i] == allocator)
        {
            if (i != count - 1)
                memmove(allocs + i, allocs + i + 1, count - i - 1);
            break;
        }
    }
    --debugServer->AllocatorsCount;
}

void Debug_Allocation(debug_server_t* debugServer, metac_alloc_t* allocator, uint32_t sz, const char* file, uint32_t line)
{
    uint32_t timestamp;
    uint32_t count = debugServer->AllocationsCount++;
    OS.GetTimeStamp(&timestamp);

    debugServer->Allocations[count].Size = sz;
    debugServer->Allocations[count].File = file;
    debugServer->Allocations[count].Line = line;
    debugServer->Allocations[count].Timestamp = timestamp;

    g_allocated += sz;
    Debug_GraphValue(debugServer, "g_allocated", g_allocated);
}

void Debug_Pump(debug_server_t* debugServer)
{
}

void Debug_CurrentScope(debug_server_t* debugServer, metac_scope_t* scopeP)
{
    debugServer->CurrentScope = scopeP;
}
void Debug_CurrentIdentifierTable(debug_server_t* debugServer,
                                  metac_identifier_table_t* idTab)
{
    debugServer->CurrentIdentifierTable = idTab;
}

metac_identifier_ptr_t DebugServer_Category(debug_server_t* debugServer, const char* category)
{
    uint32_t len = strlen(category);
    uint32_t hash = crc32c_nozero(~0, category, len);

    return GetOrAddIdentifier(&debugServer->CategoryTable, IDENTIFIER_KEY(hash, len), category);
}


void Debug_Logf(debug_server_t* debugServer,
    const char* category,
    const char* fmt,
    ...
)
{
    char buffer[1024];
    debug_message_t log;
    int len;
    va_list list;

    OS.GetTimeStamp(&log.Timestamp);

    va_start(list, fmt);
    ALIGN_STACK();
    len = vsnprintf(buffer, sizeof(buffer), fmt, list);
    RESTORE_STACK();
    va_end(list);

    if (len >= ARRAY_SIZE(buffer))
    {
        // we would have overflowed our format buffer
        // allocate a string from the heap
        char* memory =
            Allocator_Calloc(&debugServer->Allocator, char, len + 1);
        va_start(list, fmt);
        len = vsnprintf(memory, len + 1, fmt, list);
        va_end(list);
        log.Message = cast(const char*)memory;
    }
    else
    {
        // copy The message as it's formatted into a stack local buffer
        log.Message = DebugServer_AddString(debugServer, buffer, len + 1);
    }
    log.Category = DebugServer_Category(debugServer, category);
    log.Length = len;


    ARENA_ARRAY_ADD(debugServer->Logs, log);
}


void Debug_GraphValue(debug_server_t* debugServer, const char* name, double value)
{
    uint32_t timestamp;
    //TODO use hashtable
    debug_graph_t* graphP = 0;
    debug_graph_value_t graphValue;
    OS.GetTimeStamp(&timestamp);

    graphValue.Timestamp = timestamp;
    graphValue.Value = value;

    for(uint32_t i = 0; i < debugServer->GraphsCount; i++)
    {
        graphP = debugServer->Graphs + i;
        if (strcmp(name, graphP->Name) == 0)
        {
            break;
        }
    }

    if (!graphP)
    {
        uint32_t nameLen = strlen(name);
        debug_graph_t graph;

        graph.Name = DebugServer_AddString(debugServer, name, nameLen);
        ARENA_ARRAY_INIT(debug_graph_value_t, graph.Values, &debugServer->Allocator);
    }

    ARENA_ARRAY_ADD(graphP->Values, graphValue);
}

metac_token_t* Debug_PeekToken(debug_server_t* debugServer, metac_token_t* token, uint32_t offset)
{
    return token;
}
#ifndef NO_FIBERS
void Debug_RegisterWorker(debug_server_t* debugServer, worker_context_t* worker)
{
    ARENA_ARRAY_ADD(debugServer->Workers, worker);
}
#endif

#endif
