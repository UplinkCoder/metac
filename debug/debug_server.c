#ifdef DEBUG_SERVER

#include "../debug/debug_server.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

debug_server_t* g_DebugServer;

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

static int send_html(struct MHD_Connection* connection, const char* text, int len)
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

static int serveFile(const char* filename, const char* contentType, struct MHD_Connection* conn)
{
    FILE* f;
    char pageBuffer[8192];
    int sz;
    char* page;

    struct MHD_Response* response;
    int ret;

    fopen_s(&f, filename, "r");
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
    ret = MHD_queue_response(conn, MHD_HTTP_OK, response);
    MHD_destroy_response(response);

    return ret;
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

    printf("arenaCount: %u\n", alloc->ArenaCount);
    for(j = 0; j < alloc->ArenaCount; j++)
    {
        printf("j: %u\n", j);
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
                       "<td>%u</td>", alloc->ArenaCount
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
    printf("debugServer->AllocatorsCount: %u\n", debugServer->AllocatorsCount);
    for (i = 0; i < debugServer->AllocatorsCount; i++)
    {
        metac_alloc_t * alloc = debugServer->Allocators[i];
        outAllocRow(body, ARRAYSIZE(body), &p, alloc);
    }

   len = snprintf (responseString, ARRAYSIZE (responseString),
                    "<hmtl><body>"
                    "<h3>Allocators: </h3>"
                    "<table id=\"allocators\">%s</table>"
                    "</body></hmtl>",
           body);
    return send_html (connection, responseString, len);
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
    uint32_t allocIdx = atoi(allocIdxStr);
    metac_alloc_t* alloc = ((allocIdx && allocIdx <= (debugServer->AllocatorsCount))
        ? debugServer->Allocators[allocIdx - 1] : 0);

    const uint32_t arenaCount = alloc ? alloc->ArenaCount : 0;

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
                    "<hmtl><body>"
                    "<h3>Arenas: </h3>"
                    "<table id=\"arenas\">%s</table>"
                    "</body></hmtl>",
           body);
    return send_html (connection, responseBuffer, len);
}

static MHD_HANDLER(debugServerHandler)
{
    debug_server_t *debugServer = cast(debug_server_t*) cls;

    if (strcmp(url, "/allocators") == 0)
    {
        return handleAllocators(MHD_HANDLER_PASSTHROUGH);
    }

    if (memcmp(url, "/arenas", sizeof("/arenas") - 1) == 0)
    {
        return handleArenas(MHD_HANDLER_PASSTHROUGH);
    }

    if (debugServer->Handler)
    {
        return debugServer->Handler(MHD_HANDLER_PASSTHROUGH);
    }

    return MHD_NO;
}

static MHD_COMPLETED_CB (MhdCompletionCallback)
{
}


int Debug_Init(debug_server_t* debugServer, unsigned short port) {
    struct MHD_Daemon *d;
#ifdef WIN32
    WSADATA wd;
    if (WSAStartup (MAKEWORD (2, 2), &wd) != 0)
    {
        fprintf (stderr, "FATAL: failed to initialize Windows Sockets.\n");
        return 1;
    }
#endif    /**/

    d = MHD_start_daemon (MHD_USE_SELECT_INTERNALLY, port,
                          NULL, NULL,
                          debugServerHandler, debugServer,
                          MHD_OPTION_NOTIFY_COMPLETED,
                            MhdCompletionCallback, 0,
                          MHD_OPTION_END);

    debugServer->Daemon = d;

    uint32_t allocatorCapa = 64;
    uint32_t allocationCapa = 256;

    debugServer->Allocators = (metac_alloc_t**)
        malloc(sizeof(metac_alloc_t*) * allocatorCapa);
    debugServer->AllocatorsCount = 0;
    debugServer->AllocatorsCapacity = allocatorCapa;

    debugServer->Allocations = (debug_allocation_t*)
        malloc(sizeof(debug_allocation_t) * allocationCapa);
    debugServer->AllocationsCount = 0;
    debugServer->AllocationsCapacity = allocationCapa;

    return 0;
}

void Debug_Allocator(debug_server_t* debugServer, metac_alloc_t* allocator)
{
    uint32_t count = debugServer->AllocatorsCount++;
    if (count < debugServer->AllocationsCapacity)
    {
        debugServer->Allocators[count] = allocator;
    }
    else
    {
        uint32_t newCapa = debugServer->AllocatorsCapacity + 16;
        void* newMem =
            realloc(debugServer->Allocators, newCapa * sizeof(debug_allocation_t));
        if (newMem)
            debugServer->Allocators = (debug_allocation_t*)newMem;
    }
  //  if (count == 5) { asm ( "int $3" ); }
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
    uint32_t count = debugServer->AllocationsCount++;

    debugServer->Allocations[count].size = sz;
    debugServer->Allocations[count].file = file;
    debugServer->Allocations[count].line = line;
}

void Debug_Pump(debug_server_t* debugServer)
{
}

#endif
