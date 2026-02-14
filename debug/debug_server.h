#ifndef _DEBUG_SERVER_H_
#define _DEBUG_SERVER_H_
#ifdef DEBUG_SERVER

#include "microhttpd.h"
#include "../os/os.h"

#include "../os/metac_alloc.h"
#include "../semantic/metac_scope.h"
#include "../parser/metac_identifier_table.h"
#include "../parser/metac_lexer.h"

#ifndef NO_FIBERS
#  include "../os/metac_task.h"
#endif

#define MHD_COMPLETED_CB(NAME) \
void NAME (void *cls, \
           struct MHD_Connection *connection, \
           void **con_cls, \
           enum MHD_RequestTerminationCode toe)

#define MHD_HANDLER(NAME) \
int NAME (void *cls, \
          struct MHD_Connection *connection, \
          const char *url, \
          const char *method, \
          const char *version, \
          const char *upload_data, \
          size_t * upload_data_size, \
          void **con_cls)

#define MHD_HANDLER_PASSTHROUGH \
    cls, connection, \
    url, method, version, \
    upload_data, upload_data_size, \
    con_cls

typedef MHD_HANDLER ((*mhd_handler_t));

typedef struct debug_allocation_t
{
    const char* File;
    uint32_t Line;
    uint32_t Size;
    uint32_t Timestamp;
} debug_allocation_t;

typedef struct debug_graph_value_t
{
    double Value;
    uint32_t Timestamp;
} debug_graph_value_t;

typedef struct debug_graph_t
{
    const char* Name;
    ARENA_ARRAY(debug_graph_value_t, Values)
    uint32_t Timestamp;
} debug_graph_t;


typedef struct debug_message_t
{
    metac_identifier_ptr_t Category;
    const char* Message;
    uint32_t Length;
    uint32_t Timestamp;
} debug_message_t;

typedef enum debug_parser_action_kind_t
{
    ParserAction_Invalid,

    ParserAction_PeekToken,
    ParserAction_PeekMatch,
    ParserAction_Match,

    ParserAction_PushExpr,
    ParserAction_PopExpr,
    ParserAction_PushOp,
    ParserAction_PopOp,

    ParserAction_Max
} debug_parser_action_kind_t;

typedef struct debug_parser_action_t
{
    debug_parser_action_kind_t Kind;
} debug_parser_action_t;

typedef struct debug_server_route_t
{
    const char* Url;
    mhd_handler_t Handler;
} debug_server_route_t;

typedef struct debug_server_t
{
    struct MHD_Daemon* Daemon;
    mhd_handler_t Handler;

    metac_alloc_t Allocator;

    ARENA_ARRAY(debug_server_route_t, Routes)

    ARENA_ARRAY(debug_message_t, Logs)

    metac_identifier_table_t CategoryTable;

    metac_alloc_t** Allocators;
    uint32_t AllocatorsCount;
    uint32_t AllocatorsCapacity;

    debug_allocation_t* Allocations;
    uint32_t AllocationsCount;
    uint32_t AllocationsCapacity;

    metac_scope_t* CurrentScope;
    metac_identifier_table_t* CurrentIdentifierTable;

    debug_graph_t* Graphs;
    uint32_t GraphsCount;
    uint32_t GraphsCapacity;

    metac_identifier_table_t* IdentifierTables;
    uint32_t IdentifierTableCount;
    uint32_t IdentifierTableCapacity;

    debug_message_t* Messages;
    uint32_t MessagesCount;
    uint32_t MessagesCapacity;

    metac_token_t* TokenStream;
    uint32_t TokenStreamCount;
    uint32_t TokenStreamCapacity;

#ifndef NO_FIBERS
    ARENA_ARRAY(worker_context_t*, Workers)
#endif
} debug_server_t;

int Debug_Init(debug_server_t* debugServer, unsigned short port);
void Debug_Pump(debug_server_t* debugServer);

void Debug_Allocator(debug_server_t* debugServer, metac_alloc_t* allocator);
void Debug_Allocation(debug_server_t* debugServer, metac_alloc_t* allocator, uint32_t sz, const char* file, uint32_t line);
void Debug_RemoveAllocator(debug_server_t* debugServer, metac_alloc_t* allocator);

void Debug_GraphValue(debug_server_t* debugServer, const char* name, double value);

void Debug_CurrentScope(debug_server_t* debugServer, metac_scope_t* scopeP);
void Debug_CurrentIdentifierTable(debug_server_t* debugServer, metac_identifier_table_t* scopeP);

metac_token_t* Debug_PeekToken(debug_server_t* debugServer, metac_token_t* token, uint32_t offset);

#ifndef NO_FIBERS
void Debug_RegisterWorker(debug_server_t* debugServer, worker_context_t* worker);
#endif

extern debug_server_t* g_DebugServer;

#else

#define Debug_Init(D, P)
#define Debug_Pump(D)
#define Debug_Allocator(D, A)
#define Debug_Allocation(D, A, Z, F, L)
#define Debug_RemoveAllocator(D, A)
#define Debug_GraphValue(D, N, V)
#define Debug_CurrentIdentifierTable(D, IT)
#define Debug_CurrentScope(D, SC)
#define Debug_PeekToken(D, PARSER, OFFSET)
#define Debug_RegisterWorker(D, W)
#define Debug_Logf(D, F, ...)
#endif

#endif
