#ifndef _DEBUG_SERVER_H_
#define _DEBUG_SERVER_H_
#ifdef DEBUG_SERVER

#include "microhttpd.h"

#include "../os/metac_alloc.h"
#include "../semantic/metac_scope.h"
#include "../parser/metac_identifier_table.h"

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
    uint32_t size;
    const char* file;
    uint32_t line;
} debug_allocation_t;

typedef struct debug_server_t
{
    struct MHD_Daemon* Daemon;
    mhd_handler_t Handler;
    metac_alloc_t** Allocators;
    uint32_t AllocatorsCount;
    uint32_t AllocatorsCapacity;

    debug_allocation_t* Allocations;
    uint32_t AllocationsCount;
    uint32_t AllocationsCapacity;

    metac_scope_t* CurrentScope;
    metac_identifier_table_t* CurrentIdentifierTable;
} debug_server_t;

int Debug_Init(debug_server_t* debugServer, unsigned short port);
void Debug_Pump(debug_server_t* debugServer);

void Debug_Allocator(debug_server_t* debugServer, metac_alloc_t* allocator);
void Debug_Allocation(debug_server_t* debugServer, metac_alloc_t* allocator, uint32_t sz, const char* file, uint32_t line);
void Debug_RemoveAllocator(debug_server_t* debugServer, metac_alloc_t* allocator);

void Debug_CurrentScope(debug_server_t* debugServer, metac_scope_t* scopeP);
void Debug_CurrentIdentifierTable(debug_server_t* debugServer, metac_identifier_table_t* scopeP);

extern debug_server_t* g_DebugServer;

#else

#define Debug_Init(D, P)
#define Debug_Pump(D)
#define Debug_Allocator(D, A)
#define Debug_Allocation(D, A, Z, F, L)
#define Debug_RemoveAllocator(D, A)
#define Debug_CurrentIdentifierTable(D, IT)
#define Debug_CurrentScope(D, SC)
#endif

#endif
