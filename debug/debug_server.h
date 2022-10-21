#ifndef _DEBUG_SERVER_H_
#define _DEBUG_SERVER_H_
#ifdef DEBUG_SERVER

#include "microhttpd.h"

#include "../os/metac_alloc.h"

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
} debug_server_t;

int Debug_Init(debug_server_t* debugServer, unsigned short port);
void Debug_Pump(debug_server_t* debugServer);

void Debug_Allocator(debug_server_t* debugServer, metac_alloc_t* allocator);
void Debug_Allocation(debug_server_t* debugServer, metac_alloc_t* allocator, uint32_t sz, const char* file, uint32_t line);
void Debug_RemoveAllocator(debug_server_t* debugServer, metac_alloc_t* allocator);

extern debug_server_t* g_DebugServer;

#else

#define Debug_Init(S, P)
#define Debug_Pump(S)
#define Debug_Allocator(S, A)
#define Debug_Allocation(S, A, Z, F, L)
#define Debug_RemoveAllocator(S, A)
#endif

#endif
