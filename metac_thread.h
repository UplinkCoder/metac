#include "compat.h"
#include "pool.h"

#if __linux__
#  include <pthread.h>
#elif _MSC_VER
#  include <processthreadsapi.h>
#endif

typedef struct worker_context_t
{
    uint32_t WorkerId;
    //PoolAllocator threadAlloc;

#if __linux__
    pthread_t NativeThreadId;
#elif _MSC_VER
    LPVOID
#endif
} worker_context_t;

#ifdef HAS_PTHREAD


#endif
