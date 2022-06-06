#include "compat.h"

AT(per_thread)
typedef struct worker_context_t
{
    uint32_t WorkerId;
    PoolAllocator threadAlloc;
} worker_context_t;

#ifdef HAS_PTHREAD


#endif
