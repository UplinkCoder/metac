__thread void* tlsCtx;

#include <pthread.h>

void startThread(void* (*fn)(void*),
                 worker_context_t* workerContext)
{
    tlsCtx = (void*)1;
}
