#include "metac_thread.h"
#include "compat.h"
#if HAS_THREAD
__thread worker_context_t* threadContext;
#define TLS_THREAD_CONTEXT threadContext
#endif

#if !defined(ATOMIC)
#define INC(v) \
    (v++)
#elif __GNUC__
#  define INC(v) \
    (__builtin_atomic_fetch_add(&v, __ATOMIC_RELEASE))
#elif _MSC_VER
#  define INC(v) \
  _InterlockedIncrement(&v);
#else
#  error("No atomic supported")
#endif

typedef struct worker_init_args_t
{
    worker_context_t** workerContextP;

} worker_init_args_t;

// the watcher shoud allocate the worker contexts since it is responsible for distribution
// and monitoring of the work

#ifndef __STDC_NO_THREADS__
# include <threads.h>
#else
#  include "3rd_party/tinycthread/tinycthread.c"
#endif

_Thread_local threadContext;

uint32_t makeWorkerThread(void* (*workerFunc)(worker_context_t*), worker_context_t* workerContext)
{
    static uint32_t workerId = 1;
    workerContext->WorkerId = INC(workerId);

    void (*threadProc)(void*) = (void (*)(void*)) workerFunc;

#if __linux__
    int res = pthread_create(&workerContext->NativeThreadId, 0, threadProc, (void*)workerContext);
    if (res == 0)
        return ;
#elif _MSC_VER
    CreateThread(0, Megabytes(2), threadProc, (void*)workerContext, &workerContext->NativeThreadId);
#else
# error("OS not supported")
#endif
#ifdef TLS_THREAD_CONTEXT
    TLS_THREAD_CONTEXT = workerContext;
#endif
}
