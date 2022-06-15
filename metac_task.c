#include "metac_task.h"

#include <stdlib.h>

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


// the watcher shoud allocate the worker contexts since it is responsible for distribution
// and monitoring of the work

#ifndef __STDC_NO_THREADS__
# include <threads.h>
#else
#  include "3rd_party/tinycthread/tinycthread.c"
#endif


static bool watcherIntialized = 0;

_Thread_local worker_context_t* threadContext;

// the actual worker function
void workerFunction(worker_context_t* context)
{
    printf("ARRAY_SIZE: %d\n",
        ARRAY_SIZE(*context->Queue.queueMemory));

    context->Queue.queueMemory = cast(task_t (*)[1024])
        calloc(sizeof(task_t), ARRAY_SIZE(*context->Queue.queueMemory));
    threadContext = context;
}

/// Return Value thrd_success thingy
uint32_t MakeWorkerThread(void (*workerFunc)(worker_context_t*), worker_context_t* workerContext)
{
    static uint32_t workerId = 1;
    workerContext->WorkerId = INC(workerId);

//    void (*threadProc)(void*) = (void (*)(void*)) workerFunc;
    return thrd_create(&workerContext->Thread, workerFunc, workerContext);
}


void MetaCTaskSystem_Init(tasksystem_t* self, uint32_t workerThreads, void (*workerFn)(worker_context_t*))
{
    self->taskQueues = cast(taskqueue_t*)
        calloc(sizeof(taskqueue_t), workerThreads);

    self->workerContexts = cast(worker_context_t*)
        calloc(sizeof(worker_context_t), workerThreads);
    //self->workerContexts +
    for(uint32_t i = 0; i < workerThreads; i++)
    {
        worker_context_t* ctx = self->workerContexts + i;

        MakeWorkerThread(workerFunction, ctx);
    }
    //thrd_creat

    self->nWorkers = workerThreads;
}

