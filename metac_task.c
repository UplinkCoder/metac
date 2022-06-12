#include "metac_task.h"

static bool watcherIntialized = 0;


// the actual worker function
uint32_t workerFunction(uint32_t workerThreads, void* (*workerFn)(void*), worker_context_t** workerContextsP)
{
    (*workerContextsP) = (worker_context_t*)
        calloc(sizeof(worker_context_t), workerThreads);

}

tasksystem_t* MetaCTaskSystem_Init(uint32_t workerThreads, void* (*workerFn)(void*))
{

}

