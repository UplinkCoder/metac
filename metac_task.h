#ifndef _METAC_TASK_H_
#define _METAC_TASK_H_
#include "compat.h"
#include "3rd_party/tinycthread/tinycthread.h"
#define FIBERS_PER_WORKER 32
#define TASK_PAGE_SIZE 4096

typedef struct taskcontext_t
{
    void* TaskMemory;
    uint32_t BytesAllocated;
    uint32_t BytesUsed;
} taskcontext_t;

typedef struct task_t
{
    void (*TaskFunction)(taskcontext_t* contextTask);
    void* TaskParam;

    const char* (*PrintFunction)(struct task_t* task);

    struct task_t* Parent;
    struct task_t* Children;
    uint32_t ChildCount;
    uint32_t ChildrenCompleted;

    volatile uint32_t TaskFlags;

    uint16_t QueueId;
    uint16_t CompletionAttempts;
} task_t;

typedef struct taskqueue_t
{
    // TicketCounter queueLock;
    uint8_t padding[16 - sizeof(void*)];

    uint32_t readPointer; // head
    uint32_t writePointer; // tail

    task_t (*queueMemory)[1024];
} taskqueue_t;

typedef struct worker_context_t
{
    taskqueue_t Queue;

    uint32_t WorkerId;
    //PoolAllocator threadAlloc;

    thrd_t Thread;
} worker_context_t;

typedef struct tasksystem_t
{
    struct worker_context_t* workerContexts;
    taskqueue_t* taskQueues;
    uint32_t nWorkers;
} tasksystem_t;

void MetaCTaskSystem_Init(tasksystem_t* self, uint32_t workerThreads, void (*workerFn)(worker_context_t*));
#endif
