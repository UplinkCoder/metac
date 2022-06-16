#ifndef _METAC_TASK_H_
#define _METAC_TASK_H_
#include "compat.h"
#include "3rd_party/tinycthread/tinycthread.h"
#include "3rd_party/deboost.context/include/fcontext/fcontext.h"
#define FIBERS_PER_WORKER 32
#define TASK_PAGE_SIZE 4096
#define TASK_QUEUE_SIZE 1024

typedef struct ticket_t
{
    uint32_t v;
} ticket_t;

typedef struct ticket_lock_t
{
    volatile uint32_t currentlyServing;
    volatile uint32_t nextTicket;
#ifdef THREAD_MUTEX
    bool initialized;
    mtx_t Mutex;
#endif
} ticket_lock_t;

typedef struct taskcontext_t
{
    uint32_t ContextCrc;

    void* TaskMemory;
    uint32_t BytesAllocated;
    uint32_t BytesUsed;
} taskcontext_t;

typedef struct task_t
{
    void (*TaskFunction)(taskcontext_t* taskContext);
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
    ticket_lock_t TicketLock;
    uint8_t padding[sizeof(ticket_lock_t) % 16];

    uint32_t readPointer; // head
    uint32_t writePointer; // tail

    task_t (*queueMemory)[TASK_QUEUE_SIZE];
} taskqueue_t;

typedef struct fiber_pool_t
{
    uint32_t FreeBitfield;

    fcontext_t fibers[sizeof(uint32_t) * 8];
    uint32_t stackLeft[sizeof(uint32_t) * 8];
    uint32_t stackTop[sizeof(uint32_t) * 8];
    fcontext_stack_t stacks[sizeof(uint32_t) * 8];
    //static_assert(sizeof(FreeBitfield) * 8 >= FIBERS_PER_WORKER);

} fiber_pool_t;

typedef struct worker_context_t
{
    taskqueue_t Queue;

    uint32_t WorkerId;
    //PoolAllocator threadAlloc;

    thrd_t Thread;
    //fiber_pool_t* FiberPool;
} worker_context_t;

typedef struct tasksystem_t
{
    struct worker_context_t* workerContexts;
    uint32_t nWorkers;
} tasksystem_t;

void MetaCTaskSystem_Init(tasksystem_t* self, uint32_t workerThreads, void (*workerFn)(worker_context_t*));
bool AddTask(task_t* task);

#endif
