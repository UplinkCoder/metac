#ifndef _METAC_TASK_H_
#define _METAC_TASK_H_

#include "compat.h"
#include "3rd_party/tinycthread/tinycthread.h"
#include "metac_coro.h"
#include "3rd_party/rwlock.h"

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

    void* ContextMem;
    uint32_t ContextMemSize;

    void* TaskMemory;
    uint32_t BytesAllocated;
    uint32_t BytesUsed;

    const char* CallerFile;
    uint32_t CallerLine;
} taskcontext_t;

typedef void (*task_fn_t)(struct task_t*);

#define CALL_TASK_FN(FN, CTX_STRUCT_PTR) do { \
    uint8_t* ctxMem = cast(uint8_t*)alloca(sizeof(taskcontext_t) + sizeof(*(CTX_STRUCT_PTR))); \
    taskcontext_t* taskCtx = cast(taskcontext_t*) ctxMem; \
    uint8_t* ctxPtr = ctxMem + sizeof(taskcontext_t); \
    \
    memcpy(ctxPtr, CTX_STRUCT_PTR, sizeof(*(CTX_STRUCT_PTR))); \
    \
    taskcontext_t ctx = \
        { \
        crc32c(~0, #FN, sizeof(#FN) - 1), \
        ctxPtr, sizeof(*(CTX_STRUCT_PTR)), \
        \
        __FILE__, __LINE__ }; \
    \
    *taskCtx = ctx; \
    (*cast(void**)(&CTX_STRUCT_PTR)) = cast(void*)ctxPtr; \
} while (0);

typedef enum task_flags_t
{
    Task_Halted,

    Task_Running   = (1 << 0),
    Task_Resumable = (1 << 1),
    Task_Complete  = (1 << 2),
    Task_Waiting   = Task_Resumable | Task_Running,
} task_flags_t;

typedef struct task_origin_t
{
    const char* File;
    const char* Func;
    uint32_t Line;
} task_origin_t;

#define ORIGIN(VAR) \
     ( VAR.File = __FILE__, VAR.Func = __FUNCTION__,   VAR.Line = __LINE__ )


typedef struct task_t
{
    void (*TaskFunction)(struct task_t* task);
    void* Context;

    const char* (*PrintFunction)(struct task_t* task);

    volatile task_flags_t TaskFlags;
    aco_t* Fiber;

    struct task_t* Parent;
    struct task_t* Children;

    uint8_t _inlineContext[24];

    uint32_t ChildCount;
    uint32_t ChildrenCompleted;

    uint16_t QueueId;
    uint16_t CompletionAttempts;

    uint16_t ContextSize;
    uint16_t ContextCapacity;

    void* Continuation;
    task_origin_t Origin;
} task_t;

typedef struct taskqueue_t
{
    ticket_lock_t QueueLock;
    uint8_t padding[sizeof(ticket_lock_t) % 16];

    uint32_t readPointer; // head
    uint32_t writePointer; // tail

    task_t (*QueueMemory)[TASK_QUEUE_SIZE];

    void* ContextStorage;
    uint32_t ContextStorageCapacity;
} taskqueue_t;

typedef struct fiber_pool_t
{
    uint32_t FreeBitfield;


    aco_t MainCos[sizeof(uint32_t) * 8];
    aco_share_stack_t ShareStacks[sizeof(uint32_t) * 8];
    //static_assert(sizeof(FreeBitfield) * 8 >= FIBERS_PER_WORKER);
} fiber_pool_t;

typedef enum worker_flags_t
{
    None = 0,
    Worker_YieldOnTaskCreation = (1 << 0),

    Worker_Max = (1 << 0)
} worker_flags_t;

typedef struct worker_context_t
{
    taskqueue_t Queue;

    uint32_t WorkerId;
    uint32_t KillWorker;

    //PoolAllocator threadAlloc;

    volatile uint32_t Flags;

    thrd_t Thread;
    aco_t* WorkerMain;
    fiber_pool_t* FiberPool;
} worker_context_t;

typedef struct tasksystem_t
{
    struct worker_context_t* workerContexts;
    uint32_t nWorkers;
} tasksystem_t;

void TaskSystem_Init(tasksystem_t* self, uint32_t workerThreads, void (*workerFunction)(worker_context_t* worker));
bool AddTask(task_t* task);
worker_context_t* CurrentWorker(void);

bool TaskQueue_Push(taskqueue_t* self, task_t* task);
bool TaskQueue_Pull(taskqueue_t* self, task_t** taskP);

#endif
