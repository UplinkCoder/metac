#ifndef _METAC_TASK_H_
#define _METAC_TASK_H_

#ifdef NO_FIBERS
#  error "Tasks don't work without fibers at the moment"
#endif

#include "compat.h"

#if defined(_MSC_VER) || defined(__STDC_NO_THREADS__) || defined(__TINYC__)
#  include "../3rd_party/tinycthread/tinycthread.h"
#else
#  include <threads.h>
#endif

#include "../os/metac_coro.h"
#include "../3rd_party/rwlock.h"

#define FIBERS_PER_WORKER 32
#define TASK_PAGE_SIZE 4096
#define TASK_QUEUE_SIZE 1024


#if defined(_WIN32) || defined(_WIN64)
#define THREAD_FUNC(NAME) \
    DWORD WINAPI NAME(void* arg)
#else
#define THREAD_FUNC(NAME) \
    void* NAME(void* arg)
#endif

/// Cancel the task if the Value matches the expected
void CancelIf(int32_t* Value, int32_t Expected);

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

    const char* CallerFile;
    uint32_t CallerLine;

    void* TaskMemory;
    uint32_t BytesAllocated;
    uint32_t BytesUsed;
} taskcontext_t;

// typedef void (*task_fn_t)(struct task_t*);

#define CALL_TASK_FN(FN, CTX_STRUCT_PTR) do { \
    uint8_t ctxMem[sizeof(taskcontext_t) + sizeof(*(CTX_STRUCT_PTR))]; \
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
    Task_Halted    = 0,

    Task_Running   = (1 << 1),
    Task_Resumable = (1 << 2),
    Task_Complete  = (1 << 3),
    Task_Waiting   = (1 << 4),

    Task_Continuation_JumpToLabel = (1 << 5),
    Task_Continuation_Task        = (1 << 6),
    Task_Continuation_Func        = (1 << 7),
} task_flags_t;

#pragma pack(push, 1)
typedef struct task_origin_t
{
    const char* File;
    const char* Func;
    uint32_t Line;
} task_origin_t;
#pragma pack(pop)

#define ORIGIN(VAR) \
     ( VAR.File = __FILE__, VAR.Func = __FUNCTION__,   VAR.Line = __LINE__ )

#define INLINE_TASK_CTX_SZ 32

typedef struct task_inline_ctx_t
{
    uint8_t data[INLINE_TASK_CTX_SZ];
} task_inline_ctx_t;

typedef struct task_t
{
    void (*TaskFunction)(struct task_t* task);
    void* Context;
    struct task_t* Parent;
    aco_t* Fiber;
    union {
        struct task_t* Continuation;
        struct {
            void* ResultPtr;
            aco_t* Caller;
        };
        void (*ContinuationFunc)(void* ctx);
    };

    const char* YieldReason;
    const char* YieldLoc;

    union {
        uint8_t _inlineContext[INLINE_TASK_CTX_SZ];
        task_inline_ctx_t inlineContext;
    };

    task_origin_t Origin;

    volatile task_flags_t TaskFlags;

    const char* TaskName;

    uint16_t QueueId;
    uint16_t CompletionAttempts;

    uint16_t ContextSize;
    uint16_t ContextCapacity;

    uint32_t ChildCount;
    uint32_t ChildrenCompleted;

    struct task_t* Children;
} task_t;

typedef struct taskqueue_t
{
    ticket_lock_t QueueLock;
    uint8_t padding[sizeof(ticket_lock_t) % 16];

    uint16_t readPointer; // head
    uint16_t writePointer; // tail1
    uint16_t writePointerEnd; //tail2

    task_t (*QueueMemory)[TASK_QUEUE_SIZE];

    void* ContextStorage;
    uint32_t ContextStorageCapacity;
} taskqueue_t;

typedef struct fiber_pool_t
{
    uint32_t FreeBitfield;

    aco_t MainCos[sizeof(uint32_t) * 8];
    aco_share_stack_t ShareStacks[sizeof(uint32_t) * 8];
    task_t Tasks[sizeof(uint32_t) * 8];
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
    task_t* ActiveTask;
    taskqueue_t Queue;

    uint32_t WorkerId;
    uint32_t KillWorker;

    //PoolAllocator threadAlloc;

    volatile uint32_t Flags;

    uint32_t HeartBeat;

    thrd_t Thread;
    aco_t* WorkerMain;
    fiber_pool_t* FiberPool;
} worker_context_t;


// metac_file_storage_t* Worker_GetFileStorage(worker_context_t* worker);

typedef struct tasksystem_t
{
    struct worker_context_t* workerContexts;
    uint32_t nWorkers;
} tasksystem_t;

void TaskSystem_Init(tasksystem_t* self, uint32_t workerThreads, void (*workerFunction)(worker_context_t* worker));
bool AddTask(task_t* task);
bool AddTaskToQueue(task_t* task);
worker_context_t* CurrentWorker(void);

#define FIBER_TYPE aco_t*
FIBER_TYPE CurrentFiber(void);
task_t* CurrentTask(void);

#define SET_CURRENT_TASK(TASKP) \
    (*(task_t**)&(GET_CO()->arg)) = (TASKP);

/// copies the task pointed to by *taskP the queue
bool TaskQueue_Push(taskqueue_t* self, task_t* taskP);

/// writes a task into the memory taskP points to
/// the queue slot is considered empty after this
bool TaskQueue_Pull(taskqueue_t* self, task_t* taskP);
#endif

#define CTX_TYPE(FUNC) \
    FUNC ## task_context_t


#define ENQUEUE_TASK(RESULT, FUNC, ...) do { \
    taskqueue_t* q = &CurrentWorker()->Queue; \
    CTX_TYPE(FUNC) _ctx = {__VA_ARGS__}; \
    task_t task = {0}; \
    STATIC_ASSERT(sizeof(task._inlineContext) >= sizeof(CTX_TYPE(FUNC)), \
        "Context size too large for inline context storage"); \
    task.Context = task._inlineContext; \
    task.TaskFunction = (FUNC ## Task); \
    task.TaskName = #FUNC; \
    task.Parent = CurrentTask(); \
    ORIGIN(task.Origin); \
    (*(cast(CTX_TYPE(FUNC)*)task.Context)) = _ctx; \
    TaskQueue_Push(q, &task); \
} while(0);
/*
#define SPAWN_TASK_CL(RESULT, FUNC, CONT_LABEL, ...) do { \
    taskqueue_t* q = &CurrentWorker()->Queue; \
    CTX_TYPE(FUNC) ctx = {__VA_ARGS__}; \
    task_t task = {0}; \
    STATIC_ASSERT(sizeof(task._inlineContext) >= sizeof(CTX_TYPE(FUNC)), \
        "Context size too large for inline context storage"); \
    task.TaskFlags |= Task_Continuation_JumpToLabel; \
    task.Context = task._inlineContext; \
    task.TaskFunction = (FUNC ## Task); \
    task.Parent = CurrentTask(); \
    ORIGIN(task.Origin); \
    (*(cast(CTX_TYPE(FUNC)*)task.Context)) = ctx; \
    task.ResultPtr = (void*)&RESULT; \
    task.Caller = CurrentFiber(); \
    TaskQueue_Push(q, &task); \
    YIELD(); \
    if (CurrentFiber == task.Caller) \
    { \
       goto CONT_LABEL; \
    } \
} while(0);
*/
