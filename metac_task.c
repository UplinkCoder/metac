#include "metac_task.h"
#include <assert.h>
#include <stdlib.h>
#include "bsf.h"

extern int __stdout_enable = 0;

// the watcher shoud allocate the worker contexts since it is responsible for distribution
// and monitoring of the work

#if defined(_MSC_VER) || defined(__STDC_NO_THREADS__)
#  include "3rd_party/tinycthread/tinycthread.c"
#else
# include <threads.h>
#endif

#ifndef KILOBYTE
#define KILOBYTE(N) \
    ((N) * 1024)
#endif

static bool watcherIntialized = 0;

_Thread_local worker_context_t* threadContext = 0;

taskqueue_t gQueue;

_Thread_local void *_CurrentFiber;

void* CurrentFiber(void)
{
    return aco_get_co();
}

task_t* CurrentTask()
{
    return (task_t*)((aco_t*)CurrentFiber())->arg;
}

worker_context_t* CurrentWorker()
{
    return threadContext;
}

void Taskqueue_Init(taskqueue_t* queue)
{
    queue->QueueMemory = cast(task_t (*)[1024])
        calloc(sizeof(task_t), ARRAY_SIZE(*queue->QueueMemory));
    queue->QueueLock.currentlyServing = 0;
    queue->QueueLock.nextTicket = 0;

    task_t* qMem = (*queue->QueueMemory);
    for(uint32_t taskIdx = 0;
        taskIdx < ARRAY_SIZE(*queue->QueueMemory);
        taskIdx++)
    {
        task_t* task = qMem + taskIdx;
        // printf("task: %p %u\n", task, task - (*queue->QueueMemory));
        task->ContextCapacity = sizeof(task->_inlineContext);
        task->Context = task->_inlineContext;
        task->ContextSize = 0;
    }
#if THREAD_MUTEX
    mtx_init(&queue->QueueLock.Mutex);
#endif
}

void FiberDoTask(void)
{
    aco_t* thisFiber = CurrentFiber();
    worker_context_t* worker = thisFiber->main_co->arg;

    for(;;)
    {
        aco_t* fiber = CurrentFiber();
        task_t* task = (task_t*) aco_get_arg();
        assert(!(task->TaskFlags & Task_Running));
        assert(task->Fiber == fiber);

        task->TaskFlags |= Task_Running;
        task->TaskFunction(task);
        task->TaskFlags |= Task_Complete;

        YIELD(YieldingBackAfterTaskCompletion);

        // FiberReport(fiber);
    }
}

void FiberPool_Init(fiber_pool_t* self, worker_context_t* worker)
{
    self->FreeBitfield = ~0;

    for(uint32_t i = 0; i < sizeof(self->FreeBitfield) * 8; i++)
    {
        self->ShareStacks[i] = *aco_share_stack_new(KILOBYTE(512));
        self->MainCos[i] = *aco_create(worker->WorkerMain, self->ShareStacks + i, 0, FiberDoTask, worker);
    }
}

void ExecuteTask(worker_context_t* worker, task_t* task, aco_t* fiber)
{
    assert(!(task->TaskFlags & Task_Running));
    assert(!(task->TaskFlags & Task_Complete));
    if (task == worker)
    {
        assert(0);
    }
    assert(task->Fiber == fiber);
    fiber->arg = task;
    fiber_pool_t* fiberPool = worker->FiberPool;
    const uint32_t fiberIdx = fiber - fiberPool->MainCos;

    if (task->TaskFunction == 0)
    {
        assert(0);
    }
    assert((fiberPool->FreeBitfield & (1 << fiberIdx)) == 0);
    if (task->TaskFlags == Task_Halted)
    {
        START(task->Fiber);
    }
    else
    {
        RESUME(task->Fiber);
    }

    if ((task->TaskFlags & Task_Complete) == Task_Complete)
    {
        assert(fiberIdx >= 0 && fiberIdx < sizeof(fiberPool->FreeBitfield) * 8);
        fiberPool->FreeBitfield |= (1 << fiberIdx);
        fiber->arg = 0;
    }
    else
    {
        assert((task->TaskFlags & Task_Resumable) == Task_Resumable);
    }
}

static inline uint32_t tasksInQueue(const uint32_t readP, const uint32_t writeP)
{
    if (writeP >= readP)
    {
        return (writeP - readP);
    }
    else
    {
        // wrap-around
        // we go from readP to length and from zero to writeP
        return ((TASK_QUEUE_SIZE - readP) + writeP);
    }
}

void WatcherFunc(void)
{

}

void RunWorkerThread(worker_context_t* worker, void (*specialFunc)(),  void* specialFuncCtx)
{
    aco_thread_init(0);

    assert(threadContext == 0 || threadContext == worker);
    threadContext = worker;

    aco_t* threadFiber =
        aco_create(0, 0, 0, 0, worker);
    worker->WorkerMain = threadFiber;

    Taskqueue_Init(&worker->Queue);

    aco_t* specialFiber = 0;
    if (specialFunc)
    {
        specialFiber =
            aco_create(threadFiber, aco_share_stack_new(KILOBYTE(256)), 0, specialFunc, specialFuncCtx);
        START(specialFiber);
    }

    fiber_pool_t fiberPool;
    FiberPool_Init(&fiberPool, worker);
    worker->FiberPool = &fiberPool;
    uint32_t* fiberExecCounts = (uint32_t*)calloc(sizeof(uint32_t), FIBERS_PER_WORKER);

    worker->KillWorker = false;
    bool terminationRequested = false;
    uint32_t nextFiberIdx = 0;

    taskqueue_t *q = &worker->Queue;
    uint32_t* FreeBitfield = &fiberPool.FreeBitfield;

    for(;;)
    {
        uint32_t nextFiberIdx = -1;
        task_t* taskP = 0;
        aco_t* execFiber = 0;
        // check if we have tasks in our Queue and if we have a free worker fiber
        if (tasksInQueue(q->readPointer, q->writePointer)
            && ((*FreeBitfield) != 0))
        {
            // we have a free fiber
            // printf("Freebitfield %x\n", *FreeBitfield);
            nextFiberIdx = BSF(*FreeBitfield);
            // printf("Grabbing Fiber: %u\n", nextFiberIdx);
            (*FreeBitfield) &= (~(1 << (nextFiberIdx)));
            execFiber = fiberPool.MainCos + nextFiberIdx;
            // mark fiber as used

            {
                if (TaskQueue_Pull(q, &taskP))
                {
                    // printf("Pulled task\n");
                    taskP->Fiber = execFiber;
                    ExecuteTask(worker, taskP, execFiber);
                    if ((taskP->TaskFlags & Task_Complete) == Task_Complete)
                    {
                        // printf("Execution finished freeing fiber\n");
                        *(FreeBitfield) |= (1 << nextFiberIdx);
                    }
                }
                else
                {
                    printf("No task to be pulled\n");
                }
            }
        }
        execFiber = 0;

        // try to finish started tasks without starting new ones
        {
            // the completion goal is the number of active tasks
            const uint32_t completionGoal = ~(*FreeBitfield);
            // holds the information for which fibers should be executed next
            if (!completionGoal)
                goto LrunSpeicalFiberOrTerminate;
            uint32_t tryMask0 = 0; // tasks which have been tried once
            uint32_t tryMask1 = 0; // tasks which have been tried twice

            // do not allow the creation of new tasks for this run
            worker->Flags |= Worker_YieldOnTaskCreation;

            // try fibers until all of them have been tried twice
            uint32_t nextFiberBitfield = completionGoal;
            if (nextFiberBitfield)
            {
                // printf("Trying to finish started tasks %x\n", nextFiberBitfield);
            }

            for(;;)
            {
                nextFiberIdx = BSF(nextFiberBitfield);
                nextFiberBitfield &= ~(1 << nextFiberIdx);
                // printf("nextFiberIdx: %u\n", nextFiberIdx);
                // printf("nextFiberBitField: %x\n", nextFiberBitfield);
                // printf("tryMask0: %x\n", tryMask0);
                execFiber = &fiberPool.MainCos[nextFiberIdx];
                task_t* task = (task_t*)execFiber->arg;

                if (tryMask0 == completionGoal)
                {
                    assert(nextFiberBitfield == 0);
                    break;
                }

                if ((task->TaskFlags & Task_Waiting) == Task_Waiting ||
                    (task->TaskFlags & Task_Complete) == Task_Complete)
                {
                    // printf("Encountered waiting task ... skipping \n");
                    tryMask0 |= (1 << nextFiberIdx);
                    continue;
                }

                ExecuteTask(worker, task, execFiber);

                tryMask0 |= (1 << nextFiberIdx);
            }
            const uint32_t CompletedTasks = ((completionGoal) ^ ~(*FreeBitfield)) & completionGoal;

            // set all tasks which have been completed in the tryMask1
            nextFiberBitfield = (~CompletedTasks) & (~(*FreeBitfield));
            execFiber = 0;
            tryMask1 |= CompletedTasks;
            for(;;)
            {
                nextFiberIdx = BSF(nextFiberBitfield);
                nextFiberBitfield &= ~(1 << nextFiberIdx);
                execFiber = &fiberPool.MainCos[nextFiberIdx];
                task_t* task = (task_t*)execFiber->arg;

                if (tryMask1 == completionGoal)
                {
                    assert(nextFiberBitfield == 0);
                    break;
                }

                if ((task->TaskFlags & Task_Waiting) == Task_Waiting ||
                    (task->TaskFlags & Task_Complete) == Task_Complete)
                {
                    tryMask1 |= (1 << nextFiberIdx);
                    // printf("tryMask1: %x == completionGoal: %x\n", tryMask1, completionGoal);
                    continue;
                }
                assert(tryMask1 != completionGoal);

                ExecuteTask(worker, (task_t*)execFiber->arg, execFiber);

                tryMask1 |= (1 << nextFiberIdx);
            }

            assert(tryMask0 == tryMask1);
        }

        // if we couldn't finish all the tasks it's likely
        // that we need to spawn new tasks in order to succseed
LrunSpeicalFiberOrTerminate:
        worker->Flags &= ~Worker_YieldOnTaskCreation;
        if (worker->KillWorker || terminationRequested)
            break;

        if (specialFiber)
        {
            if (specialFiber->is_end)
            {
                terminationRequested = true;
                continue;
            }

            RESUME(specialFiber);
        }
    }

    printf("worker thread to be torn down\n");
}

/// Return Value thrd_success thingy
uint32_t MakeWorkerThread(void (*workerFunction)(worker_context_t*), worker_context_t* workerContext)
{
    static uint32_t workerId = 1;
    workerContext->WorkerId = INC(workerId);

    int (*const threadProc)(void*) = (int (*)(void*)) workerFunction;
    return thrd_create(&workerContext->Thread, threadProc, workerContext);
}


void TaskSystem_Init(tasksystem_t* self, uint32_t workerThreads, void (*workerFunction)(worker_context_t*))
{
    // gQueue
    self->workerContexts = cast(worker_context_t*)
        calloc(sizeof(worker_context_t), workerThreads);
    //self->workerContexts +
    for(uint32_t i = 0; i < workerThreads; i++)
    {
        worker_context_t* ctx = self->workerContexts + i;

        MakeWorkerThread((workerFunction ? workerFunction : 0), ctx);
    }
    //thrd_creat

    self->nWorkers = workerThreads;
}
#define QUEUE_CUTOFF 960


#define ATOMIC_LOAD(VAR_PTR) \
    *(VAR_PTR)

#define RELAXED_LOAD(VAR_PTR) \
    *(VAR_PTR)

bool ServingMe(volatile ticket_lock_t* lock, uint32_t myTicket)
{
#ifdef THREAD_MUTEX
    bool result = mtx_trylock(&lock->Mutex) == thrd_success;
#else
    bool result = ATOMIC_LOAD(&lock->currentlyServing) == myTicket;
#endif
#ifdef INSPECTOR
    if (result)
        __itt_sync_acquired(lock);
#endif
    return result;
}

void ReleaseTicket(volatile ticket_lock_t* lock, uint32_t ticket)
{
#ifdef INSPECTOR
    __itt_sync_releasing(lock);
#endif
    assert(lock->currentlyServing == ticket);
    INC(lock->currentlyServing);
#ifdef THREAD_MUTEX
    mtx_unlock(&lock->Mutex);
#endif
}

uint32_t DrawTicket(volatile ticket_lock_t* lock)
{
#ifdef INSPECTOR
    __itt_sync_prepare(lock);
#endif
    return INC(lock->nextTicket);
}

uint32_t TaskQueue_TasksInQueue_(taskqueue_t* self)
{
    return tasksInQueue(self->readPointer, self->writePointer);
}

// Returns true if task was pushed
//         false if the queue was already full
bool TaskQueue_Push(taskqueue_t* self, task_t** taskP)
{
    uint32_t readP = RELAXED_LOAD(&self->readPointer) & (TASK_QUEUE_SIZE - 1);
    uint32_t writeP = RELAXED_LOAD(&self->writePointer) & (TASK_QUEUE_SIZE - 1);
    task_t* task = *taskP;
    // if this is true the Queue is full
    if (readP == writeP + 1)
        return false;
    FENCE()
    uint32_t myTicket = DrawTicket(&self->QueueLock);
    FENCE()
    while (!ServingMe(&self->QueueLock, myTicket))
    {
        MM_PAUSE()
    }
    FENCE()
    readP = ATOMIC_LOAD(&self->readPointer) & (TASK_QUEUE_SIZE - 1);
    writeP = ATOMIC_LOAD(&self->writePointer) & (TASK_QUEUE_SIZE - 1);

    if (readP == writeP + 1)
    {
        ReleaseTicket(&self->QueueLock, myTicket);
        return false;
    }
    task_t* queueTask = (*self->QueueMemory) + writeP;
    // ((*self->QueueMemory)[(writePointer + 1 <= TASK_QUEUE_SIZE) ? writePointer : 0]) = *task;
    *queueTask = *task;

    if (task->ContextSize > sizeof(task->_inlineContext))
    {
        assert(0);
    //    memcpy(task->ContextStorage, task->TaskParam, task->TaskParamSz);
    }
    memcpy(queueTask->_inlineContext, task->Context, task->ContextSize);
    queueTask->Context = queueTask->_inlineContext;
    INC(self->writePointer);

    *taskP = queueTask;
    FENCE()
    ReleaseTicket(&self->QueueLock, myTicket);

    return true;
}

/// returns true if task could be pulled
/// false if no task is pulled
bool TaskQueue_Pull(taskqueue_t* self, task_t** taskP)
{
    uint32_t readP = RELAXED_LOAD(&self->readPointer) & (TASK_QUEUE_SIZE - 1);
    uint32_t writeP = RELAXED_LOAD(&self->writePointer) & (TASK_QUEUE_SIZE - 1);

    // check if queue is empty
    if (readP == writeP)
        return false;

    FENCE()
    uint32_t myTicket = DrawTicket(&self->QueueLock);
    FENCE()
    while (!ServingMe(&self->QueueLock, myTicket))
    {
        MM_PAUSE()
    }
    FENCE()

    readP = ATOMIC_LOAD(&self->readPointer);
    writeP = ATOMIC_LOAD(&self->writePointer);

    if (readP == writeP)
    {
        ReleaseTicket(&self->QueueLock, myTicket);
        return false;
    }

    *taskP = (*self->QueueMemory) + readP;
    INC(self->readPointer);

    FENCE()
    ReleaseTicket(&self->QueueLock, myTicket);
    return true;
}

bool AddTaskToQueue(task_t* task)
{
    bool result = false;

    taskqueue_t* preferredQ =
        threadContext ? &threadContext->Queue : &gQueue;
    if (TaskQueue_TasksInQueue_(preferredQ) < QUEUE_CUTOFF)
    {
        result = TaskQueue_Push(preferredQ, task);
    }
    //TODO try pushing into other queues and such things

    return result;
}

void* SpawnTask(void (*taskFn)(task_t*), const uint32_t contextSz, const uint32_t resultOffset,
                const task_origin_t origin,
                uint8_t ctxMem[INLINE_TASK_CTX_SZ])
{
    taskqueue_t* q = &CurrentWorker()->Queue;
    task_t task = {0};
    task_t* taskP = &task;
    task.TaskFunction = taskFn;
    task.Parent = CurrentTask();
    if (contextSz <= INLINE_TASK_CTX_SZ)
    {

    }
}

#define SPWAN_TASK(RESULT, FUNC, ...) do { \
    taskqueue_t* q = &CurrentWorker()->Queue; \
    task_t task = {0}; \
    CTX_TYPE(FUNC) ctx = {__VA_ARGS__}; \
    CTX_TYPE(FUNC)* ctxPtr = &ctx; \
    STATIC_ASSERT(sizeof(task._inlineContext) >= sizeof(CTX_TYPE(FUNC)), \
        "Context size too large for inline context storage"); \
    task.Context = task._inlineContext; \
    task.TaskFunction = CAT(FUNC, Task); \
    task.Parent = CurrentTask(); \
    ORIGIN(task.Origin); \
    (*(cast(CTX_TYPE(FUNC)*)task.Context)) = ctx; \
    TaskQueue_Push(q, &task); \
    WAIT_FOR(task.Parent, &task, FUNC); \
    RESULT = ctxPtr->Result; \
} while(0);

#undef KILOBYTE
