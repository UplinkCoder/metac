#ifndef NO_FIBERS

#include "../os/os.h"
#include "metac_task.h"
#include "../debug/debug_server.h"
#include <assert.h>
#include <stdlib.h>
#include "bsf.h"
#include <signal.h>

// the watcher shoud allocate the worker contexts since it is responsible for distribution
// and monitoring of the work

#if defined(_MSC_VER) || __STDC_VERSION__ < 201112L || defined(__STDC_NO_THREADS__) || defined(__TINYC__)
#  include "../3rd_party/tinycthread/tinycthread.c"
#  define HAS_THREADS
#endif

#ifndef U32
#define U32(VAR) \
    (*(uint32_t*)&VAR)
#endif

#ifndef KILOBYTE
#define KILOBYTE(N) \
    ((N) * 1024)
#endif

static bool watcherIntialized = 0;

#if defined(HAS_TLS)
_Thread_local worker_context_t* threadContext;
_Thread_local void *_CurrentFiber;
#elif defined(HAS_THREADS)
tss_t threadContextKey;
tss_t currentFiberKey;
#endif

taskqueue_t gQueue;

FIBER_TYPE CurrentFiber(void)
{
    return aco_get_co();
}

task_t* CurrentTask()
{
    return cast(task_t*)(cast(FIBER_TYPE)CurrentFiber())->arg;
}


#if defined(HAS_TLS)
#  define THREAD_CONTEXT threadContext
#  define THREAD_CONTEXT_SET(WORKER) threadContext = WORKER
#elif defined(HAS_THREADS)
#  define THREAD_CONTEXT ((worker_context_t*) tss_get(threadContextKey))
#  define THREAD_CONTEXT_SET(WORKER) tss_set(threadContextKey, WORKER)
#else
#  error "You got to have threads man!"
#endif

worker_context_t* CurrentWorker()
{
    return THREAD_CONTEXT;
}

void Taskqueue_Init(taskqueue_t* queue)
{
    queue->QueueMemory = cast(task_t (*)[1024])
        calloc(sizeof(task_t), ARRAY_SIZE(*queue->QueueMemory));
    queue->QueueLock.currentlyServing = 0;
    queue->QueueLock.nextTicket = 0;

    queue->readPointer = 0;
    queue->writePointer = 0;
    queue->writePointerEnd = 0;

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
    worker_context_t* worker = (worker_context_t*)thisFiber->main_co->arg;

    for(;;)
    {
        aco_t* fiber = CurrentFiber();
        task_t* task = (task_t*) aco_get_arg();
        if (worker->ActiveTask != task)
        {
#ifdef TASKLOG
            TaskLog_SwitchTask(worker->TaskLog, worker->ActiveTask, task);
#endif
            worker->ActiveTask = task;
        }

//        assert(!(task->TaskFlags & Task_Running));
        assert(task->Fiber == fiber);

        if (!task->TaskFunction)
        {
            YIELD_WORKER();
            continue;
        }
        U32(task->TaskFlags) |= Task_Running;
        task->TaskFunction(task);
        U32(task->TaskFlags) |= Task_Complete;
        U32(task->TaskFlags) &= ~Task_Running;

        fiber->arg = 0;

        if ((U32(task->TaskFlags) & Task_Continuation_Task) == Task_Continuation_Task)
        {
            task_t* continuation = task->Continuation;
            fiber->arg = (void*)(continuation);
            // let's set the dynamic parent
            continuation->Parent = task;
            continuation->Fiber = thisFiber;
            U32(continuation->TaskFlags) |= Task_Running;
            continuation->TaskFunction(continuation);
            U32(continuation->TaskFlags) |= Task_Complete;
        }
        else if ((U32(task->TaskFlags) & Task_Continuation_Func) == Task_Continuation_Func)
        {
            task->ContinuationFunc(task->Context);
        }
        YIELD_WORKER();

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
    // Assertion checks to ensure the task and fiber states are valid
    assert(!(U32(task->TaskFlags) & Task_Running));
    assert(!(U32(task->TaskFlags) & Task_Complete));
    assert(task->Fiber == fiber);

    // Assign the task to the fiber
    fiber->arg = task;

    // Obtain the fiber pool from the worker context
    fiber_pool_t* fiberPool = worker->FiberPool;
    const uint32_t fiberIdx = fiber - fiberPool->MainCos;

    // Check if the task function pointer is null
    if (task->TaskFunction == 0)
    {
        printf("The function pointer is null that's no good\n");
        //assert(0);
    }

    // Ensure the fiber is not already marked as free
    assert((fiberPool->FreeBitfield & (1 << fiberIdx)) == 0);

    // Resume or start the fiber based on its task state
    if (task->TaskFlags == Task_Halted)
    {
        // If the task was halted, start its execution
        START(task->Fiber, task);
    }
    else
    {
        // Otherwise, mark the task as running and resume its execution
        U32(task->TaskFlags) |= Task_Running;
        RESUME(task->Fiber);
    }

    // After execution, check if the task is complete
    if ((U32(task->TaskFlags) & Task_Complete) == Task_Complete)
    {
        // If the task is complete, mark the fiber as free and clear its argument
        assert(fiberIdx >= 0 && fiberIdx < sizeof(fiberPool->FreeBitfield) * 8);
        fiberPool->FreeBitfield |= (1 << fiberIdx);
        fiber->arg = 0;
    }
    else
    {
        // Ensure the task function is valid or the task is in waiting state
        assert((!task->TaskFunction) || (U32(task->TaskFlags) & Task_Waiting) == Task_Waiting);
    }
}

static inline uint32_t TaskQueue_TasksInQueueFront(const taskqueue_t* q)
{
    const uint32_t readP = q->readPointer & (TASK_QUEUE_SIZE - 1);
    const uint32_t writeP = q->writePointer & (TASK_QUEUE_SIZE - 1);

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


static inline uint32_t TaskQueue_TasksInQueueBack(const taskqueue_t* q)
{
    const uint32_t writePEnd = q->writePointerEnd;
    const uint32_t writeP = q->writePointer;

    if (writePEnd <= writeP)
    {
        return writeP - writePEnd;
    }
    else
    {
        return ((TASK_QUEUE_SIZE - writePEnd) + writeP);
    }
}

//|  | RW  |   | ET | eT  |
void WatcherFunc(void)
{

}

void WorkerSIGUSR1(int unused)
{
    printf("Worker: %d\n", THREAD_CONTEXT->WorkerId);
}



THREAD_FUNC(WorkerThreadFunc)
{
    // Not Implemented yet
    assert(0);
}

void RunWorkerThread(worker_context_t* worker, void (*specialFunc)(),  void* specialFuncCtx)
{
    aco_thread_init(0);
#ifdef POSIX
    signal(SIGUSR1, WorkerSIGUSR1);
#endif

    assert(THREAD_CONTEXT == 0 || THREAD_CONTEXT == worker);
    THREAD_CONTEXT_SET(worker);
#if DEBUG_SERVER
    Debug_RegisterWorker(g_DebugServer, worker);
#endif
    aco_t* threadFiber =
        aco_create(0, 0, 0, 0, worker);
    worker->WorkerMain = threadFiber;

    Taskqueue_Init(&worker->Queue);
    //FileStorage_Init(&worker->FileStorage, 0);

    aco_t* specialFiber = 0;
    task_t specialTask = {0};

    fiber_pool_t fiberPool;
    FiberPool_Init(&fiberPool, worker);
    worker->FiberPool = &fiberPool;
    uint32_t* fiberExecCounts = (uint32_t*)calloc(sizeof(uint32_t), FIBERS_PER_WORKER);

    worker->KillWorker = false;
    bool terminationRequested = false;
    uint32_t nextFiberIdx = 0;

    taskqueue_t *q = &worker->Queue;
    uint32_t* FreeBitfield = &fiberPool.FreeBitfield;

    task_t (*fileTasks)[4] = {0};
    OS.GetTimeStamp(&worker->HeartBeat);

    if (specialFunc)
    {
        ORIGIN(specialTask.Origin);
        specialTask.TaskName = "SpecialTask (probably repl)";
        specialFiber =
            aco_create(threadFiber, aco_share_stack_new(KILOBYTE(256)), 0, specialFunc, specialFuncCtx);
            specialFiber->arg = &specialTask;

            U32(((task_t*)specialFiber->arg)->TaskFlags) |= Task_Running;
            START(specialFiber, specialFuncCtx);
            //RESUME(specialFiber);
            U32(((task_t*)specialFiber->arg)->TaskFlags) |= Task_Waiting;

    }

    for(;;)
    {
        uint32_t nextFiberIdx = -1;
        aco_t* execFiber = 0;
        // do we have a free fiber ?
        if ((*FreeBitfield) == 0)
        {
            goto LscheduleNextTask;
        }
        OS.GetTimeStamp(&worker->HeartBeat);
        // if we end up here we still have a free fiber
        if (TaskQueue_TasksInQueueFront(q))
        {
            // we have a free fiber
            printf("Freebitfield %x\n", *FreeBitfield);
            nextFiberIdx = BSF(*FreeBitfield);
            printf("Grabbing Fiber: %u\n", nextFiberIdx);
            (*FreeBitfield) &= (~(1 << (nextFiberIdx)));
            execFiber = fiberPool.MainCos + nextFiberIdx;
            // mark fiber as used

            {
                task_t* taskP = &fiberPool.Tasks[nextFiberIdx];
                if (TaskQueue_Pull(q, taskP))
                {
                    printf("Pulled task\n");
                    taskP->Fiber = execFiber;
                    ExecuteTask(worker, taskP, execFiber);
                    if ((taskP->TaskFlags & Task_Complete) == Task_Complete)
                    {
                        printf("Execution finished freeing fiber\n");
                        *(FreeBitfield) |= (1 << nextFiberIdx);
                    }
                    continue;
                }
                else
                {
                    printf("No task to be pulled\n");
                }
            }
            goto LscheduleNextTask;
        }
        // if we do end up here we have a fiber to spare
        // but no work in the front queue let's check the back of our queue
        if (TaskQueue_TasksInQueueBack(q))
            execFiber = 0;

    LhandleContinutionLabel:
        {

        }
        // try to finish started tasks without starting new ones
    LscheduleNextTask:
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

                if ((U32(task->TaskFlags) & Task_Waiting) == Task_Waiting ||
                    (U32(task->TaskFlags) & Task_Complete) == Task_Complete)
                {
                    // printf("Encountered waiting task ... skipping \n");
                    tryMask0 |= (1 << nextFiberIdx);
                    continue;
                }

                ExecuteTask(worker, task, execFiber);

                tryMask0 |= (1 << nextFiberIdx);
            }
            const uint32_t CompletedTasks = ((completionGoal) ^ ~(*FreeBitfield)) & completionGoal;

            tryMask1 |= CompletedTasks;
            // set all tasks which have been completed in the tryMask1

            nextFiberBitfield = (~CompletedTasks) & (~(*FreeBitfield));
            execFiber = 0;
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

                if ((U32(task->TaskFlags) & Task_Waiting) == Task_Waiting ||
                    (U32(task->TaskFlags) & Task_Complete) == Task_Complete)
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
        // Now it's time to scan for outstanding tasks I/O
        {

        }
        // let's give waiting tasks the chance to make progress now.
        {
            uint32_t tryMask0 = 0; // tasks which have been tried once
            const uint32_t completionGoal = ~(*FreeBitfield);
            uint32_t nextFiberBitfield = completionGoal;
            uint32_t nextFiberIdx = -1;
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

                if ((U32(task->TaskFlags) & Task_Waiting) == Task_Waiting)
                {
                    printf("Encountered waiting task ... let's see if it can make progress\n");
                    ExecuteTask(worker, task, execFiber);
                }
                tryMask0 |= (1 << nextFiberIdx);
            }
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
            U32(((task_t*)specialFiber->arg)->TaskFlags) |= Task_Running;
            RESUME(specialFiber);
            U32(((task_t*)specialFiber->arg)->TaskFlags) |= Task_Waiting;
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

// Returns true if task was pushed
//         false if the queue was already full
bool TaskQueue_Push(taskqueue_t* self, task_t* task)
{
    uint32_t readP = RELAXED_LOAD(&self->readPointer) & (TASK_QUEUE_SIZE - 1);
    uint32_t writeP = RELAXED_LOAD(&self->writePointer) & (TASK_QUEUE_SIZE - 1);

    // if this is true the Queue is full
    if (readP == writeP + 1)
        return false;
    FENCE();
    uint32_t myTicket = DrawTicket(&self->QueueLock);
    FENCE();
    while (!ServingMe(&self->QueueLock, myTicket))
    {
        MM_PAUSE();
    }
    FENCE();
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

    FENCE();
    ReleaseTicket(&self->QueueLock, myTicket);

    return true;
}

/// returns true if task could be pulled
/// false if no task is pulled
bool TaskQueue_Pull(taskqueue_t* self, task_t* taskP)
{
    uint32_t readP = RELAXED_LOAD(&self->readPointer) & (TASK_QUEUE_SIZE - 1);
    uint32_t writeP = RELAXED_LOAD(&self->writePointer) & (TASK_QUEUE_SIZE - 1);

    // check if queue is empty
    if (readP == writeP)
        return false;

    FENCE();
    uint32_t myTicket = DrawTicket(&self->QueueLock);
    FENCE();
    while (!ServingMe(&self->QueueLock, myTicket))
    {
        MM_PAUSE();
    }
    FENCE();

    readP = ATOMIC_LOAD(&self->readPointer);
    writeP = ATOMIC_LOAD(&self->writePointer);

    if (readP == writeP)
    {
        ReleaseTicket(&self->QueueLock, myTicket);
        return false;
    }
    task_t* queueTask = (*self->QueueMemory) + readP;
    *taskP = *queueTask;
    INC(self->readPointer);
    queueTask->TaskFunction = 0;

    FENCE();
    ReleaseTicket(&self->QueueLock, myTicket);
    return true;
}

bool AddTaskToQueue(task_t* task)
{
    bool result = false;
    worker_context_t* threadContext = THREAD_CONTEXT;
    taskqueue_t* preferredQ =
        threadContext ? &threadContext->Queue : &gQueue;
    if (TaskQueue_TasksInQueueFront(preferredQ) < QUEUE_CUTOFF)
    {
        result = TaskQueue_Push(preferredQ, task);
    }
    //TODO try pushing into other queues and such things

    return result;
}

#undef KILOBYTE
#endif // NO_FIBERS
