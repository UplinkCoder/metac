#ifndef _METAC_TASK_H_
#define _METAC_TASK_H_
#include "compat.h"

#include "metac_thread.h"
#define FIBERS_PER_WORKER 32

typedef struct taskcontext_t
{
    uint16_t TaskPages;

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

typedef struct tasksystem_t
{
    worker_context_t* workerContexts;
    taskqueue_t* taskQueues;
    uint32_t nWorkers;
} tasksystem_t;

#endif
