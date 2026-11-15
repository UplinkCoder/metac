#include "metac_task.h"
#include "os.h"
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <threads.h>

/* 
 * watcher_context_t 
 * Holds the state for the watcher thread.
 */
typedef struct {
    tasksystem_t* system;
    volatile bool kill;
    thrd_t thread;
} watcher_context_t;

/*
 * Dummy termination task function.
 * When a worker pulls this, it should signal it's time to exit.
 */
static void Watcher_TerminationTask(task_t* task) {
    // In a real implementation, this might set a flag in the worker_context
    // or simply return to allow the worker loop to see the termination signal.
    (void)task;
}

/*
 * The main loop run by the watcher thread.
 */
static int watcher_thread_func(void* arg) {
    watcher_context_t* ctx = (watcher_context_t*)arg;
    tasksystem_t* sys = ctx->system;

    printf("[Watcher] Thread started.\n");

    while (!ctx->kill) {
        // 1. Monitoring Phase
        for (uint32_t i = 0; i < sys->nWorkers; ++i) {
            worker_context_t* worker = &sys->workerContexts[i];
            taskqueue_t* q = &worker->Queue;

            // Calculate approximate queue depth
            // Note: Using relaxed loads/pointers as seen in metac_task.c
            uint32_t readP = q->readPointer;
            uint32_t writeP = q->writePointer;
            
            // Using TASK_QUEUE_SIZE which should be defined in metac_task.h or os.h
            uint32_t depth = (writeP >= readP) ? (writeP - readP) : (TASK_QUEUE_SIZE - readP + writeP);

            // In a real system, we might log this to Tracy or a file
            // printf("[Watcher] Worker %u | Queue Depth: %u | Active Task: %p\n", 
            //        worker->WorkerId, depth, worker->ActiveTask);
        }

        // 2. Sleep to avoid spinning
        // Using a simple sleep; in production, this might be more granular
        struct timespec ts = {0, 1000000}; // 1ms
        thrd_sleep(&ts, NULL);
    }

    printf("[Watcher] Kill signal received. Cleaning up...\n");

    // 3. Termination Phase (Poison Pill)
    // Following the fluffy pattern: push a termination task to all workers
    for (uint32_t i = 0; i < sys->nWorkers; ++i) {
        worker_context_t* worker = &sys->workerContexts[i];
        
        task_t term_task = {0};
        term_task.TaskFunction = Watcher_TerminationTask;
        // Set flags if necessary
        
        // Attempt to push into the worker's specific queue
        // We might loop until successful or a timeout occurs
        bool pushed = false;
        int attempts = 0;
        while (!pushed && attempts < 10) {
            pushed = TaskQueue_Push(&worker->Queue, &term_task);
            if (!pushed) {
                struct timespec ts = {0, 500000}; // 0.5ms
                thrd_sleep(&ts, NULL);
                attempts++;
            }
        }
    }

    printf("[Watcher] Termination tasks enqueued. Exiting.\n");
    return 0;
}

/*
 * Public API
 */

/**
 * Starts the watcher thread for the given task system.
 * @param ctx Pointer to the watcher context (caller must allocate).
 * @param sys Pointer to the task system to monitor.
 * @return true if successful.
 */
bool Watcher_Start(watcher_context_t* ctx, tasksystem_t* sys) {
    if (!ctx || !sys) return false;

    ctx->system = sys;
    ctx->kill = false;

    if (thrd_create(&ctx->thread, watcher_thread_func, ctx) != thrd_success) {
        return false;
    }

    return true;
}

/**
 * Stops the watcher thread gracefully.
 * @param ctx Pointer to the watcher context.
 */
void Watcher_Stop(watcher_context_t* ctx) {
    if (!ctx) return;

    ctx->kill = true;
    thrd_join(ctx->thread, NULL);
}
