/* ======================================================================
 *  task_system_test.c
 *
 *  The integration test of the coroutine layer: the task system of
 *  MetaC runs its tasks in coroutines, so this test runs the real
 *  worker thread and checks that the tasks are executed.
 *
 *  It works with both coroutine backends:
 *
 *      # libaco (the default)
 *      cc -o task_system_test tests/task_system_test.c -I.
 *
 *      # boost.context
 *      cc -DMETAC_CORO_FCONTEXT -o task_system_test tests/task_system_test.c -I.
 * ==================================================================== */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

/* the task system and the coroutine backend */
#include "../os/os.c"
#include "../os/metac_task.c"
#include "../os/metac_coro.c"

static uint32_t gTasksRun = 0;
static int gTaskValues[16];
static uint32_t gTaskCount = 0;

static void CountingTask(task_t* task)
{
    int value = *((int*)task->Context);
    gTaskValues[gTaskCount++] = value;
    gTasksRun++;
}

/* the function of the worker thread: it pushes a few tasks into the
 * queue and lets the worker execute them */
static void WorkerFunction(void)
{
    for (int i = 1; i <= 5; i++)
    {
        task_t task = {0};
        task.TaskFlags = 0;
        task.TaskFunction = CountingTask;
        (*((int*)task._inlineContext)) = i;
        task.Context = task._inlineContext;
        task.ContextSize = sizeof(int);
        task.Continuation = 0;
        task.TaskName = "CountingTask";
        TaskQueue_Push(&CurrentWorker()->Queue, &task);

        /* let the worker pick the task up */
        YIELD();
    }

    /* the worker stops when the special function ends */
    RETURN();
}

int main(int argc, char* argv[])
{
#if defined(METAC_CORO_FCONTEXT)
    printf("coroutine backend: boost.context (deboost.context)\n");
#else
    printf("coroutine backend: libaco\n");
#endif

    worker_context_t worker = {0};
    RunWorkerThread(&worker, WorkerFunction, 0);

    printf("tasks run: %u\n", gTasksRun);

    int failed = 0;
    if (gTasksRun != 5)
    {
        printf("FAILED: expected 5 tasks to run, but %u did\n", gTasksRun);
        failed = 1;
    }
    if (gTaskCount != 5)
    {
        printf("FAILED: expected 5 task values\n");
        failed = 1;
    }
    for (uint32_t i = 0; i < gTaskCount; i++)
    {
        printf("  task %u: %d\n", i, gTaskValues[i]);
    }

    return failed;
}
