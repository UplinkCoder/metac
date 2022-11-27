#include "../metac_task.c"
#include "../metac_coro.c"
#include "../metac_coro.h"

void Do_PrintTask(task_t* task)
{
    int arg = *((int*)task->Context);
    printf("Arg %d\n", arg);
}

void Push_PrintTask(taskqueue_t* q, int n)
{
    task_t task;
    task.TaskFlags = 0;
    task.TaskFunction = Do_PrintTask;
    (*((int*)task._inlineContext)) = n;
    task.Context = task._inlineContext;
    task.ContextSize = sizeof(n);
    task.Continuation = 0;
    TaskQueue_Push(q, &task);
}

void run5Times(void)
{
    for(int i = 5; i > 0; --i)
    {
        printf("%d\n", i);
        Push_PrintTask(&CurrentWorker()->Queue, i);
        YIELD();
    }
    RETURN();
}



bool ExecFront(taskqueue_t* q)
{
    task_t task;
    if (TaskQueue_Pull(q, &task))
    {
        task.TaskFunction(&task);
        return true;
    }
    else
    {
        return false;
    }
}

#include <assert.h>

taskqueue_t gQueue;
int main(int argc, const char* argv[])
{
    taskqueue_t myQ = {0};
    taskqueue_t* q = &gQueue;
    Taskqueue_Init(q);

    printf("Let's try our Queue\n");

    assert(TaskQueue_TasksInQueueFront_(q) == 0);

    Push_PrintTask(q, 1);
    assert(TaskQueue_TasksInQueueFront_(q) == 1);
    Push_PrintTask(q, 5);
    assert(TaskQueue_TasksInQueueFront_(q) == 2);

    ExecFront(q);
    assert(TaskQueue_TasksInQueueFront_(q) == 1);
    ExecFront(q);
    assert(TaskQueue_TasksInQueueFront_(q) == 0);


    printf("Lets now run a worker which will attempt to go into it's work loop 5 times\n");

    worker_context_t worker;
    RunWorkerThread(&worker, run5Times, 0);

    return 0;
}
