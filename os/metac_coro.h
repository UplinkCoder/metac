#ifndef NO_FIBERS

/* The backends update the active task of the worker when the control
 * is handed over to another coroutine.  The task system is not known
 * to the backends, so the update is done through this macro (it is
 * expanded where the task system is known). */
#  ifndef METAC_CORO_UPDATE_ACTIVE_TASK
#    define METAC_CORO_UPDATE_ACTIVE_TASK(CO) \
    (CurrentWorker()->ActiveTask = ((task_t*)(CO)->arg))
#  endif

/* The coroutine layer has two implementations:
 *
 *   - libaco (3rd_party/libaco) - the default
 *   - boost.context (3rd_party/deboost.context) - METAC_CORO_FCONTEXT
 *
 * Both of them provide the same API, so nothing above this header has
 * to care which one is used.
 */
#  if defined(METAC_CORO_FCONTEXT)
#    include "metac_coro_fcontext.h"
#  else
#    include "../3rd_party/libaco/aco.h"
#  endif

#endif // NO_FIBERS

#define START(CO, ARG) do { \
/*    printf("Starting %x from {%s:%d}\n", CO, __FILE__, __LINE__);*/ \
    CurrentWorker()->ActiveTask = ((task_t*)(CO)->arg); \
    aco_resume(CO); \
} while(0);

#define RESUME(CO) do { \
/*    printf("Resuming %x from {%s:%d}\n", CO, __FILE__, __LINE__);*/ \
    CurrentWorker()->ActiveTask = ((task_t*)(CO)->arg); \
    assert(CurrentWorker()->ActiveTask->TaskFlags & Task_Running); \
    aco_resume(CO); \
} while(0);

#define STRINGIZE(x) STRINGIZE2(x)
#define STRINGIZE2(x) #x
#define LOCSTR ("+" STRINGIZE(__LINE__) " " __FILE__)

#ifndef NO_FIBERS
#  define YIELD(REASON) do { \
/*    printf("Yielding %x from {%s:%d} %s\n",  (GET_CO()), __FILE__, __LINE__, #REASON);*/ \
    ((task_t*)(GET_CO())->arg)->YieldReason = #REASON; \
    ((task_t*)(GET_CO())->arg)->YieldLoc = LOCSTR; \
    CurrentWorker()->ActiveTask = &idleTask; \
    aco_yield(); \
} while(0)
#  define YIELD_WORKER() do { \
    aco_yield(); \
} while(0)
#else
#  define YIELD(REASON) do { \
    fprintf(stderr, "No yielding supported -- " REASON); \
    assert(0); \
} while (0)
#endif
#define RETURN() do { \
/*    printf("Yielding %x from {%s:%d} %s\n",  (GET_CO()), __FILE__, __LINE__, #REASON);*/ \
    aco_exit(); \
} while(0)
