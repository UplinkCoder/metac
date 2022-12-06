#ifndef NO_FIBERS
#  include "../3rd_party/libaco/aco.h"
#endif

#define START(CO, ARG) do { \
/*    printf("Starting %x from {%s:%d}\n", CO, __FILE__, __LINE__);*/ \
    aco_resume(CO); \
} while(0);

#define RESUME(CO) do { \
/*    printf("Resuming %x from {%s:%d}\n", CO, __FILE__, __LINE__);*/ \
    aco_resume(CO); \
} while(0);

#ifndef NO_FIBERS
#  define YIELD(REASON) do { \
/*    printf("Yielding %x from {%s:%d} %s\n",  (GET_CO()), __FILE__, __LINE__, #REASON);*/ \
    ((task_t*)(GET_CO())->arg)->YieldReason = #REASON; \
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
