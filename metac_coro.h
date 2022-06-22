#include "3rd_party/libaco/aco.h"

#define START(CO) do { \
    printf("Starting %x from {%s:%d}\n", CO, __FILE__, __LINE__); \
    aco_resume(CO); \
} while(0);

#define RESUME(CO) do { \
    printf("Resuming %x from {%s:%d}\n", CO, __FILE__, __LINE__); \
    aco_resume(CO); \
} while(0);

#define YIELD(REASON) do { \
    printf("Yielding %x from {%s:%d} %s\n",  (GET_CO()), __FILE__, __LINE__, #REASON); \
    aco_yield(); \
} while(0)
