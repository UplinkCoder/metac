#ifndef NO_FIBERS
static task_t idleTask;

#include "../3rd_party/libaco/aco.c"

#  ifndef ACOSW_EXTERNAL_ASM
#    include "../3rd_party/libaco/acosw.c"
#  endif
#endif
