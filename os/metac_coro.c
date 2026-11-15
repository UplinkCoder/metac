#ifndef NO_FIBERS
static task_t idleTask;

#if defined(METAC_CORO_FCONTEXT)
#  include "metac_coro_fcontext.c"
#else
#  include "../3rd_party/libaco/aco.c"

#  ifndef ACOSW_EXTERNAL_ASM
#    include "../3rd_party/libaco/acosw.c"
#  endif
#endif
#endif
