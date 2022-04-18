#define cast(T) (T)

#ifdef _WIN32
#  include "stdint_msvc.h"
# ifndef __cplusplus
#  error "win32 compile only works in c++ mode ... use /TP"
# endif
#else
#  include <stdint.h>
#endif

#ifndef __cplusplus
# include "stdbool.h"
#endif


#ifdef __cplusplus
#  define EXTERN_C extern "C"
#else
#  define EXTERN_C extern
#endif

#ifdef _MSC_VER
#  if _MSC_VER <= 1800
#    define inline
#  endif
#endif

