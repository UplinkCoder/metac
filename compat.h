#ifdef _WIN32
#  include "stdint_msvc.h"
#else
#  include <stdint.h>
#endif

#ifdef __cplusplus
#  define EXTERN_C extern "C"
#else
#  define EXTERN_C extern
#endif

#ifdef _MSC_VER
#  if _MSC_VER <= 1500
#    define inline
#  endif
#endif

