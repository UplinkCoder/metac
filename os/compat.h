#ifndef _COMPAT_H_
#define _COMPAT_H_

#define cast(T) (T)

#ifndef ARRAY_SIZE
#  define ARRAY_SIZE(A) \
     ((unsigned int)(sizeof((A)) / sizeof((A)[0])))
#endif

#if !defined(_MSC_VER)
#  define noinline __attribute__ ((noinline))
#else
#  define noinline __declspec(noinline)
#endif

#if (defined(_MSC_VER) && _MSC_VER < 1800)
#  define snprintf _snprintf
#endif

#if defined(_MSC_VER)
#  include "../3rd_party/stdint_msvc.h"
#elif defined(HAVE_STDINT_H)
#  include <stdint.h>
#else
#  include <stdint.h>
#endif

#if defined(_MSC_VER)
#  ifndef __cplusplus
#    error "win32 compile only works in c++ mode ... use /TP"
#  endif
#endif

#if !defined( __STDC_VERSION__ ) || __STDC_VERSION__ < 201112L
# ifdef __COUNTER__
  /* microsoft */
#  define STATIC_ASSERT(E, M) \
    enum { CAT(static_assert_, __COUNTER__) = 1/(int)(!!(E)) }
# else
#  define STATIC_ASSERT(E, M) \
    enum { CAT(assert_line_, __LINE__) = 1/(int)(!!(E)) }
# endif
#else
# define STATIC_ASSERT(E, M) _Static_assert(E, M)
#endif

#  ifdef __CC65__
#  define bool _Bool
typedef unsigned char _Bool;
#define inline

/* Standard test-results. */
#  define false 0
#  define true  1
#else
#  ifndef __cplusplus
#     include "stdbool.h"
#  endif
#endif

#if defined(__GNUC__)
#  define UNLIKELY(X) \
    __builtin_expect((X), 0)
#else
#  define UNLIKELY(X) (X)
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

#endif
