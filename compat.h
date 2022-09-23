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

#if defined(_MSC_VER)
#  define snprintf _snprintf
#endif

#ifndef HAVE_STDINT_H
typedef signed char int8_t;
typedef unsigned char uint8_t;
typedef signed short int int16_t;
typedef unsigned short int uint16_t;
typedef signed int int32_t;
typedef unsigned int uint32_t;

#  if defined(_MSC_VER)
    typedef signed __int64       int64_t;
    typedef unsigned __int64     uint64_t
#  else
    typedef signed long int int64_t;
    typedef unsigned long int uint64_t;
#  endif
#  define UINT32_MAX (cast(uint32_t)0xffffffff)
#  define UINT16_MAX (cast(uint16_t)0xffff)

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

