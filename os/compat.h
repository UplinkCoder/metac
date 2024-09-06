#ifndef _COMPAT_H_
#define _COMPAT_H_

#define _scope

#define xprintf(...) \
    ALIGN_STACK() \
    printf(__VA_ARGS__); \
    RESTORE_STACK();

#define xfprintf(...) \
    ALIGN_STACK() \
    fprintf(__VA_ARGS__); \
    RESTORE_STACK();

#define cast(T) (T)

#ifndef ARRAY_SIZE
#  define ARRAY_SIZE(A) \
     ((unsigned int)(sizeof((A)) / sizeof((A)[0])))
#endif

#if !defined(_MSC_VER)
#  define metac_noinline __attribute__ ((noinline))
#else
#  define metac_noinline __declspec(noinline)
#endif


#if (defined(_MSC_VER) && (_MSC_VER < 1800) )
#  define snprintf _snprintf
#endif

#if (defined(_MSC_VER) && (_MSC_VER < 1600) )
#  define __STDC_LIMIT_MACROS
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

#ifdef __x86_64__

// Macro to align the stack to 16 bytes and restore it
#  define ALIGN_STACK() { \
    static uintptr_t __old_stack_p; \
    asm ( \
        "movq %%rsp, %0;"      /* Move the current value of %rsp to specified variable */ \
        "andq $-16, %%rsp;"    /* Align the stack by bitwise AND with -16 */ \
        : "=m" (__old_stack_p) /* Output operands (OLD_SP) */ \
        :                      /* Input operands (none explicitly) */ \
        : "memory"             /* Clobbered registers */ \
    );
// Macro to restore the stack pointer to its original value
#  define RESTORE_STACK() asm ( \
        "movq %0, %%rsp;"     /* Restore the stack pointer from the specified variable */ \
        :                     /* No output operands */ \
        : "m" (__old_stack_p) /* Input: read the old stack pointer value from the specified variable */ \
        : "memory"            /* Clobbered registers */ \
    ); \
}

#else
#  define RESTORE_STACK()
#  define ALIGN_STACK()
#endif


#define CAT(A, B) \
    CAT2(A, B)

#define CAT2(A, B) \
    A ## B

#if !defined( __STDC_VERSION__ ) || __STDC_VERSION__ < 201112L
# if defined(__COUNTER__)
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
