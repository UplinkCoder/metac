#ifndef _METAC_ATOMIC_H_
#define _METAC_ATOMIC_H_

#if defined(__i386__) || defined(__x86_64__)
#  define FENCE() __asm__ volatile ("mfence" ::: "memory");
#  define MM_PAUSE()
#elif defined(__aarch64__)
# define FENCE() __asm__ volatile ("dmb sy" ::: "memory");
# define MM_PAUSE() __asm__ volatile ("yield");
#else
#  define FENCE()
#  define MM_PAUSE()
#endif

#if defined(_MSC_VER)
#  include <intrin.h>
#elif defined (__GNUC__)
#  define _InterlockedIncrement(PTR) \
      (__sync_add_and_fetch((PTR), 1))

#  define _InterlockedDecrement(PTR) \
      (__sync_sub_and_fetch((PTR), 1))

#  define _InterlockedCompareExchange(PTR, NEWVAL, OLDVAL) \
      (__sync_val_compare_and_swap((PTR), (OLDVAL), (NEWVAL)))

#  define _InterlockedExchangeAdd(PTR, VAL) \
    (__sync_fetch_and_add((PTR), VAL))
#else
# if defined(ATOMIC)
#  error "No atomics for this platfrom"
# else
#  define NO_ATOMICS
# endif
#endif

#if !defined(ATOMIC)
#  define INC(v) \
    (v++)
#  define DEC(v) \
    (v--)
#  define POST_ADD(v, b) \
    _InterlockedExchangeAdd(cast(long volatile*)(&(v)), b)
#else
#  define INC(v) \
  _InterlockedIncrement(&v);
#  define DEC(v) \
  _InterlockedDecrement(&v);
#endif


#endif
