#ifndef _METAC_ATOMIC_H_
#define _METAC_ATOMIC_H_

// Define memory fences and CPU pause instructions for different architectures
#if defined(__i386__) || defined(__x86_64__)
#  define FENCE() __asm__ volatile ("mfence" ::: "memory")
#  define MM_PAUSE() // No-op for x86/x86_64
#elif defined(__aarch64__) && !defined(__TINYC__)
#  define FENCE() __asm__ volatile ("dmb sy" ::: "memory")
#  define MM_PAUSE() __asm__ volatile ("yield")
#else
#  define FENCE() // No-op for other architectures
#  define MM_PAUSE() // No-op for other architectures
#endif

// Define atomic operations for different compilers
#if defined(_MSC_VER) || defined(_WIN32)
#  include <intrin.h>
#elif defined(__GNUC__)
#  define _InterlockedIncrement(PTR) (__sync_add_and_fetch((PTR), 1))
#  define _InterlockedDecrement(PTR) (__sync_sub_and_fetch((PTR), 1))
#  define _InterlockedCompareExchange(PTR, NEWVAL, OLDVAL) \
        (__sync_val_compare_and_swap((PTR), (OLDVAL), (NEWVAL)))
#  define _InterlockedExchangeAdd(PTR, VAL) (__sync_fetch_and_add((PTR), VAL))
#else
#  if defined(ATOMIC)
#    error "No atomics for this platform"
#  else
#    define NO_ATOMICS
#  endif
#endif

// Define macros for atomic operations or fall back to non-atomic operations
#if !defined(ATOMIC)
#  define INC(v) (v++)
#  define DEC(v) (v--)
#  define POST_ADD(v, b) (v += b, v - b)
#else
#  define INC(v) _InterlockedIncrement(&(v))
#  define DEC(v) _InterlockedDecrement(&(v))
#  define POST_ADD(v, b) _InterlockedExchangeAdd((long volatile*)(&(v)), b)
#endif

#endif // _METAC_ATOMIC_H_