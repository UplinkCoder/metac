#ifndef _RWLOCK_H_
#define _RWLOCK_H_
// Written by Bruce Carneal
// Ported to C by Stefan Koch

/// the lock is not reentrant wrt writing but reentrant read acquisition is fine
/// bit 31, the sign bit, indicates a writer is active
/// bits 0..30 keep a count of the number of active readers (ok to be inexact transitorily)
/// the lock becomes racy once the active reader count hits 2^31 - 1

#include "metac_atomic.h"
#include "../compat.h"
#include <assert.h>

typedef struct RWLock
{
#ifdef _MSC_VER
    volatile long _rwctr;
#else
    volatile int32_t _rwctr;
#endif
} RWLock;


static inline bool RWLock_TryReadLock(RWLock *self)
{
  bool result = 1;

  if ( _InterlockedIncrement(&self->_rwctr) <= 0 )
  {
    _InterlockedDecrement(&self->_rwctr);
    result = 0;
  }

  return result;
}

static inline void RWLock_ReleaseReadLock(RWLock *self)
{
  if ( _InterlockedDecrement(&self->_rwctr) < 0 )
  {
    //"read lock underflow on release";
    assert(0);
  }
}

static inline bool RWLock_TryWriteLock(RWLock* self)
{
  return _InterlockedCompareExchange(&self->_rwctr, 0x80000000, 0) == 0;
}

static inline void RWLock_ReleaseWriteLock(RWLock *self)
{
  if ( _InterlockedExchangeAdd(&self->_rwctr, 0x80000000) != 0x80000000 )
  {
    //"write lock confusion on release";
    assert(0);
  }
}
#endif

#define RLOCK(LOCK) do { \
    while (!RWLock_TryReadLock(LOCK)) \
    { \
        MM_PAUSE() \
    } \
} while (0) \
FENCE()

#define WLOCK(LOCK) do { \
    while (!RWLock_TryWriteLock(LOCK)) \
    { \
        MM_PAUSE() \
    } \
} while (0) \
FENCE()

#define RWLOCK(LOCK) do { \
    RWLock_ReleaseReadLock(LOCK); \
    while (!RWLock_TryWriteLock(LOCK)) \
    { \
        MM_PAUSE() \
    } \
} while (0)
