#include "rwlock.h"
#include <stdio.h>
int main(int argc, const char** argv)
{
    printf("Testing locks\n");

    RWLock lock1 = {0};
    RWLock lock2 = {0};
    // both start out unlocked

    printf("TryWriteLock: %d ... should be 1\n",
    RWLock_TryWriteLock(&lock1));

    printf("lock1 TryWriteLock: %d ... should be 0\n",
    RWLock_TryWriteLock(&lock1));

    RWLock_ReleaseWriteLock(&lock1);
    printf("TryWriteLock: %d ... should be 1\n",
    RWLock_TryWriteLock(&lock1));

    RWLock_ReleaseWriteLock(&lock1);
    printf("releasing write lock twice  ... should crash\n");
    RWLock_ReleaseWriteLock(&lock1);
}
