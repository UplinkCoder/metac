#ifndef _OS_H_
#define _OS_H_
#include "compat.h"

#ifdef __unix__
#  include <unistd.h>
#  define POSIX
#endif

#ifdef _MSC_VER
#    <memoryapi.h>
#endif

struct OS
{
    uint32_t PageSize;
    
    void* (*PageAlloc)(uint32_t minSize, uint32_t* allocatedSize);
};

extern struct OS OS;
#endif // _OS_H_
