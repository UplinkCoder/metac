/// Small platfrom abstraction
#include "os.h"
#include <sys/mman.h>
#define DEFAULT_PAGESIZE 4096
void* PageAlloc(uint32_t minSize, uint32_t* allocatedSize);


struct OS OS =
{
    DEFAULT_PAGESIZE,
    PageAlloc,
};

void* PageAlloc(uint32_t minSize, uint32_t* allocatedSize)
{
    void* result;
    
    uint32_t allocated = 
        (minSize + (OS.PageSize - 1)) & ~(OS.PageSize - 1);
#if defined(WINDOWS) 
   result = VirtualAlloc(0, allocated, 
                         MEM_RESERVE | MEM_COMMIT,
                         PAGE_READWRITE);
#elif defined(POSIX)
#  if !defined(MAP_ANONYMOUS)
#    define MAP_ANONYMOUS MAP_ANON
#  endif 
    result = mmap(0, allocated,
                  PROT_READ | PROT_WRITE,
                  MAP_PRIVATE | MAP_ANONYMOUS | MAP_POPULATE,
                  -1, 0);
#else
#  error "OS not supported"
#endif
    if (result)
    {
        (*allocatedSize) = allocated;
    }
    
    return result;
}
