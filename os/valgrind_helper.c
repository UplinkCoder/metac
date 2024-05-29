#include <valgrind/memcheck.h>
#include <stdint.h>


#ifdef VALGRIND
void mark_memory_defined(void* memP, size_t sz)
{
    VALGRIND_MAKE_MEM_DEFIND(memP, sz);
}

void mark_memory_undefined(void* memP, size_t sz)
{
    VALGRIND_MAKE_MEM_UNDEFIND(memP, sz);
}

void mark_memory_noaccess(void* memP, size_t sz)
{
    VALGRIND_MAKE_MEM_NOACCESS(memP, sz);
}
#else
#define mark_memory_defined(P, S)
#define mark_memory_undefined(P, S)
#define mark_memory_noaccess(P, S)
#endif
/*
+    mark_memory_defined(debugServer->Logs,
+                        (debugServer->LogsCount + 1) * sizeof(*debugServer->Logs));
     ARENA_ARRAY_ADD(debugServer->Logs, log);
+    mark_memory_noaccess(debugServer->Logs,
+                         debugServer->LogsCount * sizeof(*debugServer->Logs));
*/
