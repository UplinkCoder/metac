#include <sys/resource.h>

#define Megabytes(N) ((N) * 1024 * 1024)  

void enforce_limits() {
    struct rlimit mem_limit;
    mem_limit.rlim_cur = Megabytes(256); // 256MB Soft Limit
    mem_limit.rlim_max = Megabytes(256); // 256MB Hard Limit
    setrlimit(RLIMIT_AS, &mem_limit);
}
