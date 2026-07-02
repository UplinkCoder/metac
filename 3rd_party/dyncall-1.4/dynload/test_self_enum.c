#include "dynload.h"
#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>

__attribute__((visibility("default"))) extern int f() {
    printf("Exported_f\n");
}

int main(int argc, const char* argv[])
{
    DLSyms* mySyms = dlSymsInit("/proc/self/exe");
    int32_t nSyms = dlSymsCount(mySyms);
    printf("going to enum our syms [%d]\n", nSyms);
    for(int i = 0; i < nSyms; i++)
    {
        printf("sym [%d] : '%s'\n", i, dlSymsName(mySyms, i));
    }    
}
