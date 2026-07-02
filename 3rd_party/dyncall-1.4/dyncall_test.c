#include <stdio.h>
#include "dyncall/dyncall.h"
int f(int a, int b, int c)
{
    printf("(%d + %d) * %d\n", a, b, c);
    return (a + b) * c;
}

int main(int argc, const char* argv[])
{
   DCCallVM* vm = dcNewCallVM(4096);
   dcMode(vm, DC_CALL_C_DEFAULT);
   dcReset(vm);
   int result;
   dcArgInt(vm, 17);
   dcArgInt(vm, 3);
   dcArgInt(vm, 10);
   result = dcCallInt(vm, &f);
   dcFree(vm);
   printf("\t=%d\n", result);
}

