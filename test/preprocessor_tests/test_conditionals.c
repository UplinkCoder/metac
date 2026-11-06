#include "header.h"

int main() {
    int val = 0;
#ifdef TEST_FLAG
    val = TEST_FLAG;
#else
    val = 0;
#endif

#if TEST_FLAG > 50
    val += 1;
#elif TEST_FLAG > 100
    val += 2;
#else
    val += 3;
#endif

#ifndef UNKNOWN_MACRO
    val += 4;
#endif

    return val;
}
