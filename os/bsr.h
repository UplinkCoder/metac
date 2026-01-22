#ifndef _BSR_H_
#define _BSR_H_

#define LOG2(X) \
    (BSR(X) + 1)

#define NEXTPOW2(X) \
    (1 << LOG2(X))

#if (defined(_MSC_VER) && _MSC_VER >= 1400)
#include <intrin.h>
    static unsigned long BSR(uint32_t x)
    {
        unsigned long result;
        _BitScanReverse(&result, x);
        return result;
	}
#elif ((defined(__GNUC__) || defined(__clang__)) && !defined(__TINYC__))
#  define BSR(X) \
    (__builtin_clz(X) ^ 31)
#else
#  include "../3rd_party/bsr.c"
#endif

#endif // _BSR_H_
