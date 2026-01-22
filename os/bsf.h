#ifndef _BSF_H_
#define _BSF_H_

#if (defined(_MSC_VER) && _MSC_VER >= 1400)
#  include <intrin.h>
    static unsigned long BSF(uint32_t x)
    {
        unsigned long result;
        _BitScanForward(&result, x);
        return result;
	}
#elif ((defined(__GNUC__) || defined(__clang__)) && !defined(__TINYC__))
#  define BSF(X) \
    (__builtin_ctz(X))
#else
#  include "../3rd_party/bsf.c"
#endif

#endif // _BSF_H_
