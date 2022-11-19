#ifndef _BSF_H_
#define _BSF_H_

#if defined(_MSC_VER)
#  include <intrin.h>
    static unsigned long BSF(uint32_t x)
    {
        unsigned long result;
        _BitScanForward(&result, x);
        return result;
	}
#elif defined(__TINYC__)
#  include "../3rd_party/bsf.c"
#else
#  define BSF(X) \
    (__builtin_ctz(X))
#endif

#endif // _BSF_H_
