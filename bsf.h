#ifndef _BSF_H_
#define _BSF_H_

#if defined(_MSC_VER)
    unsigned long BSF(uint32_t x)
    {
        unsigned long result;
        _BitScanForward(&result, x);
        return result;
	}
#elif defined(__TINYC__)
#  include "3rd_party/bsf.c"
#else
#  define BSF(X) \
    (__builtin_ctz(X) + 1)
#endif

#endif // _BSF_H_
