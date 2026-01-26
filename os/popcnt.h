#ifndef _POPCNT_H_
#define _POPCNT_H_

#if (defined(_MSC_VER) && _MSC_VER >= 1400)
#  include <intrin.h>
#  define POPCNT(X) __popcnt(X)
#elif ((defined(__GNUC__) || defined(__clang__)) && !defined(__TINYC__))
#  define POPCNT(X) __builtin_popcount(X)
#else
#  include "../3rd_party/popcnt.c"
#endif

#endif // _POPCNT_H_
