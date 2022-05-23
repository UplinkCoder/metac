// taken from gcc testsuite
#include "../compat.h"
uint32_t BSF (uint32_t x)
{
  static int table[32] =
    {
      0, 1, 2,24, 3,19, 6,25, 22, 4,20,10,16, 7,12,26,
      31,23,18, 5,21, 9,15,11,30,17, 8,14,29,13,28,27
    };

  uint32_t i = (x & -x) * 0x04D7651F;
  return x ? table[i >> 27] + 1: 0;
}
