#include "../os/compat.h"
#ifndef _POPCNT_C_
#define _POPCNT_C_
// taken from hackers delight.
static inline uint32_t POPCNT(uint32_t v) {
    v = v - ((v >> 1) & 0x55555555);
    v = (v & 0x33333333) + 
        ((v >> 2) & 0x33333333);
    v = ((v + (v >> 4)) & 0x0F0F0F0F);
   return (v * 0x01010101) >> 24;
}
#endif