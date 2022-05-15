#include "../compat.h"

// modified CLZ2 taken from https://embeddedgurus.com/state-space/2014/09/fast-deterministic-and-portable-counting-leading-zeros/
static inline uint32_t BSR(uint32_t x) {
    static uint8_t const clz_lkup[] = {
        32, 31, 30, 30, 29, 29, 29, 29,
        28, 28, 28, 28, 28, 28, 28, 28
    };
    uint32_t n;

    if (x >= (1U << 16)) {
        if (x >= (1U << 24)) {
            if (x >= (1 << 28)) {
                n = 28U;
            }
            else {
                n = 24U;
            }
        }
        else {
            if (x >= (1U << 20)) {
                n = 20U;
            }
            else {
                n = 16U;
            }
        }
    }
    else {
        if (x >= (1U << 8)) {
            if (x >= (1U << 12)) {
                n = 12U;
            }
            else {
                n = 8U;
            }
        }
        else {
            if (x >= (1U << 4)) {
                n = 4U;
            }
            else {
                n = 0U;
            }
        }
    }
    return ((uint32_t)(clz_lkup[x >> n] - n)) ^ 31;
}
