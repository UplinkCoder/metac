#include "metac_simd.h"

uint32_t test(const int16_t inHash, const int16x8_t inHashes)
{
    uint32_t mask = 0;
    int16x8_t hashes = inHashes;

    uint16_t hash12 = inHash & 0xFFF0;

    uint32_t startSearch = 0;

#if defined(SIMD)
    const int16x8_t hash8 = Set1_16(hash12);
    const int16x8_t hashMask = Set1_16(0xFFF0);
    const int16x8_t maskedHashes = And16(hashes, hashMask);
    const int16x8_t matches = Eq16(maskedHashes, hash8);
    mask = MoveMask16(matches);
#elif defined(NEON)
    const int16x8_t hash8 =
        {hash12, hash12, hash12, hash12,
         hash12, hash12, hash12, hash12};
    const int16x8_t hashMask =
        {0xFFF0, 0xFFF0, 0xFFF0, 0xFFF0,
         0xFFF0, 0xFFF0, 0xFFF0, 0xFFF0};

    const int16x8_t maskedHashes = hashes & hashMask;
    const int16x8_t matches = maskedHashes == hash8;
    // == sets the result element to 0xFF on match
    const int16x8_t multi = {1 << 0, 1 << 1, 1 << 2, 1 << 3,
                             1 << 4, 1 << 5, 1 << 6, 1 << 7};
    // horizontal add over all elements to get the result
    mask = vaddvq_s16(matches & multi);
#else
#endif
    return mask;
}
