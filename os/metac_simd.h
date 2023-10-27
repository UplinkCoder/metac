#ifndef _METAC_SIMD_H_
#define _METAC_SIMD_H_

#if defined(__aarch64__) && !defined(__TINYC__)
#  define NEON
#endif

#include "../os/compat.h"

#  if !defined(NEON)
#    if defined(SSE2)
#      include <xmmintrin.h>
#    endif

#    define E16(SIMD_REG, IDX) \
         SIMD_REG.E[IDX]

#    pragma pack(push, 16)
typedef struct int16x8_t
{
    union {
      uint16_t E[8];
      uint64_t EX[2];
#    if defined(SSE2)
    __m128i  XMM;
#    endif
    };
} int16x8_t;
#    pragma pack(pop)
#  elif defined (NEON)
#    include <arm_neon.h>
#    define E16(SIMD_REG, IDX) \
        SIMD_REG[IDX]
#  else
#    error "this can't happen"
#  endif

// Set all elements of a vector to a 16-bit value
static inline int16x8_t Set1_16(int16_t value);

// Perform element-wise bitwise AND operation between two vectors
static inline int16x8_t And16(int16x8_t a, int16x8_t b);

// Compare equality of corresponding elements in two vectors
// returns a mask
static inline int16x8_t Eq16(int16x8_t a, int16x8_t b);

// Extract the most significant bit of each element in a vector and concatenate them into a 8-bit integer
static inline uint32_t MoveMask16(int16x8_t a);

// Perform element-wise bitwise AND-NOT operation between two vectors
static inline int16x8_t Andnot16(int16x8_t a, int16x8_t b);

// Load 8 consecutive 16-bit values from memory address
static inline int16x8_t Load16(int16x8_t const* ptr);

// Store 8 consecutive 16-bit values to memory address
static inline void Store16(int16x8_t* ptr, int16x8_t value);

#  if defined (SSE2)
/// taken from https://github.com/AuburnSounds/intel-intrinsics/blob/master/source/inteli/emmintrin.d
/// Thanks Guillaume!
static inline uint32_t MoveMask16( const int16x8_t a )
{
    return _mm_movemask_epi8(_mm_packs_epi16(a.XMM, _mm_setzero_si128()));
}

static inline int16x8_t Set1_16(const int16_t v)
{
    int16x8_t result;
    result.XMM = _mm_set1_epi32(v | (v << 16));
    return result;
}

static inline int16x8_t And16(const int16x8_t a, const int16x8_t b)
{
    int16x8_t result;
    result.XMM = _mm_and_si128(a.XMM, b.XMM);
    return result;
}

static inline int16x8_t Andnot16(const int16x8_t a, const int16x8_t b)
{
    int16x8_t result;
    result.XMM = _mm_andnot_si128(a.XMM, b.XMM);
    return result;
}

static inline int16x8_t Eq16(const int16x8_t a, const int16x8_t b)
{
    int16x8_t result;
    result.XMM = _mm_cmpeq_epi16(a.XMM, b.XMM);
    return result;
}

static inline int16x8_t Load16(int16x8_t const * ptr)
{
    int16x8_t result;
    result.XMM = _mm_loadu_si128((__m128i*)ptr->E);
    return result;
}

static inline void Store16(int16x8_t* ptr, const int16x8_t value)
{
    _mm_storeu_si128((__m128i*)ptr->E, value.XMM);
}
#  elif defined(NEON)
static inline uint32_t MoveMask16(const int16x8_t a)
{
    const int16x8_t multi = {1 << 0, 1 << 1, 1 << 2, 1 << 3,
                             1 << 4, 1 << 5, 1 << 6, 1 << 7};
    // horizontal add over all elements to get the result
    return vaddvq_s16(a & multi);
}

static inline int16x8_t Set1_16(const int16_t v)
{
    return (int16x8_t){v, v, v, v,
                       v, v, v, v};
}

static inline int16x8_t And16(const int16x8_t a, const int16x8_t b)
{
    return a & b;
}

static inline int16x8_t Andnot16(const int16x8_t a, const int16x8_t b)
{
    return (~a) & b;
}

static inline int16x8_t Eq16(const int16x8_t a, const int16x8_t b)
{
    return a == b;
}

static inline int16x8_t Load16(int16x8_t const * ptr)
{
    return *ptr;
}

static inline void Store16(int16x8_t* ptr, const int16x8_t value)
{
    *ptr = value;
}
#  else // no supported SIMD_PLATFROM
static inline uint32_t MoveMask16(const int16x8_t a)
{
    uint32_t result = 0;

    for(int i = 0; i < 8; i++)
    {
        if (a.E[i] == 0xFFFF)
        {
            result |= (1 << i);
        }
    }

    return result;
}

static inline int16x8_t Set1_16(const int16_t v)
{
    int16x8_t result;

    for(int i = 0; i < 8; i++)
    {
        result.E[i] = v;
    }

    return result;
}

static inline int16x8_t And16(const int16x8_t a, const int16x8_t b)
{
    int16x8_t result = {0};

    for(int i = 0; i < 8; i++)
    {
        result.E[i] = (a.E[i] & b.E[i]);
    }

    return result;
}

static inline int16x8_t Andnot16(const int16x8_t a, const int16x8_t b)
{
    int16x8_t result = {0};

    for(int i = 0; i < 8; i++)
    {
        result.E[i] = ((~a.E[i]) & b.E[i]);
    }

    return result;
}

static inline int16x8_t Eq16(const int16x8_t a, const int16x8_t b)
{
    int16x8_t result = {0};

    for(int i = 0; i < 8; i++)
    {
        result.E[i] = ((a.E[i] == b.E[i]) ? 0xFFFF : 0);
    }

    return result;
}

static inline int16x8_t Load16(int16x8_t const * ptr)
{
    int16x8_t result;

    for(int i = 0; i < 8; i++)
    {
        result.E[i] = ptr->E[i];
    }

    return result;
}

static inline void Store16(int16x8_t* ptr, const int16x8_t value)
{
    for(int i = 0; i < 8; i++)
    {
        ptr->E[i] = value.E[i];
    }
}
#  endif

#endif // _METAC_SIMD_H_
