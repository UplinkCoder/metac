#include "compat.h"
static inline char* u64tostr(uint64_t v, char buffer[21])
{
    int i;
    buffer[20] = '\0';

    if (!v)
    {
        buffer[19] = '0';
        return &buffer[19];
    }

    for(i = 19; v > 0; --i)
    {
        buffer[i] = (v % 10) + '0';
        v /= 10;
    }
    return buffer + i + 1;
}

static inline char* i64tostr(int64_t v, char buffer[22])
{
    int wasNegative = (v & (1LL << 63LL)) != 0;

    if (wasNegative) v = ~v + 1;

    char* result = u64tostr(v, buffer + wasNegative);

    if (wasNegative) {
        (--result)[0] = '-';
    }

    return result;
}

static inline char* u64tohexstr(uint64_t v, char buffer[17])
{
    buffer[16] = '\0';
    for(int i = 0; i < 16; i++)
        buffer[i] = '0';

    if (!v)
    {
        buffer[15] = '0';
        return &buffer[15];
    }

    int i;
    for(i = 15; v > 0; --i)
    {
        uint8_t nibble = v & 0xF;
        buffer[i] = nibble + ((nibble > 9) ? ('A' - 10) : '0');
        v = (v & ~0xFUL) >> 4;
    }
    return buffer + i + 1;
}
