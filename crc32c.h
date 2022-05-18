#include "compat.h"

uint32_t crc32c(uint32_t crc, const void* s, const uint32_t len_p);
inline uint32_t crc32c_byte(uint32_t crc, uint8_t byte);

