#include "../os/compat.h"

uint32_t crc32c_nozero(uint32_t crc, const void* s, const uint32_t len_p);
uint32_t crc32c_byte(uint32_t crc, uint8_t byte);

#define CRC32C_VALUE(HASH, VAL) \
    (crc32c_nozero(HASH, &(VAL), sizeof(VAL)))


