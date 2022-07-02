#ifdef _WIN32
#  include "stdint_msvc.h"
#else
#  include <stdint.h>
#endif

#include <assert.h>

#ifdef __ARM_FEATURE_CRC32
#  define ARM_NEON_CRC32C
#endif

#ifdef ARM_NEON_CRC32C
# include <arm_acle.h>
#  define NO_CRC32C_TABLE
#endif

#ifdef __cplusplus
#  define EXTERN_C extern "C"
#else
#  define EXTERN_C extern
#endif

#ifdef _MSC_VER
#    define inline
#endif
// exports

uint32_t crc32c_nozero(uint32_t crc, const void* s, const uint32_t len_p);

#define FINALIZE_CRC32C(CRC) \
    ((CRC) ^ 0xFFFFFFFF)
#define INITIAL_CRC32C ((uint32_t)0xFFFFFFFF)

// implementation
#ifndef NO_CRC32C_TABLE
const unsigned int crc32Table[256] = {
    0x00000000U, 0xF26B8303U, 0xE13B70F7U, 0x1350F3F4U,
    0xC79A971FU, 0x35F1141CU, 0x26A1E7E8U, 0xD4CA64EBU,
    0x8AD958CFU, 0x78B2DBCCU, 0x6BE22838U, 0x9989AB3BU,
    0x4D43CFD0U, 0xBF284CD3U, 0xAC78BF27U, 0x5E133C24U,
    0x105EC76FU, 0xE235446CU, 0xF165B798U, 0x030E349BU,
    0xD7C45070U, 0x25AFD373U, 0x36FF2087U, 0xC494A384U,
    0x9A879FA0U, 0x68EC1CA3U, 0x7BBCEF57U, 0x89D76C54U,
    0x5D1D08BFU, 0xAF768BBCU, 0xBC267848U, 0x4E4DFB4BU,
    0x20BD8EDEU, 0xD2D60DDDU, 0xC186FE29U, 0x33ED7D2AU,
    0xE72719C1U, 0x154C9AC2U, 0x061C6936U, 0xF477EA35U,
    0xAA64D611U, 0x580F5512U, 0x4B5FA6E6U, 0xB93425E5U,
    0x6DFE410EU, 0x9F95C20DU, 0x8CC531F9U, 0x7EAEB2FAU,
    0x30E349B1U, 0xC288CAB2U, 0xD1D83946U, 0x23B3BA45U,
    0xF779DEAEU, 0x05125DADU, 0x1642AE59U, 0xE4292D5AU,
    0xBA3A117EU, 0x4851927DU, 0x5B016189U, 0xA96AE28AU,
    0x7DA08661U, 0x8FCB0562U, 0x9C9BF696U, 0x6EF07595U,
    0x417B1DBCU, 0xB3109EBFU, 0xA0406D4BU, 0x522BEE48U,
    0x86E18AA3U, 0x748A09A0U, 0x67DAFA54U, 0x95B17957U,
    0xCBA24573U, 0x39C9C670U, 0x2A993584U, 0xD8F2B687U,
    0x0C38D26CU, 0xFE53516FU, 0xED03A29BU, 0x1F682198U,
    0x5125DAD3U, 0xA34E59D0U, 0xB01EAA24U, 0x42752927U,
    0x96BF4DCCU, 0x64D4CECFU, 0x77843D3BU, 0x85EFBE38U,
    0xDBFC821CU, 0x2997011FU, 0x3AC7F2EBU, 0xC8AC71E8U,
    0x1C661503U, 0xEE0D9600U, 0xFD5D65F4U, 0x0F36E6F7U,
    0x61C69362U, 0x93AD1061U, 0x80FDE395U, 0x72966096U,
    0xA65C047DU, 0x5437877EU, 0x4767748AU, 0xB50CF789U,
    0xEB1FCBADU, 0x197448AEU, 0x0A24BB5AU, 0xF84F3859U,
    0x2C855CB2U, 0xDEEEDFB1U, 0xCDBE2C45U, 0x3FD5AF46U,
    0x7198540DU, 0x83F3D70EU, 0x90A324FAU, 0x62C8A7F9U,
    0xB602C312U, 0x44694011U, 0x5739B3E5U, 0xA55230E6U,
    0xFB410CC2U, 0x092A8FC1U, 0x1A7A7C35U, 0xE811FF36U,
    0x3CDB9BDDU, 0xCEB018DEU, 0xDDE0EB2AU, 0x2F8B6829U,
    0x82F63B78U, 0x709DB87BU, 0x63CD4B8FU, 0x91A6C88CU,
    0x456CAC67U, 0xB7072F64U, 0xA457DC90U, 0x563C5F93U,
    0x082F63B7U, 0xFA44E0B4U, 0xE9141340U, 0x1B7F9043U,
    0xCFB5F4A8U, 0x3DDE77ABU, 0x2E8E845FU, 0xDCE5075CU,
    0x92A8FC17U, 0x60C37F14U, 0x73938CE0U, 0x81F80FE3U,
    0x55326B08U, 0xA759E80BU, 0xB4091BFFU, 0x466298FCU,
    0x1871A4D8U, 0xEA1A27DBU, 0xF94AD42FU, 0x0B21572CU,
    0xDFEB33C7U, 0x2D80B0C4U, 0x3ED04330U, 0xCCBBC033U,
    0xA24BB5A6U, 0x502036A5U, 0x4370C551U, 0xB11B4652U,
    0x65D122B9U, 0x97BAA1BAU, 0x84EA524EU, 0x7681D14DU,
    0x2892ED69U, 0xDAF96E6AU, 0xC9A99D9EU, 0x3BC21E9DU,
    0xEF087A76U, 0x1D63F975U, 0x0E330A81U, 0xFC588982U,
    0xB21572C9U, 0x407EF1CAU, 0x532E023EU, 0xA145813DU,
    0x758FE5D6U, 0x87E466D5U, 0x94B49521U, 0x66DF1622U,
    0x38CC2A06U, 0xCAA7A905U, 0xD9F75AF1U, 0x2B9CD9F2U,
    0xFF56BD19U, 0x0D3D3E1AU, 0x1E6DCDEEU, 0xEC064EEDU,
    0xC38D26C4U, 0x31E6A5C7U, 0x22B65633U, 0xD0DDD530U,
    0x0417B1DBU, 0xF67C32D8U, 0xE52CC12CU, 0x1747422FU,
    0x49547E0BU, 0xBB3FFD08U, 0xA86F0EFCU, 0x5A048DFFU,
    0x8ECEE914U, 0x7CA56A17U, 0x6FF599E3U, 0x9D9E1AE0U,
    0xD3D3E1ABU, 0x21B862A8U, 0x32E8915CU, 0xC083125FU,
    0x144976B4U, 0xE622F5B7U, 0xF5720643U, 0x07198540U,
    0x590AB964U, 0xAB613A67U, 0xB831C993U, 0x4A5A4A90U,
    0x9E902E7BU, 0x6CFBAD78U, 0x7FAB5E8CU, 0x8DC0DD8FU,
    0xE330A81AU, 0x115B2B19U, 0x020BD8EDU, 0xF0605BEEU,
    0x24AA3F05U, 0xD6C1BC06U, 0xC5914FF2U, 0x37FACCF1U,
    0x69E9F0D5U, 0x9B8273D6U, 0x88D28022U, 0x7AB90321U,
    0xAE7367CAU, 0x5C18E4C9U, 0x4F48173DU, 0xBD23943EU,
    0xF36E6F75U, 0x0105EC76U, 0x12551F82U, 0xE03E9C81U,
    0x34F4F86AU, 0xC69F7B69U, 0xD5CF889DU, 0x27A40B9EU,
    0x79B737BAU, 0x8BDCB4B9U, 0x988C474DU, 0x6AE7C44EU,
    0xBE2DA0A5U, 0x4C4623A6U, 0x5F16D052U, 0xAD7D5351U
};

static inline uint32_t singletable_crc32c(uint32_t crc, const uint8_t *buf, uint32_t size)
{
    const uint8_t *p = buf;

#  ifdef BY_4_OPT
    uint32_t size4 = size / 4;
    while(size4--)
    {
        const uint32_t fetch = *(uint32_t*)p;

        crc = crc32Table[(crc ^ ((fetch >>  0))) & 0xff] ^ (crc >> 8);
        crc = crc32Table[(crc ^ ((fetch >>  8))) & 0xff] ^ (crc >> 8);
        crc = crc32Table[(crc ^ ((fetch >> 16))) & 0xff] ^ (crc >> 8);
        crc = crc32Table[(crc ^ ((fetch >> 24))) & 0xff] ^ (crc >> 8);
        p += 4;
    }

    size = size & 3;
#  endif

    while (size--)
        crc = crc32Table[(crc ^ *p++) & 0xff] ^ (crc >> 8);

    return crc;
}

#endif // NO_CRC32C_TABLE

#ifdef ARM_NEON_CRC32C
/*
inline static uint32_t* makeTable(uint32_t* result, uint8_t initValue)
{
    for(int i = 0; i < 256; i++)
    {
        uint8_t c = i;
        result[i] = __crc32cb(initValue, c);
    }

    return result;
}
*/
#undef MIN

#define MIN(A, B) \
    ((A) < (B) ? (A) : (B))

static inline uint32_t intrinsic_crc32c(uint32_t crc, const void* s, uint32_t len)
{
    const uint8_t* p = (const uint8_t*) s;


    if (len > 3)
    {
        switch(((uintptr_t)p) & 3)
        {
          case 4 - 3:
            crc = __crc32cb(crc, *p++);
            len--;
          case 4 - 2:
            crc = __crc32cb(crc, *p++);
            len--;
          case 4 - 1:
            crc = __crc32cb(crc, *p++);
            len--;
          default : break;
        }

        assert(((uintptr_t)p) % 4 == 0);

        while(len >= 4)
        {
            crc = __crc32cw(crc, (*(uint32_t*)p));
            p += 4;
            len -= 4;
        }
    }

    switch(len)
    {
      case 3:
        crc = __crc32cb(crc, *p++);
      case 2:
        crc = __crc32cb(crc, *p++);
      case 1:
        crc = __crc32cb(crc, *p++);
      case 0: break ;
      default : assert(0);
    }

    return crc;

}

#undef MIN

#endif

uint32_t crc32c(uint32_t crc, const void* s, const uint32_t len_p)
{
    const uint32_t len = len_p;
    const uint8_t* p = (const uint8_t*) s;
#ifdef ARM_NEON_CRC32C
    crc = intrinsic_crc32c(crc, p, len);
#else
    crc = singletable_crc32c(crc, p, len);
#endif
    return crc;
}

uint32_t crc32c_nozero(uint32_t crc, const void* s, const uint32_t len_p)
{
    crc = crc32c(crc, s, len_p);

    return crc ? crc : ~0;
}

inline uint32_t crc32c_byte(uint32_t crc, uint8_t byte)
{
#ifdef ARM_NEON_CRC32C
    return __crc32cb(crc, byte);
#else
    return crc32Table[(crc ^ byte) & 0xFF] ^ (crc >> 8);
#endif
}


#define CRC32C_S(STRING) \
    (crc32c_nozero(~(uint32_t)0, STRING, sizeof(STRING) - 1))

#ifdef TEST_MAIN
#include <assert.h>
int main(int argc, char* argv[])
{
    assert(CRC32C_S("addr:housenumber") == 0x3F233FF2);
    assert(CRC32C_S("addr:housenumber") == 0x3F233FF2);
//    printf("seems to work\n");
}
#endif
