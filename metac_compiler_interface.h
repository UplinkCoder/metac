typedef unsigned int uint32_t;
typedef struct metac_compiler_t
{
    uint32_t StartTimeStmap;
    uint32_t (*CurrentTimeStamp)();
    const char* Hello;
} metac_compiler_t;
