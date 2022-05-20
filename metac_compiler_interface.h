typedef unsigned int uint32_t;
typedef struct metac_compiler_t
{
    uint32_t StartTimeStmap;
    uint32_t (*CurrentTimeStamp)();
    uint32_t (*BuiltinCount)();
    const char* (*BuiltinName)(uint32_t builtinNumber);
} metac_compiler_t;
