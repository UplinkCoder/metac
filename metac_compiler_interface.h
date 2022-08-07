typedef unsigned int uint32_t;

enum node_type
{
    some_type,
    antr_type,
};

typedef struct metac_compiler_t
{
    uint32_t StartTimeStamp;
    uint32_t (*CurrentTimeStamp)();
    uint32_t (*BuiltinCount)();

    const char* (*BuiltinName)(uint32_t builtinNumber);
    const char* (*help)();

    void (*message)(const char* str);
    void (*error)(const char* str);

    void (*RegisterLogCallback)(void (*LogCb)(const char* msg, void* context), void* context);
} metac_compiler_t;
