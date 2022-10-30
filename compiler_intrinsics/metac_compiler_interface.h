typedef unsigned int uint32_t;
typedef void* type;

typedef struct metac_compiler_t
{
    void* semanticState;

    uint32_t StartTimeStamp;
    uint32_t (*CurrentTimeStamp) ();
    uint32_t (*BuiltinCount) ();

    const char* (*PrintType) (type T);

    const char** (*FieldNames) (type T);

    const char* (*BuiltinName) (uint32_t builtinNumber);
    const char* (*help) ();

    void (*message) (const char* str);
    void (*error) (const char* str);

    void (*RegisterLogCallback) (void (*LogCb)(const char* msg, void* context), void* context);
} metac_compiler_t;

static metac_compiler_t* getCompiler();
