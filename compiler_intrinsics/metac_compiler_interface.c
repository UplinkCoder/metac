#include "metac_compiler_interface.h"

/*
 *     void* semanticState;

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
*/

uint32_t compiler_CurrentTimeStamp ()
{

}

uint32_t compiler_BuiltinCount ()
{
    return 1;
}

const char* compiler_PrintType (type T)
{

}

const char** compiler_FieldNames (type T)
{

}

const char* compiler_BuiltinName (uint32_t builtinNumber)
{
    static const char* names[] = {"not here", "not here either"};
    return names[builtinNumber];
}

const char* compiler_help ()
{
    return "Hello I am Mr. compiler. I cannot help you ...";
}



metac_compiler_t compiler = {
    0,
    0,
    compiler_CurrentTimeStamp,
    compiler_BuiltinCount,

    compiler_PrintType,

    compiler_FieldNames,

    compiler_BuiltinName,
    compiler_help,

    0,
    0,

    0,
};

