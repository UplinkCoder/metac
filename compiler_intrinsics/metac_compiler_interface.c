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

uint32_t* compiler_CurrentTimeStamp (uint32_t* ts)
{
    OS.GetTimeStamp(ts);
    return ts;
}

uint32_t compiler_Message (const char* msg)
{

}

uint32_t compiler_Error ()
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
    // hard code bultinNumber since it's called with the wrong
    // prototype at the moment
    builtinNumber = 1;
    return names[builtinNumber];
}

const char* compiler_help ()
{
    return "Hello I am Mr. compiler. I cannot help you ...";
}


const char* compiler_msg ()
{
    return "you called compiler message, and it should not return a string";
}


metac_parser_t* GetCurrentParser(metac_compiler_t* compilerP)
{

}

void compiler_RegisterIdentifierCallback (metac_compiler_t* compilerP,
                                          void (*IdentifierCb)(const char* idChars, uint32_t idKey, void* userCtx),
                                          void* userContext)
{
    metac_parser_t* parser = GetCurrentParser(compilerP);
    identifier_callback_t idCallback;

    idCallback.FuncP = IdentifierCb;
    idCallback.Ctx = userContext;

    ARENA_ARRAY_ADD(parser->IdentifierCallbacks, idCallback);
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

    compiler_Message,
    compiler_Error,

    0,

    compiler_RegisterIdentifierCallback,
};

