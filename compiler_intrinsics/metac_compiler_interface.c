#include "../os/os.h"
#include "metac_compiler_interface.h"
#include "../semantic/metac_type.h"
extern metac_compiler_t compiler;

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
    uint32_t ts;
    OS.GetTimeStamp(&ts);
    return ts;
}

void compiler_Message(struct metac_compiler_t* compilerP,
                      const char* str)
{
}

void compiler_Error(struct metac_compiler_t* compilerP,
                    const char* str)
{
    
}

type_kind_t compiler_GetTypeKind (struct metac_compiler_t* compilerP, uint32_t T)
{
    metac_type_index_t typeIdx;
    typeIdx.v = T;
    return TYPE_INDEX_KIND(typeIdx);
}


const char* compiler_Help ()
{
    return "Hello I am Mr. compiler. I cannot help you ...";
}


void compiler_PrintInt(int32_t value)
{
    char Buffer[24];
    int len = sprintf(Buffer, "%d", value);
    Buffer[len] = '\0';
    //compiler.Message(&compiler, Buffer, 0);
    fprintf(stderr, "-> %s\n", Buffer);
}
metac_enum_members_t* compiler_GetEnumMembers (struct metac_compiler_t* compilerP, uint32_t T)
{
    
}

metac_node_t compiler_ResolveNode(struct metac_compiler_t* compilerP, const char* name)
{
    return 0;
}



    uint32_t (*CurrentTimeStamp) ();
   
    const char* (*Help) ();

    void (*Message) (struct metac_compiler_t* compilerP,
                     const char* str);

    void (*Error) (struct metac_compiler_t* compilerP,
                   const char* str);

    type_kind_t (*GetTypeKind) (struct metac_compiler_t* compilerP, type* T);

    void (*PrintInt) (int32_t value);

    metac_enum_members_t* (*GetEnumMembers) (struct metac_compiler_t* compilerP, type* T);

    metac_node_t (*ResolveNode)(struct metac_compiler_t* compilerP, const char* name);


metac_compiler_t compiler = {
    0,

    compiler_CurrentTimeStamp,
    compiler_Help,

    compiler_Message,
    compiler_Error,

    compiler_GetTypeKind, // get type kind

    compiler_PrintInt,

    compiler_GetEnumMembers,
    
    compiler_ResolveNode,
    
     "v0.1",
};

