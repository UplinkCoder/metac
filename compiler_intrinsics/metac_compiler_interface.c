#include "../os/os.h"
#include "metac_compiler_interface.h"
#include "../semantic/metac_type.h"
#include "metac_type_kind.c"

extern metac_compiler_t compiler;

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

metac_type_kind_t compiler_GetTypeKind (uint32_t T)
{
    metac_type_index_t typeIdx;
    typeIdx.v = T;
    return (metac_type_kind_t)TYPE_INDEX_KIND(typeIdx);
}

const char* compiler_TypeKindString(metac_type_kind_t T)
{
    return MetaCTypeKind_toChars(T);
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



metac_compiler_t compiler = {
    0,

    compiler_CurrentTimeStamp,
    compiler_Help,

    compiler_Message,
    compiler_Error,

    compiler_GetTypeKind,
    compiler_TypeKindString,

    compiler_PrintInt,

    compiler_GetEnumMembers,

    compiler_ResolveNode,

     "v0.1",
     20260205,
};

