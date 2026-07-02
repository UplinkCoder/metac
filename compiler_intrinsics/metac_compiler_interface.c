#include <stdio.h>
#include "metac_compiler_interface.h"
#include "../semantic/metac_type.h"
#include "metac_type_kind.c"

extern metac_compiler_t compiler;

static void compiler_Message(struct metac_compiler_t* compilerP,
                      const char* str)
{
}

static void compiler_Error(struct metac_compiler_t* compilerP,
                    const char* str)
{

}

static metac_type_kind_t compiler_GetTypeKind (uint32_t T)
{
    metac_type_index_t typeIdx;
    typeIdx.v = T;
    return (metac_type_kind_t)TYPE_INDEX_KIND(typeIdx);
}

static const char* compiler_TypeKindString(metac_type_kind_t T)
{
    return MetaCTypeKind_toChars(T);
}

static const char* compiler_Help ()
{
    return "Hello I am Mr. compiler. I cannot help you ...";
}


static metac_enum_members_t* compiler_GetEnumMembers (struct metac_compiler_t* compilerP, uint32_t T)
{

}




metac_compiler_t compiler = {
    0,

    compiler_Help,

    compiler_Message,
    compiler_Error,

    compiler_GetTypeKind,
    compiler_TypeKindString,

    compiler_GetEnumMembers,

     "v0.1",
     20260205,
};

