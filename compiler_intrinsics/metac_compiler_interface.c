#include <stdio.h>
#include "metac_compiler_interface.h"
#include "../semantic/metac_type.h"
#include "../semantic/metac_semantic.c"
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
    metac_sema_state_t* sema = (metac_sema_state_t*) compilerP->semanticState;
    metac_type_enum_t* enumType = EnumTypePtr(sema, TYPE_INDEX_INDEX(T));
    metac_alloc_t alloc =  sema->TempAlloc;
    uint32_t nMembers = enumType->MemberCount;
    metac_enum_members_t *memberMemory = Allocator_Calloc(&alloc, metac_enum_members_t, 1);
    memberMemory->Count = nMembers;
    memberMemory->Names = Allocator_Calloc(&alloc, char*, nMembers);
    memberMemory->Values = Allocator_Calloc(&alloc, uint32_t, nMembers);
    
    for(uint32_t i = 0; i < nMembers; i++)
    {
        memberMemory->Names[i] = IdentifierPtrToCharPtr(&sema->SemanticIdentifierTable, enumType->Members[i].Identifier);
        memberMemory->Values[i] = (int32_t)enumType->Members[i].Value->ValueU64;
    }
    
    return memberMemory;
}



metac_compiler_t compiler = {
    0,
    0,

    compiler_PushAlloc,
    compiler_Help,

    compiler_Message,
    compiler_Error,

    compiler_GetTypeKind,
    compiler_TypeKindString,

    compiler_GetEnumMembers,

     "v0.1",
     20260205,
};

