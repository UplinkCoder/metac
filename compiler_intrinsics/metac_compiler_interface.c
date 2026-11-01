#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "metac_compiler_interface.h"
#include "../semantic/metac_type.h"
#include "../semantic/metac_semantic.h"
#include "../parser/metac_identifier_table.h"
#include "metac_type_kind.c"

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

/* Appends a copy of the token, so the caller's token does not need to outlive the call.
   The buffer storage is malloc/realloc managed. */
static void compiler_InjectToken (struct metac_compiler_t* compilerP,
                                  metac_eject_buffer_handle_t handle,
                                  const struct metac_token_t* token)
{
    MetaCTokenBuffer_Append((metac_token_buffer_t*)handle, token);
}





static metac_enum_members_t* compiler_GetEnumMembers (struct metac_compiler_t* compilerP, uint32_t T)
{
    metac_sema_state_t* sema = (metac_sema_state_t*) compilerP->semanticState;
    if (!sema)
        return 0;

    metac_type_index_t typeIdx;
    typeIdx.v = T;
    metac_type_enum_t* enumType = EnumTypePtr(sema, TYPE_INDEX_INDEX(typeIdx));
    uint32_t nMembers = enumType->MemberCount;
    metac_enum_members_t* result = Allocator_Calloc(&sema->TempAlloc, metac_enum_members_t, 1);
    result->Count = nMembers;
    result->Array = Allocator_Calloc(&sema->TempAlloc, enum_member_t, nMembers);

    for(uint32_t i = 0; i < nMembers; i++)
    {
        result->Array[i].Identifier =
            IdentifierPtrToCharPtr(&sema->SemanticIdentifierTable, enumType->Members[i].Identifier);
        result->Array[i].Value = (int32_t)enumType->Members[i].Value->ValueU64;
    }

    return result;
}



/* Positional initializer, in the field order of metac_compiler_t. */
metac_compiler_t compiler = {
    0,                      /* semanticState */
    0,                      /* SetReturnAlloc */
    compiler_Help,
    compiler_Message,
    compiler_Error,
    compiler_GetTypeKind,
    compiler_TypeKindString,
    compiler_GetEnumMembers,
    compiler_InjectToken,
    "v0.1",
    20260205,
};
