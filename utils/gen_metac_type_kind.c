#include <stdio.h>
#include <ctype.h>
#include "../semantic/metac_type.h"

#define FILLINFO_MEMBER(MEMBER_NAME, MEMBER_VALUE) \
        {#MEMBER_NAME+11, MEMBER_VALUE},

struct type_kind_info
{
    const char* KindString;
    int KindValue;
} typeKindInfos[] = { FOREACH_TYPE_INDEX_KIND(FILLINFO_MEMBER) {0,0} };
typedef struct type_kind_info type_kind_info;


int main(int argc, const char* argv[])
{
// #define FOREACH_TYPE_INDEX_KIND(M) \
//    M(type_index_unknown       , 0x0) \

    FILE *f = fopen("../compiler_intrinsics/metac_type_kind.h","w+");

    const char* INCLUDE_GUARD_NAME = "_METAC_TYPE_KIND_H_";
    const char* enumName = "metac_type_kind_t";
    uint32_t maxKindNameLength = 0;
    char* indentString;

    fprintf(f, "#ifndef %s\n", INCLUDE_GUARD_NAME);
    fprintf(f, "#define %s\n", INCLUDE_GUARD_NAME);

    fprintf(f, "typedef enum %s {\n", enumName);


    for(type_kind_info *typeKindInfo = typeKindInfos;
        typeKindInfo->KindString; typeKindInfo++)
    {
        unsigned int kindStringLength = strlen(typeKindInfo->KindString);
        if (maxKindNameLength < kindStringLength)
        {
            maxKindNameLength = kindStringLength;
        }
    }

    indentString = malloc(maxKindNameLength + 1);
    memset(indentString, ' ', maxKindNameLength);
    indentString[maxKindNameLength] = '\0';

    for(type_kind_info *typeKindInfo = typeKindInfos;
        typeKindInfo->KindString; typeKindInfo++)
    {
        const char* kindString = typeKindInfo->KindString;
        int kindValue = typeKindInfo->KindValue;
        unsigned int kindStringLength = strlen(kindString);

        char firstChar = (char)toupper(kindString[0]);
        fprintf(f, "    TypeKind_%c%s %s= 0x%X,\n", firstChar, kindString + 1, indentString + kindStringLength, kindValue);
    }

// #define PRINT_MEMBER(MEMBER_NAME, MEMBER_VALUE) \
//        fprintf(f, "    TypeKind_%c%s\t\t\t= 0x%X,\n", toupper((#MEMBER_NAME + 11)[0]), #MEMBER_NAME + 12, MEMBER_VALUE);

    fprintf(f, "} %s;\n", enumName);

    fprintf(f, "#endif // %s\n", INCLUDE_GUARD_NAME);

    fclose(f);

}


