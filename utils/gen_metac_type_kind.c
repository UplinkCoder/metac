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

    const char* INCLUDE_GUARD_NAME = "_METAC_TYPE_KIND_H_";
    const char* enumName = "metac_type_kind_t";
    uint32_t maxKindNameLength = 0;
    uint32_t typeKindCount = 0;
    char* indentString;
    char const * *typeKindNames;

    {
        for(type_kind_info *typeKindInfo = typeKindInfos;
            typeKindInfo->KindString; typeKindInfo++)
        {
            unsigned int kindStringLength = strlen(typeKindInfo->KindString);
            if (maxKindNameLength < kindStringLength)
            {
                maxKindNameLength = kindStringLength;
            }
            typeKindCount++;
        }

        indentString = malloc(maxKindNameLength + 1);
        memset(indentString, ' ', maxKindNameLength);
        indentString[maxKindNameLength] = '\0';
    }
    typeKindNames = malloc(sizeof(char *) * typeKindCount);

    {
#define HEADER_PATH "../compiler_intrinsics/metac_type_kind.h"
        FILE *header = fopen(HEADER_PATH ,"w+");

        fprintf(header, "#ifndef %s\n", INCLUDE_GUARD_NAME);
        fprintf(header, "#define %s\n", INCLUDE_GUARD_NAME);
        fprintf(header, "typedef enum %s {\n", enumName);

        {
            uint32_t i = 0;
            for(type_kind_info *typeKindInfo = typeKindInfos;
                typeKindInfo->KindString; typeKindInfo++)
            {
                const char* kindString = typeKindInfo->KindString;
                int kindValue = typeKindInfo->KindValue;
                unsigned int kindStringLength = strlen(kindString);
                char firstChar = (char)toupper(kindString[0]);
                uint32_t kindNameLength = snprintf(0, 0, "TypeKind_%c%s", firstChar, kindString + 1);
                char* kindNameString = (char*)(typeKindNames[i++] = malloc(kindNameLength + 1));
                sprintf(kindNameString, "TypeKind_%c%s", firstChar, kindString + 1);

                fprintf(header, "    %s %s= 0x%X,\n", kindNameString, indentString + kindStringLength, kindValue);
            }
        }

        fprintf(header, "} %s;\n", enumName);
        fprintf(header, "#endif // %s\n", INCLUDE_GUARD_NAME);
        fclose(header);
    }

    {
        FILE *code = fopen("../compiler_intrinsics/metac_type_kind.c","w+");

        fprintf(code, "#include \"" HEADER_PATH "\"\n");
        fprintf(code, "const char* MetaCTypeKind_toChars(%s value)\n", enumName);
        fprintf(code, "{\n");
        fprintf(code, "  const char* result = 0;\n");
        fprintf(code, "  switch(value) {\n");
        {
            uint32_t i = 0;
            for(i = 0; i < typeKindCount; i++)
            {
                const char* mName =  typeKindNames[i];
                fprintf(code, "    case %s: result = \"%s\"; break;\n", mName, mName);
            }
        }
        fprintf(code, "  }\n");
        fprintf(code, "  return result;\n");
        fprintf(code, "}\n");

        fclose(code);
    }
}
