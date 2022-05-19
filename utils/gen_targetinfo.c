#include "metac_target_info.h"
#include <stdio.h>

uint32_t StaticSize(metac_type_kind_t Kind)
{
    if ((Kind >= type_bool) & (Kind <= type_unsigned_long_long))
    {
        if ((Kind >= type_unsigned_char)
         &  (Kind <= type_unsigned_long))
        {
            (*(uint32_t*)&Kind) -=
                (((uint32_t)type_unsigned_char) - ((uint32_t) type_char));
        }
        else if (Kind == type_unsigned_long_long)
        {
            Kind = type_long_long;
        }

        switch((basic_type_kind_t) Kind)
        {
            case basic_bool :
                return sizeof(bool);
            case basic_char :
                return sizeof(char);
            case basic_short :
                return sizeof(short);
            case basic_int :
                return sizeof(int);
            case basic_long :
                return sizeof(long);
            case basic_float :
                return sizeof(float);
            case basic_double :
                return sizeof(double);
            case basic_long_long :
                return sizeof(long long);
        }
    }
    return -1;
}

uint32_t StaticAlign(metac_type_kind_t Kind)
{
    if ((Kind >= type_bool) & (Kind <= type_unsigned_long_long))
    {
        if ((Kind >= type_unsigned_char)
         &  (Kind <= type_unsigned_long))
        {
            (*(uint32_t*)&Kind) -=
                (((uint32_t)type_unsigned_char) - ((uint32_t) type_char));
        }
        else if (Kind == type_unsigned_long_long)
        {
            Kind = type_long_long;
        }

        switch((basic_type_kind_t) Kind)
        {
            case basic_bool :
                return _Alignof(bool);
            case basic_char :
                return _Alignof(char);
            case basic_short :
                return _Alignof(short);
            case basic_int :
                return _Alignof(int);
            case basic_long :
                return _Alignof(long);
            case basic_float :
                return _Alignof(float);
            case basic_double :
                return _Alignof(double);
            case basic_long_long :
                return _Alignof(long long);
        }
    }
    return -1;
}

int main(int argc, const char* argv[])
{
    printf("#ifndef _DEFAULT_TARGET_INFO_H_\n");
    printf("#define _DEFAULT_TARGET_INFO_H_\n");

    printf("#include \"metac_target_info.h\"\n\n");
    printf("static const metac_target_info_t default_target_info = {\n");
    printf("  {\n");
    printf("    /*TypeSizes*/\n    ");

    for(metac_type_kind_t kind = type_bool;
        kind < type_long_double;
        (*(int*)&kind)++)
    {
        printf("%u", StaticSize(kind));
        if (kind < basic_long_long)
        {
            printf(", ");
        }
    }

    printf("\n  },\n");
    printf("  {\n");
    printf("    /*TypeAligment*/\n    ");

    for(metac_type_kind_t kind = type_bool;
        kind < type_long_double;
        (*(int*)&kind)++)
    {
        printf("%u", StaticAlign(kind));
        if (kind < basic_long_long)
        {
            printf(", ");
        }
    }
    printf("\n  },\n");
    printf("  /*PtrSize*/\n");
    printf("  %u\n", (unsigned int)sizeof(void*));

    printf("};\n");
    printf("#endif // _DEFAULT_TARGET_INFO_H_\n");

    return 0;
}

