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
            case basic_short :
                return sizeof(short);
            case basic_int :
                return sizeof(int);
            case basic_long :
                return sizeof(long);
            case basic_float :
                return sizeof(float);
            case basic_size_t :
                return sizeof(size_t);
            case basic_double :
                return sizeof(double);
            case basic_long_long :
                return sizeof(long long);
            case basic_long_double :
                return sizeof(long double);
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
           case basic_size_t :
                return _Alignof(size_t);
            case basic_float :
                return _Alignof(float);
            case basic_double :
                return _Alignof(double);
            case basic_long_long :
                return _Alignof(long long);
            case basic_long_double:
                return _Alignof(long double);
        }
    }
    return -1;
}

int main(int argc, const char* argv[])
{
/*
    printf("#ifndef _DEFAULT_TARGET_INFO_H_\n");
    printf("#define _DEFAULT_TARGET_INFO_H_\n");
    printf("#include \"../compat.h\");
    printf("#include \"../metac_target_info.h\"\n\n");
    printf("static const metac_target_info_t default_target_info =\n");
    printf("{\n");
    printf("  .SizeBool = sizeof(bool;";
    printf("  .SizeShort = ;";
    printf("  .SizeInt;
    printf("  .SizeLong;
    printf("  .SizeSizeT;
    printf("  .SizeFloat;
    printf("  .SizeDouble;
    printf("  .SizeLongLong;
    printf("  .SizeLongDouble;
    printf("\n");
    printf("  .AlignmentBool;
    printf("  .AlignmentShort;
    printf("  .AlignmentInt;
    printf("  .AlignmentLong;
    printf("  .AlignmentSizeT;
    printf("  .AlignmentFloat;
    printf("  .AlignmentDouble;
    printf("  .AlignmentLongLong;
    printf("  .AlignmentLongDouble;
    printf("\n");
    printf("PtrSize;
    printf("AlignmentChar;
    printf("};\n");
    printf("#endif // _DEFAULT_TARGET_INFO_H_\n");
*/
    return 0;
}

