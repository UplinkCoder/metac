#include "metac_target_info.h"

uint32_t MetaCTargetInfo_GetBasicSize(const metac_target_info_t* targetInfo,
                                      basic_type_kind_t Kind)
{
    uint32_t result = -1;

    switch(Kind)
    {
        case basic_bool:
            result = targetInfo->SizeBool;
        break;
        case basic_char:
            result = 1;
        break;
        case basic_short:
            result = targetInfo->SizeShort;
        break;
        case basic_int:
            result = targetInfo->SizeInt;
        break;
        case basic_long:
            result = targetInfo->SizeLong;
        break;
        case basic_long_long:
            result = targetInfo->SizeLongLong;
        break;
        case basic_size_t:
            result = targetInfo->SizeSizeT;
        break;
        case basic_float:
            result = targetInfo->SizeFloat;
        break;
        case basic_double:
            result = targetInfo->SizeDouble;
        break;
        case basic_long_double:
            result = targetInfo->SizeLongDouble;
        break;
    }
 
    return result;
}

uint32_t MetaCTargetInfo_GetBasicAlign(const metac_target_info_t* targetInfo,
                                       basic_type_kind_t Kind)
{
    uint32_t result = -1;

    switch(Kind)
    {
        case basic_bool:
            result = targetInfo->AlignmentBool;
        case basic_char:
            result = targetInfo->AlignmentChar;
        case basic_short:
            result = targetInfo->AlignmentShort;
        case basic_int:
            result = targetInfo->AlignmentInt;
        case basic_long:
            result = targetInfo->AlignmentLong;
        case basic_long_long:
            result = targetInfo->AlignmentLongLong;
        case basic_size_t:
            result = targetInfo->AlignmentSizeT;
        case basic_float:
            result = targetInfo->AlignmentFloat;
        case basic_double:
            result = targetInfo->AlignmentDouble;
        case basic_long_double:
            result = targetInfo->AlignmentLongDouble;                
    }
    
    return result;
}
