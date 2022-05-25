#ifndef _METAC_TARGET_INFO_H_
#define _METAC_TARGET_INFO_H_
#include "metac_type.h"
#include <assert.h>

typedef enum basic_type_kind_t
{
// DoNT CHANGE THE ORDER FROM HERE
// XXX: Order needs to be in sync with the metac_type_kind_t order in metac_type.h
    basic_bool = type_bool,
    basic_char,
    basic_short,
    basic_int,
    basic_long,
    basic_size_t,

    basic_float,
    basic_double,

    basic_long_long,
    basic_long_double
} basic_type_kind_t;

typedef struct metac_target_info_t
{
    const uint8_t SizeBool;
    const uint8_t SizeShort;
    const uint8_t SizeInt;
    const uint8_t SizeLong;
    const uint8_t SizeSizeT;
    const uint8_t SizeFloat;
    const uint8_t SizeDouble;
    const uint8_t SizeLongLong;
    const uint8_t SizeLongDouble;

    const uint8_t AlignmentBool;
    const uint8_t AlignmentShort;
    const uint8_t AlignmentInt;
    const uint8_t AlignmentLong;
    const uint8_t AlignmentSizeT;
    const uint8_t AlignmentFloat;
    const uint8_t AlignmentDouble;
    const uint8_t AlignmentLongLong;
    const uint8_t AlignmentLongDouble;
    
    const uint8_t PtrSize;
    const uint8_t AlignmentChar;
} metac_target_info_t;


uint32_t MetaCTargetInfo_GetBasicSize(const metac_target_info_t* targetInfo,
                                      basic_type_kind_t Kind);

uint32_t MetaCTargetInfo_GetBasicAlign(const metac_target_info_t* targetInfo,
                                       basic_type_kind_t Kind);
#endif