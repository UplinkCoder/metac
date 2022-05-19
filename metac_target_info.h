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
    uint8_t SizeBool;
    uint8_t SizeShort;
    uint8_t SizeInt;
    uint8_t SizeLong;
    uint8_t SizeSizeT;
    uint8_t SizeFloat;
    uint8_t SizeDouble;
    uint8_t SizeLongLong;
    uint8_t SizeLongDouble;

    uint8_t AlignmentBool;
    uint8_t AlignmentShort;
    uint8_t AlignmentInt;
    uint8_t AlignmentLong;
    uint8_t AlignmentSizeT;
    uint8_t AlignmentFloat;
    uint8_t AlignmentDouble;
    uint8_t AlignmentLongLong;
    uint8_t AlignmentLongDouble;
    
    uint8_t PtrSize;
    uint8_t AlignmentChar;
} metac_target_info_t;


uint32_t MetaCTargetInfo_GetBasicSize(const metac_target_info_t* targetInfo,
                                      basic_type_kind_t Kind);

uint32_t MetaCTargetInfo_GetBasicAlign(const metac_target_info_t* targetInfo,
                                       basic_type_kind_t Kind);
#endif