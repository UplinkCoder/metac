#ifndef _METAC_DEFAULT_TARGET_INFO_H_
#define _METAC_DEFAULT_TARGET_INFO_H_

#include "compat.h"

static const metac_target_info_t default_target_info =
{
      .SizeBool = sizeof(bool),
      .SizeShort = sizeof(short),
      .SizeInt = sizeof(int),
      .SizeLong = sizeof(long),
      .SizeSizeT = sizeof(size_t),
      .SizeFloat = sizeof(float),
      .SizeDouble = sizeof(double),
      .SizeLongLong = sizeof(long long),
      .SizeLongDouble = sizeof(long double),

#ifdef MSVC_VER_
#define _Alignof(X) \
    __alignof(X)
#endif

      .AlignmentBool = _Alignof(bool),
      .AlignmentShort = _Alignof(short),
      .AlignmentInt = _Alignof(int),
      .AlignmentLong = _Alignof(long),
      .AlignmentSizeT = _Alignof(size_t),
      .AlignmentFloat = _Alignof(float),
      .AlignmentDouble = _Alignof(double),
      .AlignmentLongLong = _Alignof(long long),
      .AlignmentLongDouble = _Alignof(long double),

      .PtrSize = sizeof(void*),
      .AlignmentChar = _Alignof(char)
 };
#endif //_METAC_DEFAULT_TARGET_INFO_H_

