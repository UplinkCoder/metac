// #include "../compiler_intrinsics/metac_type_kind.h"

const char* MetaCTypeKind_toChars(metac_type_kind_t value)
{
  const char* result = 0;
  switch(value) {
    case TypeKind_Unknown: result = "TypeKind_Unknown"; break;
    case TypeKind_Basic: result = "TypeKind_Basic"; break;
    case TypeKind_Enum: result = "TypeKind_Enum"; break;
    case TypeKind_Ptr: result = "TypeKind_Ptr"; break;
    case TypeKind_Array: result = "TypeKind_Array"; break;
    case TypeKind_Struct: result = "TypeKind_Struct"; break;
    case TypeKind_Union: result = "TypeKind_Union"; break;
    case TypeKind_Class: result = "TypeKind_Class"; break;
    case TypeKind_Map: result = "TypeKind_Map"; break;
    case TypeKind_Functiontype: result = "TypeKind_Functiontype"; break;
    case TypeKind_Typedef: result = "TypeKind_Typedef"; break;
    case TypeKind_Tuple: result = "TypeKind_Tuple"; break;
    case TypeKind_Template: result = "TypeKind_Template"; break;
    case TypeKind_Unresolved: result = "TypeKind_Unresolved"; break;
    case TypeKind_Extended: result = "TypeKind_Extended"; break;
    case TypeKind_Invalid: result = "TypeKind_Invalid"; break;
  }
  return result;
}

