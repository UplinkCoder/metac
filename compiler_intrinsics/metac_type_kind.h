#ifndef _METAC_TYPE_KIND_H_
#define _METAC_TYPE_KIND_H_
typedef enum metac_type_kind_t {
    TypeKind_Unknown      = 0x0,
    TypeKind_Basic        = 0x1,
    TypeKind_Enum         = 0x2,
    TypeKind_Ptr          = 0x3,
    TypeKind_Array        = 0x4,
    TypeKind_Struct       = 0x5,
    TypeKind_Union        = 0x6,
    TypeKind_Class        = 0x7,
    TypeKind_Map          = 0x8,
    TypeKind_Functiontype = 0x9,
    TypeKind_Typedef      = 0xA,
    TypeKind_Tuple        = 0xB,
    TypeKind_Template     = 0xC,
    TypeKind_Unresolved   = 0xD,
    TypeKind_Extended     = 0xE,
    TypeKind_Invalid      = 0xF,
} metac_type_kind_t;
#endif // _METAC_TYPE_KIND_H_
