#ifndef _BC_COMMON_H_
#define _BC_COMMON_H_ 1

#ifndef _MSC_VER
#  include <stdint.h>
#  include <stdbool.h>
#else
#  include "../os/compat.h"
#endif
#include <assert.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>


#ifdef __cplusplus
#  define EXTERN_C extern "C"
#else
#  define EXTERN_C extern
#endif

#undef  offsetof
#define offsetof(st, m) \
    ((size_t)((char *)&((st *)0)->m - (char *)0))

#if !defined(UINT32_MAX)
#  define UINT32_MAX ((uint32_t)0xffffffff)
#endif

static const uint32_t skipFn = UINT32_MAX;
static const uint32_t nodeFromName = UINT32_MAX - 1;
static const uint32_t currentScope = UINT32_MAX - 2;

#define CONSTEXPR

typedef struct HeapAddr
{
    uint32_t addr;
} HeapAddr;

typedef struct ExternalAddr
{
    uint32_t addr;
} ExternalAddr;

typedef struct StackAddr
{
    uint16_t addr;
} StackAddr;

typedef struct Imm32
{
    uint32_t imm32;
    bool signed_;
} Imm32;

typedef struct Imm64
{
    uint64_t imm64;
    bool signed_;
} Imm64;

typedef void (*ReadI32_cb_t)(uint32_t value, void* userCtx);

typedef struct ReadI32_ctx_t
{
    void* userCtx;
    ReadI32_cb_t cb;
} ReadI32_ctx_t;

typedef enum BCTypeEnum
{
    BCTypeEnum_Undef,

    BCTypeEnum_Null,
    BCTypeEnum_Void,

    BCTypeEnum_c8,
    BCTypeEnum_c16,
    BCTypeEnum_c32,

    /// signed integer
    BCTypeEnum_i8,
    /// DITTO
    BCTypeEnum_i16,
    /// DITTO
    BCTypeEnum_i32,
    /// DITTO
    BCTypeEnum_i64,

    BCTypeEnum_u8,
    BCTypeEnum_u16,
    BCTypeEnum_u32,
    BCTypeEnum_u64,

    BCTypeEnum_f23, /// 32  bit float mantissa has 23 bit
    BCTypeEnum_f52, /// 64  bit float mantissa has 52 bit
    BCTypeEnum_f106, /// 128 bit float mantissa has 106 bit (52+52)

    BCTypeEnum_string8,
    BCTypeEnum_string16,
    BCTypeEnum_string32,

    BCTypeEnum_Function, // synonymous to i32
    BCTypeEnum_Delegate, // synonymous to {i32, i32}

    BCTypeEnum_ValueCallback, // {void (*f)(BCValue*, void*), void*}
    BCTypeEnum_VoidCallback, // {void (*f)(void*), void*}

    //  everything below here is not used by the bc layer.
    BCTypeEnum_Enum,
    BCTypeEnum_Array,
    BCTypeEnum_Struct,
    BCTypeEnum_Ptr,

    BCTypeEnum_Tuple,
    BCTypeEnum_Class,
    BCTypeEnum_Slice,
    BCTypeEnum_AArray,
} BCTypeEnum;

EXTERN_C const char* BCTypeEnum_toChars(const BCTypeEnum* self);

typedef struct BCAddr
{
    uint32_t addr;
} BCAddr;

typedef enum BCTypeFlags
{
    BCTypeFlags_None = 0,
    BCTypeFlags_Const = 1 << 0,
} BCTypeFlags;

EXTERN_C const char* BCTypeFlags_toChars(BCTypeFlags* self);

#define STRUCT_NAME BCType
typedef struct BCType
{
    BCTypeEnum type;
    uint32_t typeIndex;

    // additional information
    BCTypeFlags flags;
} STRUCT_NAME;

EXTERN_C const char* BCType_toChars(BCType* self);

#undef STRUCT_NAME

typedef enum BCValueType
{
    BCValueType_Unknown = 0,

    BCValueType_Temporary  = 1,
    BCValueType_Parameter  = 2,
    BCValueType_Local      = 3,

    BCValueType_StackValue = 1 << 3,
    BCValueType_Immediate  = 2 << 3,
    BCValueType_HeapValue  = 3 << 3,
    BCValueType_External   = 4 << 3,
    BCValueType_ExternalFunction   = 5 << 3,
    BCValueType_RegisterValue = 6 << 3,

    BCValueType_LastCond  = 0xFB,
    BCValueType_Bailout   = 0xFC,
    BCValueType_Exception = 0xFD,
    BCValueType_ErrorWithMessage = 0xFE,
    BCValueType_Error     = 0xFF, //Pinned = 0x80,
    /// Pinned values can be returned
    /// And should be kept in the compacted heap
} BCValueType;

EXTERN_C const char* BCValueType_toChars(const BCValueType* vTypePtr);

typedef struct BCValue
{
    BCValueType vType;
    BCType type;
    const char* name;
    union
    {
        int8_t parameterIndex;
        uint16_t temporaryIndex;
        uint16_t localIndex;
        uint16_t externalIndex;
    };

    bool couldBeVoid; // = false

    union
    {
        StackAddr stackAddr;
        HeapAddr heapAddr;
        ExternalAddr externalAddr;
        Imm32 imm32;
        Imm64 imm64;
/* for now we represent floats in imm32 or imm64 respectively
        Imm23f imm23f;
        Imm52f imm52f;
*/
        // instead of void*
        void* voidStar;
    };
} BCValue;
extern BCValue BCValue_Init;

#define BCVALUE_INIT BCValue_Init;

EXTERN_C bool BCValue_eq(const BCValue* lhs, const BCValue* rhs);

#undef STRUCT_NAME

typedef struct CndJmpBegin
{
    BCAddr at;
    BCValue* cond;
    bool ifTrue;
} CndJmpBegin;

// heap address 00 - 10
// external address 10
// stack address 11


#define AddrMask  ((1U << 31) | \
                   (1U << 30))

#define stackAddrMask  ((1U << 31) | (1U << 30))
#define externalAddrMask (1U << 31 | 0U << 30)
#define heapAddrMask (0U << 31 | 0U << 30)

typedef enum address_kind_t
{
    AddressKind_Invalid,

    AddressKind_Register,
    AddressKind_Stack,
    AddressKind_External,
    AddressKind_Heap,
    AddressKind_Reserved,

    AddressKind_Max
} address_kind_t;

static inline bool isStackAddress(uint32_t unrealPointer)
{
    // a stack address has the upper 2 bits set
    return (unrealPointer & AddrMask) == stackAddrMask;
}

static inline bool isExternalAddress(uint32_t unrealPointer)
{
    // an external address has 1 upper bit set
    return (unrealPointer & AddrMask) == externalAddrMask;
}

static inline bool isHeapAddress(uint32_t unrealPointer)
{
    // a heap address does not have the upper 2 bits set
    return (unrealPointer & stackAddrMask) == heapAddrMask;
}

static inline uint32_t toStackOffset(uint32_t unrealPointer)
{
    assert(isStackAddress(unrealPointer));
    return (unrealPointer & ~stackAddrMask);
}


CONSTEXPR static inline const uint32_t align4(const uint32_t val)
{
    return ((val + 3) & ~3);
}

static inline void storeu32(uint8_t* ptr, const uint32_t v32)
{
    ptr[0] = (v32 >> 0)  & 0xFF;
    ptr[1] = (v32 >> 8)  & 0xFF;
    ptr[2] = (v32 >> 16) & 0xFF;
    ptr[3] = (v32 >> 24) & 0xFF;
}


static inline uint32_t loadu32(const uint8_t* ptr)
{
    uint32_t v32 = (ptr[0] << 0)
                 | (ptr[1] << 8)
                 | (ptr[2] << 16)
                 | (ptr[3] << 24);
    return v32;
}

/* newCTFE VM Memory Map - 2026 Edition */
typedef enum BCMemoryMap {
    NullTrapStart   = 0x00000000,
    RegisterPage    = 0x00002000, // 8KB Trap
    DoubleGuard     = 0x00003000, // 4KB Register Page
    StackStart      = 0x00004000, // 4KB Double Guard
    
    ExternalStart   = 0x02000000,
    HeapStart       = 0x42000000,
    ReservedStart   = 0xC2000000
} BCMemoryMap;


CONSTEXPR static inline uint32_t align16(const uint32_t val)
{
    return ((val + 15) & ~15);
}

EXTERN_C const uint32_t BCTypeEnum_basicTypeSize(const BCTypeEnum bct);
void* alloc_with_malloc(void* ctx, uint32_t size, void* fn);

EXTERN_C bool BCTypeEnum_anyOf(BCTypeEnum type, const BCTypeEnum acceptedTypes[], uint32_t n_types);

EXTERN_C bool BCType_isFloat(BCType bct);

EXTERN_C bool BCType_isBasicBCType(BCType bct);

EXTERN_C bool BCValue_isStackValueOrParameter(const BCValue* val);

static const int BCHeap_initHeapMax = (1 << 15);

typedef struct BCHeap
{
    uint32_t heapMax;// = initHeapMax;
    uint32_t heapSize;// = 4;
    uint8_t* heapData;
} BCHeap;

static const int heapSizeOffset = offsetof(BCHeap, heapSize);
static const int heapMaxOffset = offsetof(BCHeap, heapMax);
static const int heapDataOffset = offsetof(BCHeap, heapData);

static const int heapDataPtrOffset    = offsetof(BCHeap, heapData) + sizeof(uint8_t*);
static const int heapDataLengthOffset = offsetof(BCHeap, heapData) + sizeof(uint8_t*) + sizeof(void*);

typedef struct BCLabel
{
    BCAddr addr;
} BCLabel;


typedef struct BCLocal
{
    uint16_t idx;
    BCType type;
    StackAddr addr;
    const char* name;
} BCLocal;

typedef struct BCParameter
{
    uint8_t idx;
    BCType type;
    StackAddr pOffset;
    const char* name;
} BCParameter;

typedef struct BCStructField
{
    const char* name;
    BCType type;
    uint32_t offset;
    uint32_t size;
} BCStructField;

typedef struct BCStructType
{
    BCStructField* fields;
    const char* name;

    uint16_t nFields;
    uint16_t alignOf;
    uint32_t sizeOf;

    BCType type;
} BCStructType;

typedef struct BCTupleType
{
    BCStructField* fields;

    uint16_t nFields;
    uint16_t alignOf;
    uint32_t sizeOf;

    BCType type;
} BCTupleType;

typedef struct BCEnumMember {
    const char* name;
    BCValue value;
} BCEnumMember;

typedef struct BCEnumType
{
    BCEnumMember* members;
    const char* name;

    uint16_t nMembers;
    BCType baseType;
} BCEnumType;

typedef struct BCPointerType
{
    BCType elementType;
} BCPointerType;

typedef struct BCArrayType
{
    BCType elementType;
    uint32_t arraySize;
} BCArrayType;

typedef struct BCFunctionType
{
    BCType returnType;
    BCType* parameterTypes;
    uint32_t nParameterTypes;
} BCFunctionType;

typedef enum BCTypeInfoKind
{
    BCTypeInfoKind_Invalid,
    BCTypeInfoKind_Enum,
    BCTypeInfoKind_Ptr,
    BCTypeInfoKind_Array,
    BCTypeInfoKind_Struct,
    BCTypeInfoKind_Function,
    BCTypeInfoKind_Tuple,
} BCTypeInfoKind;

typedef struct BCTypeInfo
{
    BCTypeInfoKind kind;
    union
    {
        struct BCEnumType enumType;
        struct BCStructType structType;
        struct BCFunctionType functionType;
        struct BCTupleType tupleType;
        struct BCArrayType arrayType;
        struct BCPointerType pointerType;
    };
} BCTypeInfo;

#define imm32(VALUE) imm32_((VALUE), false)

EXTERN_C BCValue imm32_(uint32_t value, bool signed_);

#define imm64(VALUE) imm64_((VALUE), false)

EXTERN_C BCValue imm64_(uint64_t value, bool signed_);

typedef struct Imm23f
{
    float imm23f;
} Imm23f;

typedef struct Imm52f
{
    double imm52f;
} Imm52f;

/// commonType enum used for implicit conversion
static const BCTypeEnum smallIntegerTypes[] = {BCTypeEnum_u16, BCTypeEnum_u8,
                                      BCTypeEnum_i16, BCTypeEnum_i8,
                                      BCTypeEnum_c32, BCTypeEnum_c16, BCTypeEnum_c8};
#undef ARRAY_SIZE
#define ARRAY_SIZE(A) (sizeof(A) / sizeof(A[0]))

EXTERN_C BCTypeEnum BCTypeEnum_commonTypeEnum(BCTypeEnum lhs, BCTypeEnum rhs);
static inline void AllocDefaultHeap(BCHeap* newHeap);

#undef offsetof

static const BCType BCType_i32 = {BCTypeEnum_i32};
static const BCType BCType_u32 = {BCTypeEnum_u32};

#endif
