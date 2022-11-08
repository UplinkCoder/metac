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

    BCValueType_LastCond  = 0xFB,
    BCValueType_Bailout   = 0xFC,
    BCValueType_Exception = 0xFD,
    BCValueType_ErrorWithMessage = 0xFE,
    BCValueType_Error     = 0xFF, //Pinned = 0x80,
    /// Pinned values can be returned
    /// And should be kept in the compacted heap
} BCValueType;

EXTERN_C const char* BCValueType_toChars(const BCValueType* vTypePtr);

typedef struct BCHeapRef
{
#define STRUCT_NAME BCHeapRef
    BCValueType vType;
    union
    {
        uint16_t tmpIndex;
        uint16_t localIndex;
        uint8_t paramIndex;
    };

    union
    {
        HeapAddr heapAddr;
        StackAddr stackAddr;
        ExternalAddr externalAddr;
        Imm32 imm32;
    };

    const char* name;
#if 0
    operator bool();

    STRUCT_NAME(const BCValue that);
#endif
} STRUCT_NAME;
#undef STRUCT_NAME

typedef struct BCValue
{
#define STRUCT_NAME BCValue
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

    BCHeapRef heapRef;
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
    //TODO PERF minor: use a 32bit value for heapRef;

#if 0
    uint32_t toUint();

    const char* toChars();

    const char* valueToString();

    bool operator bool();

    bool operator == (const BCValue* rhs);

    STRUCT_NAME(const Imm32 imm32);

    STRUCT_NAME(const Imm64 imm64);

    STRUCT_NAME(const Imm23f imm23f);

    STRUCT_NAME(const Imm52f imm52f);

    STRUCT_NAME(const BCParameter param);

    STRUCT_NAME(const StackAddr sp, const BCType type, const ushort tmpIndex = 0);

    STRUCT_NAME(const StackAddr sp, const BCType type, const ushort localIndex, const char* name);

    STRUCT_NAME(const void* base, const short addr, const BCType type);

    STRUCT_NAME(const HeapAddr addr, const BCType type = i32Type);

    STRUCT_NAME(const BCHeapRef heapRef);
#endif
} STRUCT_NAME;
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


#define AddrMask  ((1 << 31) | \
                   (1 << 30))

#define stackAddrMask  ((1 << 31) | (1 << 30))
#define externalAddrMask (1 << 31 | 0 << 30)
#define heapAddrMask (0 << 31 | 0 << 30)

typedef enum address_kind_t
{
    AddressKind_Invalid,

    AddressKind_Frame,
    AddressKind_Heap,
    AddressKind_External,

    AddressKind_Max
} address_kind_t;

static inline address_kind_t ClassifyAddress(uint32_t unrealPointer)
{
    address_kind_t result = AddressKind_Invalid;

    switch ((unrealPointer & AddrMask))
    {
        case stackAddrMask:
            result = AddressKind_Frame;
        break;
        case externalAddrMask:
            result = AddressKind_External;
        break;
        case heapAddrMask:
            result = AddressKind_Heap;
        break;
    }

    return result;
}


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


CONSTEXPR static inline uint32_t align16(const uint32_t val)
{
    return ((val + 15) & ~15);
}

EXTERN_C const uint32_t BCTypeEnum_basicTypeSize(const BCTypeEnum bct);
void* alloc_with_malloc(void* ctx, uint32_t size, void* fn);
/*
static inline const uint32_t fastLog10(const uint32_t val)
{
    return (val < 10) ? 0 : (val < 100) ? 1 : (val < 1000) ? 2 : (val < 10000) ? 3
        : (val < 100000) ? 4 : (val < 1000000) ? 5 : (val < 10000000) ? 6
        : (val < 100000000) ? 7 : (val < 1000000000) ? 8 : 9;
}
*/
/*@unique*/
/*
static const fastPow10tbl = [
    1, 10, 100, 1000, 10000, 100000, 1000000, 10000000, 100000000, 1000000000,
];

const char* itos(const uint val)
{
    char* result = new char[](length);
    immutable length = fastLog10(val) + 1;

    foreach (i; 0 .. length)
    {
        immutable _val = val / fastPow10tbl[i];
        result[length - i - 1] = cast(char)((_val % 10) + '0');
    }

    return result;
}

string itos64(const uint64_t val)
{
    if (val <= UINT32_MAX)
        return itos(val & UINT32_MAX);

    uint lw = val & UINT32_MAX;
    uint hi = val >> 32;

    auto lwString = itos(lw);
    auto hiString = itos(hi);

    return cast(string) "((" ~ hiString ~ "UL << 32)" ~ "|" ~ lwString ~ ")";
}

string sitos(const int val)
{
    int sign = (val < 0) ? 1 : 0;
    uint abs_val = (val < 0) ? -val : val;

    immutable length = fastLog10(abs_val) + 1;
    char[] result;
    result.length = length + sign;

    foreach (i; 0 .. length)
    {
        immutable _val = abs_val / fastPow10tbl[i];
        result[length - i - !sign] = cast(char)((_val % 10) + '0');
    }

    if (sign)
    {
        result[0] = '-';
    }

    return cast(string) result;
}

string floatToString(float f)
{
    return fpconv_dtoa(f) ~ "f";
}

string doubleToString(double d)
{
   return fpconv_dtoa(d);
}
*/

EXTERN_C bool BCTypeEnum_anyOf(BCTypeEnum type, const BCTypeEnum acceptedTypes[], uint32_t n_types);

EXTERN_C bool BCType_isFloat(BCType bct);

EXTERN_C bool BCType_isBasicBCType(BCType bct);

EXTERN_C bool BCValue_isStackValueOrParameter(const BCValue* val);

EXTERN_C BCValue BCValue_fromHeapref(const BCHeapRef heapRef);

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
    BCTypeEnum baseType;
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
    BCTypeInfofKind_Invalid,
    BCTypeInfofKind_Enum,
    BCTypeInfofKind_Ptr,
    BCTypeInfofKind_Array,
    BCTypeInfofKind_Struct,
    BCTypeInfofKind_Function,
    BCTypeInfofKind_Tuple,
} BCTypeInfoKind;

typedef struct BCTypeInfo
{
    BCTypeEnum kind;
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

typedef struct BCBlock
{
    BCLabel begin;
    BCLabel end;
} BCBlock;

typedef struct BCBranch
{
    BCLabel ifTrue;
    BCLabel ifFalse;
} BCBranch;

/*
template ensureIsBCGen(BCGenT)
{
    static assert(is(typeof(BCGenT.beginFunction(uint.init)) == void),
            BCGenT.stringof ~ " is missing void beginFunction(uint)");
    static assert(is(typeof(BCGenT.endFunction())), BCGenT.stringof ~ " is missing endFunction()");
    static assert(is(typeof(BCGenT.Initialize()) == void),
            BCGenT.stringof ~ " is missing void Initialize()");
    static assert(is(typeof(BCGenT.Finalize()) == void),
            BCGenT.stringof ~ " is missing void Finalize()");
    static assert(is(typeof(BCGenT.genTemporary(BCType.init)) == BCValue),
            BCGenT.stringof ~ " is missing BCValue genTemporary(BCType bct)");
    static assert(is(typeof(BCGenT.genParameter(BCType.init, string.init)) == BCValue),
            BCGenT.stringof ~ " is missing BCValue genParameter(BCType bct, string name)");
    static assert(is(typeof(BCGenT.beginJmp()) == BCAddr),
            BCGenT.stringof ~ " is missing BCAddr beginJmp()");
    static assert(is(typeof(BCGenT.endJmp(BCAddr.init, BCLabel.init)) == void),
            BCGenT.stringof ~ " is missing void endJmp(BCAddr atIp, BCLabel target)");
    static assert(is(typeof(BCGenT.incSp()) == void), BCGenT.stringof ~ " is missing void incSp()");
    static assert(is(typeof(BCGenT.currSp()) == StackAddr),
            BCGenT.stringof ~ " is missing StackAddr currSp()");
    static assert(is(typeof(BCGenT.genLabel()) == BCLabel),
            BCGenT.stringof ~ " is missing BCLabel genLabel()");
    static assert(is(typeof(BCGenT.beginCndJmp(BCValue.init, bool.init)) == CndJmpBegin),
            BCGenT.stringof
            ~ " is missing CndJmpBegin beginCndJmp(BCValue cond = BCValue.init, bool ifTrue = false)");
    static assert(is(typeof(BCGenT.endCndJmp(CndJmpBegin.init, BCLabel.init)) == void),
            BCGenT.stringof ~ " is missing void endCndJmp(CndJmpBegin jmp, BCLabel target)");
    static assert(is(typeof(BCGenT.Jmp(BCLabel.init)) == void),
            BCGenT.stringof ~ " is missing void Jmp(BCLabel target)");
    static assert(is(typeof(BCGenT.emitFlg(BCValue.init)) == void),
            BCGenT.stringof ~ " is missing void emitFlg(BCValue lhs)");
    static assert(is(typeof(BCGenT.Alloc(BCValue.init, BCValue.init)) == void),
            BCGenT.stringof ~ " is missing void Alloc(BCValue heapPtr, BCValue size)");
    static assert(is(typeof(BCGenT.Assert(BCValue.init, BCValue.init)) == void),
            BCGenT.stringof ~ " is missing void Assert(BCValue value, BCValue message)");
    static assert(is(typeof(BCGenT.Not(BCValue.init, BCValue.init)) == void),
            BCGenT.stringof ~ " is missing void Not(BCValue result, BCValue val)");
    static assert(is(typeof(BCGenT.Set(BCValue.init, BCValue.init)) == void),
            BCGenT.stringof ~ " is missing void Set(BCValue lhs, BCValue rhs)");

    static assert(is(typeof(BCGenT.Ult3(BCValue.init, BCValue.init, BCValue.init)) == void),
            BCGenT.stringof ~ " is missing void Ult3(BCValue result, BCValue lhs, BCValue rhs)");
    static assert(is(typeof(BCGenT.Ugt3(BCValue.init, BCValue.init, BCValue.init)) == void),
            BCGenT.stringof ~ " is missing void Ugt3(BCValue result, BCValue lhs, BCValue rhs)");

    static assert(is(typeof(BCGenT.Lt3(BCValue.init, BCValue.init, BCValue.init)) == void),
            BCGenT.stringof ~ " is missing void Lt3(BCValue result, BCValue lhs, BCValue rhs)");
    static assert(is(typeof(BCGenT.Gt3(BCValue.init, BCValue.init, BCValue.init)) == void),
            BCGenT.stringof ~ " is missing void Gt3(BCValue result, BCValue lhs, BCValue rhs)");

    static assert(is(typeof(BCGenT.Le3(BCValue.init, BCValue.init, BCValue.init)) == void),
            BCGenT.stringof ~ " is missing void Le3(BCValue result, BCValue lhs, BCValue rhs)");
    static assert(is(typeof(BCGenT.Ge3(BCValue.init, BCValue.init, BCValue.init)) == void),
            BCGenT.stringof ~ " is missing void Ge3(BCValue result, BCValue lhs, BCValue rhs)");

    static assert(is(typeof(BCGenT.Ule3(BCValue.init, BCValue.init, BCValue.init)) == void),
            BCGenT.stringof ~ " is missing void Ule3(BCValue result, BCValue lhs, BCValue rhs)");
    static assert(is(typeof(BCGenT.Uge3(BCValue.init, BCValue.init, BCValue.init)) == void),
            BCGenT.stringof ~ " is missing void Uge3(BCValue result, BCValue lhs, BCValue rhs)");

    static assert(is(typeof(BCGenT.Neq3(BCValue.init, BCValue.init, BCValue.init)) == void),
            BCGenT.stringof ~ " is missing void Neq3(BCValue result, BCValue lhs, BCValue rhs)");
    static assert(is(typeof(BCGenT.Add3(BCValue.init, BCValue.init, BCValue.init)) == void),
            BCGenT.stringof ~ " is missing void Add3(BCValue result, BCValue lhs, BCValue rhs)");
    static assert(is(typeof(BCGenT.Sub3(BCValue.init, BCValue.init, BCValue.init)) == void),
            BCGenT.stringof ~ " is missing void Sub3(BCValue result, BCValue lhs, BCValue rhs)");
    static assert(is(typeof(BCGenT.Mul3(BCValue.init, BCValue.init, BCValue.init)) == void),
            BCGenT.stringof ~ " is missing void Mul3(BCValue result, BCValue lhs, BCValue rhs)");
    static assert(is(typeof(BCGenT.Div3(BCValue.init, BCValue.init, BCValue.init)) == void),
            BCGenT.stringof ~ " is missing void Div3(BCValue result, BCValue lhs, BCValue rhs)");
    static assert(is(typeof(BCGenT.Udiv3(BCValue.init, BCValue.init, BCValue.init)) == void),
            BCGenT.stringof ~ " is missing void Udiv3(BCValue result, BCValue lhs, BCValue rhs)");
    static assert(is(typeof(BCGenT.And3(BCValue.init, BCValue.init, BCValue.init)) == void),
            BCGenT.stringof ~ " is missing void And3(BCValue result, BCValue lhs, BCValue rhs)");
    static assert(is(typeof(BCGenT.Or3(BCValue.init, BCValue.init, BCValue.init)) == void),
            BCGenT.stringof ~ " is missing void Or3(BCValue result, BCValue lhs, BCValue rhs)");
    static assert(is(typeof(BCGenT.Xor3(BCValue.init, BCValue.init, BCValue.init)) == void),
            BCGenT.stringof ~ " is missing void Xor3(BCValue result, BCValue lhs, BCValue rhs)");
    static assert(is(typeof(BCGenT.Lsh3(BCValue.init, BCValue.init, BCValue.init)) == void),
            BCGenT.stringof ~ " is missing void Lsh3(BCValue result, BCValue lhs, BCValue rhs)");
    static assert(is(typeof(BCGenT.Rsh3(BCValue.init, BCValue.init, BCValue.init)) == void),
            BCGenT.stringof ~ " is missing void Rsh3(BCValue result, BCValue lhs, BCValue rhs)");
    static assert(is(typeof(BCGenT.Mod3(BCValue.init, BCValue.init, BCValue.init)) == void),
            BCGenT.stringof ~ " is missing void Mod3(BCValue result, BCValue lhs, BCValue rhs)");
    static assert(is(typeof(BCGenT.Umod3(BCValue.init, BCValue.init, BCValue.init)) == void),
            BCGenT.stringof ~ " is missing void Umod3(BCValue result, BCValue lhs, BCValue rhs)");
    static assert(is(typeof(BCGenT.Call(BCValue.init, BCValue.init,
            BCValue[].init)) == void),
            BCGenT.stringof ~ " is missing void Call(BCValue result, BCValue fn, BCValue[] args)");

    static assert(is(typeof(BCGenT.Load8(BCValue.init, BCValue.init)) == void),
            BCGenT.stringof ~ " is missing void Load8(BCValue _to, BCValue from)");
    static assert(is(typeof(BCGenT.Store8(BCValue.init, BCValue.init)) == void),
            BCGenT.stringof ~ " is missing void Store8(BCValue _to, BCValue value)");

    static assert(is(typeof(BCGenT.Load16(BCValue.init, BCValue.init)) == void),
            BCGenT.stringof ~ " is missing void Load162(BCValue _to, BCValue from)");
    static assert(is(typeof(BCGenT.Store16(BCValue.init, BCValue.init)) == void),
            BCGenT.stringof ~ " is missing void Store16(BCValue _to, BCValue value)");

    static assert(is(typeof(BCGenT.Load32(BCValue.init, BCValue.init)) == void),
            BCGenT.stringof ~ " is missing void Load32(BCValue _to, BCValue from)");
    static assert(is(typeof(BCGenT.Store32(BCValue.init, BCValue.init)) == void),
            BCGenT.stringof ~ " is missing void Store32(BCValue _to, BCValue value)");

    static assert(is(typeof(BCGenT.Load64(BCValue.init, BCValue.init)) == void),
        BCGenT.stringof ~ " is missing void Load64(BCValue _to, BCValue from)");
    static assert(is(typeof(BCGenT.Store64(BCValue.init, BCValue.init)) == void),
        BCGenT.stringof ~ " is missing void Store64(BCValue _to, BCValue value)");

    static assert(is(typeof(BCGenT.Ret(BCValue.init)) == void),
            BCGenT.stringof ~ " is missing void Ret(BCValue val)");
    static assert(is(typeof(BCGenT.insideFunction) == bool),
        BCGenT.stringof ~ " is missing bool insideFunction");

    enum ensureIsBCGen = true;
}
*/
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
