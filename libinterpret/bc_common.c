#ifndef _BC_COMMON_C_
#define _BC_COMMON_C_

#ifndef _WIN32
#  include <stdint.h>
#  include <stdbool.h>
#endif

#include <assert.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "bc_common.h"
#include "backend_interface_funcs.h"

#ifdef __cplusplus
#define EXTERN_C extern "C"
#else
#define EXTERN_C extern
#endif

#undef offsetof

#define offsetof(st, m) \
    ((size_t)((char *)&((st *)0)->m - (char *)0))

#define CONSTEXPR
BCValue BCValue_Init = { BCValueType_Unknown };

const char*  BCTypeEnum_toChars(const BCTypeEnum* self)
{
    switch(*self)
    {
    case BCTypeEnum_Undef:
        return "BCTypeEnum_Undef";

    case BCTypeEnum_Null:
        return "BCTypeEnum_Null";

    case BCTypeEnum_Void:
        return "BCTypeEnum_Void";


    case BCTypeEnum_c8:
        return "BCTypeEnum_c8";

    case BCTypeEnum_c16:
        return "BCTypeEnum_c16";

    case BCTypeEnum_c32:
        return "BCTypeEnum_c32";


    case BCTypeEnum_i8:
        return "BCTypeEnum_i8";

    case BCTypeEnum_i16:
        return "BCTypeEnum_i16";

    case BCTypeEnum_i32:
        return "BCTypeEnum_i32";

    case BCTypeEnum_i64:
        return "BCTypeEnum_i64";


    case BCTypeEnum_u8:
        return "BCTypeEnum_u8";

    case BCTypeEnum_u16:
        return "BCTypeEnum_u16";

    case BCTypeEnum_u32:
        return "BCTypeEnum_u32";

    case BCTypeEnum_u64:
        return "BCTypeEnum_u64";


    case BCTypeEnum_f23:
        return "BCTypeEnum_f23";

    case BCTypeEnum_f52:
        return "BCTypeEnum_f52";


    case BCTypeEnum_f106 :
        return "BCTypeEnum_f106";

    case BCTypeEnum_string8:
        return "BCTypeEnum_string8";

    case BCTypeEnum_string16:
        return "BCTypeEnum_string16";

    case BCTypeEnum_string32:
        return "BCTypeEnum_string32";

    case BCTypeEnum_Function:
        return "BCTypeEnum_Function";

    case BCTypeEnum_Delegate:
        return "BCTypeEnum_Delegate";

    case BCTypeEnum_Array:
        return "BCTypeEnum_Array";

    case BCTypeEnum_AArray:
        return "BCTypeEnum_AArray";

    case BCTypeEnum_Struct:
        return "BCTypeEnum_Struct";

    case BCTypeEnum_Class:
        return "BCTypeEnum_Class";

    case BCTypeEnum_Ptr:
        return "BCTypeEnum_Ptr";

    case BCTypeEnum_Slice:
        return "BCTypeEnum_Slice";

    case BCTypeEnum_Tuple:
        return "BCTypeEnum_Tuple";

    case BCTypeEnum_Enum:
        return "BCTypeEnum_Enum";
    }

    assert(0);
    return 0;
}

const char* BCTypeFlags_toChars(BCTypeFlags* self)
{
    if (*self == 0)
        return "None";

    if ((*self & (1 << 0)) != 0)
        return "Const";

    assert(0);
    return 0;
}

const char* BCType_toChars(BCType* self)
{
    char format_buffer[1024];

    int sz = sprintf(format_buffer, "BCType\n\t{type: %s, typeIndex: %u, flags: %s}\n"
            , BCTypeEnum_toChars(&self->type)
            , self->typeIndex
            , BCTypeFlags_toChars(&self->flags));

    assert(sz < 1024);

    char* result = (char*) malloc (sz);
    memcpy(result, format_buffer, sz);

    return result;
}
#undef STRUCT_NAME

const char* BCValueType_toChars(const BCValueType* vTypePtr)
{
    const BCValueType vType = *vTypePtr;
    const char* result = 0;

    switch(vType)
    {
        case BCValueType_Unknown:
            result = "BCValueType_Unknown";
        break;
        case BCValueType_External:
            result = "BCValueType_External";
        break;
        case BCValueType_Temporary:
            result = "BCValueType_Temporary";
        break;
        case BCValueType_Parameter:
            result = "BCValueType_Parameter";
        break;
        case BCValueType_Local:
            result = "BCValueType_Local";
        break;
        case BCValueType_StackValue:
            result = "BCValueType_StackValue";
        break;
        case BCValueType_Immediate:
            result = "BCValueType_Immediate";
        break;
        case BCValueType_HeapValue:
            result = "BCValueType_HeapValue";
        break;
        case BCValueType_LastCond:
            result = "BCValueType_LastCond";
        break;
        case BCValueType_Bailout:
            result = "BCValueType_Bailout";
        break;
        case BCValueType_Exception:
            result = "BCValueType_Exception";
        break;
        case BCValueType_ErrorWithMessage:
            result = "BCValueType_ErrorWithMessage";
        break;
        case BCValueType_Error:
            result = "BCValueType_Error";
        break;
    }

    return result;
}

#define STRUCT_NAME BCValue
#if 0
    operator BCValue::bool()
    {
        // the check for Undef is a workaround
        // consider removing it when everything works correctly.

        return this.vType != vType.Unknown;
    }

    BCValue::STRUCT_NAME(const BCValue that)
    {
        switch (that.vType)
        {
        case BCValueType_StackValue, BCValueType_Parameter:
        case BCValueType_Temporary:
            stackAddr = that.stackAddr;
            temporaryIndex = that.temporaryIndex;
            break;

        case BCValueType_Local:
            stackAddr = that.stackAddr;
            localIndex = that.localIndex;
            this.name = that.name;
            break;

        case BCValueType_HeapValue:
            heapAddr = that.heapAddr;
            break;

        case BCValueType_Immediate:
            imm32 = that.imm32;
            break;

        default:
            printf("vType unsupported: %s\n", BCValueType_toChars(that.vType));
            assert(0);
        }
        vType = that.vType;
    }
#endif
#undef STRUCT_NAME

static inline uint32_t BCValue_toU32(const BCValue* self)
{
    uint32_t result;

    switch (self->vType)
    {
        case BCValueType_Parameter :
        case BCValueType_Temporary :
        case BCValueType_StackValue :
        {
            result = self->stackAddr.addr;
        } break;
        case BCValueType_HeapValue :
        {
           result = self->heapAddr.addr;
        } break;
        case BCValueType_Immediate :
        {
            result =  self->imm32.imm32;
        } break;
        default:
        {
            // printf("toUint not implemented for %s\n", BCValueType_toChars(vType))
            assert(0);
        }
    }

    return result;
}

EXTERN_C bool BCValue_eq(const BCValue* lhs, const BCValue* rhs)
{
    BCTypeEnum commonType = BCTypeEnum_commonTypeEnum(lhs->type.type, rhs->type.type);

    BCValueType lhs_vType = lhs->vType;
    if (lhs_vType == rhs->vType)
    {
        switch (lhs_vType)
        {
        case BCValueType_StackValue:
        case BCValueType_Parameter:
        case BCValueType_Local:
                return lhs->stackAddr.addr == rhs->stackAddr.addr;

        case BCValueType_Temporary:
            return lhs->temporaryIndex == rhs->temporaryIndex;
        case BCValueType_Immediate:
            switch (commonType)
            {
            case BCTypeEnum_i32:
            case BCTypeEnum_u32:
                {
                    return lhs->imm32.imm32 == rhs->imm32.imm32;
                }
            case BCTypeEnum_i64:
            case BCTypeEnum_u64:
                {
                    return lhs->imm64.imm64 == rhs->imm64.imm64;
                }

            default:
                assert(0); //, "No comperasion for immediate");
            }
        case BCValueType_HeapValue:
            return lhs->heapAddr.addr == rhs->heapAddr.addr;

        case BCValueType_Unknown:
        case BCValueType_Bailout:
            return false;
        case BCValueType_Error:
        case BCValueType_ErrorWithMessage:
        case BCValueType_Exception:
            return false;
        case BCValueType_LastCond:
            return true;
        }

    }

    return false;
}

#undef STRUCT_NAME

const uint32_t BCTypeEnum_basicTypeSize(const BCTypeEnum bct)
{
    uint32_t result = 4;

    switch (bct)
    {
    case BCTypeEnum_Undef:
        {
            assert(!"This is not supposed to happen");
        }
    case BCTypeEnum_c8:
    case BCTypeEnum_i8:
    case BCTypeEnum_u8:
        {
            result = 1;
        } break;
    case BCTypeEnum_c16:
    case BCTypeEnum_i16:
    case BCTypeEnum_u16:
        {
            result =  2;
        } break;
    case BCTypeEnum_c32:
    case BCTypeEnum_i32:
    case BCTypeEnum_u32:
    case BCTypeEnum_f23:
        {
        } break;
    case BCTypeEnum_i64:
    case BCTypeEnum_u64:
    case BCTypeEnum_f52:
        {
            result = 8;
        } break;
    case BCTypeEnum_f106:
        {
            result = 16;
        } break;

    case BCTypeEnum_Function:
    case BCTypeEnum_Null:
        {
        } break;
    case BCTypeEnum_Delegate:
        {
            result = 8;
        } break;
    case BCTypeEnum_Ptr:
        {
            assert(!"Ptr is not supposed to be a basicType anymore");
        }

    case BCTypeEnum_string8:
    case BCTypeEnum_string16:
    case BCTypeEnum_string32:
        {
            //FIXME actually strings don't have a basicTypeSize as is
            result = 16;
        } break;
    case BCTypeEnum_Void:
    case BCTypeEnum_Array:
    case BCTypeEnum_Slice:
    case BCTypeEnum_Struct:
    case BCTypeEnum_Class:
    case BCTypeEnum_AArray:
    case BCTypeEnum_Tuple:
        {
            result = 0;
        } break;

    default : assert(0);
    }

    return result;
}

static inline const uint32_t adjustmentMask(BCTypeEnum t)
{
    uint32_t mask = 0;
    int typeSize = BCTypeEnum_basicTypeSize(t);

    if (typeSize == 1)
        mask = 0xFF;
    else if (typeSize == 2)
        mask = 0xFFFF;
    else if (typeSize == 4)
        mask = 0xFFFFFFFF;

    return mask;
}

/*
const uint32_t fastLog10(const uint32_t val)
{
    return (val < 10) ? 0 : (val < 100) ? 1 : (val < 1000) ? 2 : (val < 10000) ? 3
        : (val < 100000) ? 4 : (val < 1000000) ? 5 : (val < 10000000) ? 6
        : (val < 100000000) ? 7 : (val < 1000000000) ? 8 : 9;
}

// @unique
static immutable fastPow10tbl = [
    1, 10, 100, 1000, 10000, 100000, 1000000, 10000000, 100000000, 1000000000,
];

const char* itos(const uint val) pure @trusted nothrow
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

string sitos(const int val) pure @trusted nothrow
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

bool BCTypeEnum_anyOf(BCTypeEnum type, const BCTypeEnum acceptedTypes[], uint32_t n_types)
{
    bool result = false;

    for(uint32_t i = 0; i < n_types; i++)
    {
        if (type == acceptedTypes[i])
        {
            result = true;
            break;
        }
    }

    return result;
}

bool isFloat(BCType bct)
{
    return bct.type == BCTypeEnum_f23 || bct.type == BCTypeEnum_f52;
}

EXTERN_C bool BCType_isBasicBCType(BCType bct)
{
    return !(bct.type == BCTypeEnum_Struct || bct.type == BCTypeEnum_Array || bct.type == BCTypeEnum_Class
            || bct.type == BCTypeEnum_Slice || bct.type == BCTypeEnum_Undef || bct.type == BCTypeEnum_Ptr
            || bct.type == BCTypeEnum_AArray || bct.type == BCTypeEnum_Tuple);
}

EXTERN_C bool BCValue_isStackValueOrParameter(const BCValue* val)
{
    return (val->vType == BCValueType_StackValue
         || val->vType == BCValueType_Parameter
         || val->vType == BCValueType_Local
         || val->vType == BCValueType_Temporary
         || val->vType == BCValueType_External);
}
/*
/*
+ ADDRESS RANGE           | SIZE    | SEGMENT NAME    | BITMASK (Top 7)  | PURPOSE / NOTES
+-------------------------|---------|-----------------|------------------|---------------------------------------
+ 0x00000000 - 0x00001FFF | 8 KB    | NULL TRAP       | 0000000          | Catch NULL derefs (up to 8KB offset)
+ 0x00002000 - 0x00002FFF | 4 KB    | REGISTER PAGE   | 0000000          | 256 x 64-bit GPRs + VM State
+ 0x00003000 - 0x00003FFF | 4 KB    | DOUBLE GUARD    | 0000000          | Protects Regs from Stack underflow
+ 0x00004000 - 0x01FFFFFF | ~32 MB  | STACK (UP)      | 0000000          | Frame: Grows UP toward External
+-------------------------|---------|-----------------|------------------|---------------------------------------
+ 0x02000000 - 0x41FFFFFF | 1 GB    | EXTERNAL (TLB)  | 0000001 - 0100000| 4096 x 256KB Cascaded Slots
+-------------------------|---------|-----------------|------------------|---------------------------------------
+ 0x42000000 - 0xC1FFFFFF | ~2 GB   | HEAP (ARENA)    | 0100001 - 1100000| Main Growable Host Memory (Hot Path)
+-------------------------|---------|-----------------|------------------|---------------------------------------
+ 0xC2000000 - 0xC2100FFF | 1 MB    | RETURN STACK    | 1100001 - ...    | Shadow Stack (32-bit return addrs)
+ 0xC2101000 - 0xD2100FFF | 256 MB  | RO METADATA     | ...              | core.reflect / Immutable Literals
+ 0xD2101000 - 0xFFFFFFFF | ~735 MB | SYSTEM WORKER   | ...     - 1111111| Fiber Meta / Waiter Tables / Debug
+-------------------------|---------|-----------------|------------------|---------------------------------------
*/

#define VM_TOP_BITS_SHIFT         25
#define VM_KIND_HEAP_START        33  // 0100001
#define VM_KIND_HEAP_END          96  // 1100000
#define VM_KIND_SYSTEM_START      97  // 1100001

// --- LOW SYSTEM AREA (top == 0) ---
#define VM_ADDR_NULL_TRAP         0x00000000
#define VM_ADDR_REG_PAGE          0x00002000
#define VM_ADDR_GUARD_PAGE        0x00003000
#define VM_ADDR_STACK_START       0x00004000

// --- EXTERNAL / TLB ---
#define VM_ADDR_EXTERNAL_START    0x02000000

// --- HEAP AREA ---
#define VM_ADDR_HEAP_START        0x42000000

// --- SYSTEM RESERVED AREA (top >= 97) ---
#define VM_ADDR_RESERVED_START    0xC2000000
#define VM_ADDR_RETURN_STACK      0xC2000000
#define VM_ADDR_RO_METADATA       0xC2101000
#define VM_ADDR_SYSTEM_WORKER     0xD2101000

// --- SIZES & LIMITS ---
#define VM_SIZE_REG_PAGE          (4  * 1024)
#define VM_SIZE_RETURN_STACK      (1  * 1024 * 1024)
#define VM_SIZE_RO_METADATA       (256 * 1024 * 1024)
#define VM_LIMIT_RECURSION        2000

#if 0
address_kind_t ClassifyAddress(uint32_t addr, uint32_t* out_offset)
{
    const uint32_t top = addr >> VM_TOP_BITS_SHIFT; // 128 chunks of 32MB
    address_kind_t kind = AddressKind_Invalid;
    uint32_t offset = 0;

    /* HOT PATH: Heap (0x42000000 - 0xC1FFFFFF) */
    // Top 7 bits: 33 (0100001) to 96 (1100000)
    if (top >= VM_KIND_HEAP_START && top <=  VM_KIND_HEAP_START)
    {
        kind   = AddressKind_Heap;
        offset = addr - VM_ADDR_HEAP_START;
    }
    /* SECOND HOT PATH: Frame (Regs & Stack in the first 32MB) */
    else if (top == 0)
    {
        // Stack grows up, starts at 0x4000
        if (addr >= VM_ADDR_STACK_START) 
        {
            kind   = AddressKind_Stack;
            offset = addr - VM_ADDR_STACK_START;
        }
        // 4KB Guard (0x3000 - 0x3FFF)
        else if (addr >= VM_ADDR_GUARD_PAGE)
        {
             // kind = AddressKind_Invalid; // Guard Hit
        }
        // 4KB Register Page (0x2000 - 0x2FFF)
        else if (addr >= 0x2000)
        {
            kind   = AddressKind_Register;
            offset = addr - 0x2000;
        }
        // 8KB NULL trap (0x0000 - 0x1FFF)
        else
        {
            // kind = AddressKind_Invalid; // NULL trap Hit
        }
    }
    /* SYSTEM RESERVED: (Shadow Stack, RO Metadata, System Data) */
    // Top 7 bits: 97 (1100001) to 127 (1111111)
    else if (top >= 97)
    {
        // Shadow Return Stack (dedicated for 32-bit return addresses)
/*      this is address invalid
        if (addr < 0xC2101000) 
        {
            kind   = AddressKind_ReturnStack;
            offset = addr - 0xC2000000;
        }
*/
        // RO METADATA (core.reflect, string literals, immutable data)
        else if (addr < 0xD2101000)
        {
            kind   = AddressKind_ROMetadata;
            offset = addr - 0xC2101000;
        }
        // Rest of SYSTEM RESERVED (Fibers, Waiter Tables, etc.)
        else
        {
            kind   = AddressKind_SystemReserved;
            offset = addr - 0xD2101000;
        }
    }
    /* EXTERNAL (TLB-backed) (0x02000000 - 0x41FFFFFF) */
    else if (top <= 32)
    {
        kind   = AddressKind_External;
        offset = addr - 0x02000000;
    }

    *out_offset = offset;
    return kind;
}
#endif

static inline address_kind_t ClassifyAddress(uint32_t addr)
{
    const uint32_t top = addr >> 25; // 128 chunks of 32MB

    /* HOT PATH: Heap */
    if (top >= 33 && top <= 96)
        return AddressKind_Heap;

    /* SECOND HOT PATH: Stack (top == 0, addr >= 0x4000) */
    if (top == 0)
    {
        if (addr >= 0x4000) return AddressKind_Stack;
        if (addr < 0x1000)  return AddressKind_Invalid; // NULL trap
        if (addr < 0x2000)  return AddressKind_Register;  // Register page

        return AddressKind_Invalid;         // Guard page
    }

    if (top <= 32)
        return AddressKind_External;

    return AddressKind_Reserved;
}

/*
string typeFlagsToString(BCTypeFlags flags) pure @safe
{
    string result;

    if (!flags)
    {
        result = "None";
        goto Lret;
    }

    if (flags & BCTypeFlags_Const)
    {
        result ~= "Const|";
    }

    // trim last |
    result = result[0 .. $-1];

Lret:
    return result;
}
*/

EXTERN_C BCValue imm32_(uint32_t value, bool signed_)
{
    BCValue ret = { BCValueType_Immediate };

    ret.type.type = signed_ ? BCTypeEnum_i32 : BCTypeEnum_u32;
    ret.type.typeIndex = 0;
    ret.type.flags = BCTypeFlags_None;
    ret.name = 0;
    ret.imm32.imm32 = value;

    ret.imm64.imm64 &= UINT32_MAX;
    return ret;
}

EXTERN_C BCValue imm64_(uint64_t value, bool signed_)
{
    BCValue ret = { BCValueType_Immediate };
;

    ret.type.type = signed_ ? BCTypeEnum_i64 : BCTypeEnum_u64;
    ret.type.typeIndex = 0;
    ret.type.flags = BCTypeFlags_None;
    ret.name = 0;
    ret.imm64.imm64 = value;
    return ret;
}

static inline void AllocDefaultHeap(BCHeap* newHeap)
{
    newHeap->heapMax = 1 << 14;
    newHeap->heapData = (uint8_t*)malloc(newHeap->heapMax);
    for(uint32_t i = 0; i < 255; i++)
    {
        newHeap->heapData[i] = (i & 0xff);
    }
    newHeap->heapSize = 128;
}

void* alloc_with_malloc(void* ctx, uint32_t size, void* fn)
{
    void* result = 0;

    if (size != FREE_SIZE)
    {
        result = malloc(size);
    }
    else
    {
        free(fn);
    }
    return result;
}

/*
template BCGenFunction(T, alias fn)
{
    static assert(ensureIsBCGen!T && is(typeof(fn()) == T));
    BCValue[] params;

    static if (is(typeof(T.init.functionalize()) == string))
    {
        static immutable BCGenFunction = mixin(fn().functionalize);
    }
    else static if (is(typeof(T.init.interpret(typeof(T.init.byteCode), typeof(params).init)()) : int))
    {
        static immutable BCGenFunction = ((BCValue[] args,
                BCHeap* heapPtr) => fn().interpret(args, heapPtr));
    }
}
*/
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
#ifndef ARRAY_SIZE
#  define ARRAY_SIZE(A) (sizeof(A) / sizeof(A[0]))
#endif

BCTypeEnum BCTypeEnum_commonTypeEnum(BCTypeEnum lhs, BCTypeEnum rhs)
{
    // HACK

    BCTypeEnum commonType = BCTypeEnum_Undef;

    if (lhs == BCTypeEnum_f52 || rhs == BCTypeEnum_f52)
    {
        commonType = BCTypeEnum_f52;
    }
    else if (lhs == BCTypeEnum_f23 || rhs == BCTypeEnum_f23)
    {
        commonType = BCTypeEnum_f23;
    }
    else if (lhs == BCTypeEnum_u64 || rhs == BCTypeEnum_u64)
    {
        commonType = BCTypeEnum_u64;
    }
    else if (lhs == BCTypeEnum_i64 || rhs == BCTypeEnum_i64)
    {
        commonType = BCTypeEnum_i64;
    }
    else if (lhs == BCTypeEnum_u32 || rhs == BCTypeEnum_u32)
    {
        commonType = BCTypeEnum_u32;
    }
    else if (lhs == BCTypeEnum_i32 || rhs == BCTypeEnum_i32)
    {
        commonType = BCTypeEnum_i32;
    }
    else if (BCTypeEnum_anyOf(lhs, smallIntegerTypes, ARRAY_SIZE(smallIntegerTypes))
        ||   BCTypeEnum_anyOf(rhs, smallIntegerTypes, ARRAY_SIZE(smallIntegerTypes)))
    {
        commonType = BCTypeEnum_i32;
    }

    if (commonType == BCTypeEnum_Undef)
    {
        // debug { if (!__ctfe) writeln("could not find common type for lhs: ", lhs, " and rhs: ", rhs); }
    }

    return commonType;
}

#undef offsetof

#endif
