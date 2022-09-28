#define _jit self->Jit

#include "3rd_party/lightning/include/lightning.h"
#include "../os/compat.h"
#include "backend_interface_funcs.h"
#include "bc_common.h"

#include "../os/bsf.h"

#define NREGS 6

typedef struct register_index_t
{
    uint16_t reg;
    uint16_t paried_reg;
} register_index_t;

typedef struct register_status_t
{
    uint32_t DirtyRegisterBitfield;
    uint32_t FreeBitfield;
    uint32_t UnusedBitfield;

    // which variable is currenly held in this register
    uint16_t FrameOffsets[NREGS];
} register_status_t;
// F = 1 D = 0 Inital state register is ready to use
// F = 0 D = 0 We loaded a value into register and it's associated but haven't modified it
// F = 0 D = 1 this register should not be used unless we are sure we can restore the state
// F = 1 D = 1 We are no longer relaying on the value is this register but we need to write it back into memory before using it again
//register_index_t

typedef struct Lightning
{
    jit_state_t* Jit;

    register_status_t Registers;

    const char** VariableNames;
    uint32_t* VariableLocations;
    uint32_t VariableCount;

    uint32_t* TemporaryLocations;
    uint32_t TemporaryCount;

    _Bool InFunction;
    uint32_t FrameSize;
    uint32_t CurrentFunctionIdx;
    uint8_t* codePage;
    uint32_t coagePageLeft;

    alloc_fn_t allocFn;
    void* allocCtx;

    uint32_t (*FunctionPtrs[512]) (BCHeap* heap,
                                   const char* fargs, ...);
    uint32_t nFunctions;
} Lightning;

static inline void RegisterStatus_Init(register_status_t* self)
{
    self->DirtyRegisterBitfield = 0;
    self->FreeBitfield = ((1 << NREGS) - 1);
    self->UnusedBitfield = 0;
    memset(self->FrameOffsets, 0, sizeof(self->FrameOffsets));
}

static inline void MarkDirty(Lightning* self, register_index_t reg)
{
    assert(reg.paried_reg == 0xFFFF);
    // register is not paired
    assert(reg.reg < 32);
    assert((self->Registers.DirtyRegisterBitfield & (1 << (reg.reg))) == 0);
}
/*
static inline jit_gpr_t GetInt32Register(Lightning* self, const BCValue* value)
{
    jit_gpr_t reg;

    assert(self->Registers.FreeBitfield != 0);

    uint32_t freeIdx = BSF(self->Registers.FreeBitfield) + 1;
#define callee_save_regs 4

    if (freeIdx && freeIdx <= callee_save_regs)
    {
        --freeIdx;
        self->Registers.FreeBitfield &= ~(1 << (freeIdx));
    }
    else if (1)
    {
        assert(0);
    }

    reg = JIT_R(freeIdx);
    if (value->vType == BCValueType_Immediate)
    {
        jit_movi(reg, value->imm32.imm32);
    }
    else if (BCValue_isStackValueOrParameter(value))
    {
        jit_ldxr_i(reg, JIT_FP, value->stackAddr.addr);
    }
    return (jit_gpr_t) { reg };
}
*/
bool BCType_IsInt32(BCTypeEnum type)
{
    switch(type)
    {
        case BCTypeEnum_i8:
        case BCTypeEnum_i16:
        case BCTypeEnum_i32:
        case BCTypeEnum_u8:
        case BCTypeEnum_u16:
        case BCTypeEnum_u32:
            return true;
        default:
            return false;
    }
}

static inline void Lightning_Initialize(Lightning* self, uint32_t n_args, ...)
{
    self->Jit = jit_new_state();
    RegisterStatus_Init(&self->Registers);
    jit_prolog();
}

static inline void Lightning_InitializeV(Lightning* self, uint32_t n_args, va_list args)
{
    assert(0);
}

static inline void Lightning_Finalize(Lightning* self)
{
    // jit_clear_state();
}

static inline uint32_t Lightning_BeginFunction(Lightning* self, uint32_t fnIdx, const void* fd)
{
    assert(!self->InFunction);
    self->InFunction = 1;
    if (!fnIdx)
    {
        fnIdx = ++self->nFunctions;
    }
    self->CurrentFunctionIdx = fnIdx;

    return fnIdx;
}

static inline void* Lightning_EndFunction(Lightning* self, uint32_t fnIdx)
{
    assert(self->InFunction);
    assert(self->CurrentFunctionIdx == fnIdx);
    self->InFunction = 0;
    self->FunctionPtrs[fnIdx] = jit_emit();
}

static inline BCValue Lightning_GenTemporary(Lightning* self, BCType bct)
{
    BCValue result = { BCValueType_Temporary };

    result.type = bct;
    result.temporaryIndex = ++self->TemporaryCount;
    result.stackAddr.addr = self->FrameSize;

    if (BCType_isBasicBCType(bct))
    {
        self->FrameSize += align4(BCTypeEnum_basicTypeSize(bct.type));
    }
    else
    {
        self->FrameSize += sizeof(void*);
    }

    return result;
}

static inline void Lightning_DestroyTemporary(Lightning* self, BCValue* tmp)
{
    assert(0);
}

static inline BCValue Lightning_GenLocal(Lightning* self, BCType bct, const char* name)
{
    assert(0);
}

static inline void Lightning_DestroyLocal(Lightning* self, BCValue* local)
{
    assert(0);
}

static inline BCValue Lightning_GenParameter(Lightning* self, BCType bct, const char* name)
{
    assert(0);
}

static inline void Lightning_EmitFlag(Lightning* self, BCValue* lhs)
{
    assert(0);
}

static inline void Lightning_Alloc(Lightning* self, BCValue *heapPtr, const BCValue* size)
{
    assert(0);
}

static inline void Lightning_Assert(Lightning* self, const BCValue* value, const BCValue* err)
{
    assert(0);
}

static inline void Lightning_MemCpy(Lightning* self, const BCValue* dst, const BCValue* src, const BCValue* size)
{
    assert(0);
}

static inline void Lightning_File(Lightning* self, const char* filename)
{
    assert(0);
}

static inline void Lightning_Line(Lightning* self, uint32_t line)
{
    assert(0);
}

static inline void Lightning_Comment(Lightning* self, const char* comment)
{
    assert(0);
}

static inline void Lightning_Prt(Lightning* self, const BCValue* value, bool isString)
{
    assert(0);
}

static inline void Lightning_Set(Lightning* self, BCValue *lhs, const BCValue* rhs)
{
    assert(0);
}

static inline void Lightning_Ult3(Lightning* self, BCValue *result, const BCValue* lhs, const BCValue* rhs)
{
    assert(0);
}

static inline void Lightning_Ule3(Lightning* self, BCValue *result, const BCValue* lhs, const BCValue* rhs)
{
    assert(0);
}

static inline void Lightning_Lt3(Lightning* self, BCValue *result, const BCValue* lhs, const BCValue* rhs)
{
    assert(0);
}

static inline void Lightning_Le3(Lightning* self, BCValue *result, const BCValue* lhs, const BCValue* rhs)
{
    assert(0);
}

static inline void Lightning_Ugt3(Lightning* self, BCValue *result, const BCValue* lhs, const BCValue* rhs)
{
    assert(0);
}

static inline void Lightning_Uge3(Lightning* self, BCValue *result, const BCValue* lhs, const BCValue* rhs)
{
    assert(0);
}

static inline void Lightning_Gt3(Lightning* self, BCValue *result, const BCValue* lhs, const BCValue* rhs)
{
    assert(0);
}

static inline void Lightning_Ge3(Lightning* self, BCValue *result, const BCValue* lhs, const BCValue* rhs)
{
    assert(0);
}

static inline void Lightning_Eq3(Lightning* self, BCValue *result, const BCValue* lhs, const BCValue* rhs)
{
    assert(0);
}

static inline void Lightning_Neq3(Lightning* self, BCValue *result, const BCValue* lhs, const BCValue* rhs)
{
    assert(0);
}

static inline void Lightning_Add3(Lightning* self, BCValue *result, const BCValue* lhs, const BCValue* rhs)
{
    assert(result->type.type == lhs->type.type && lhs->type.type == rhs->type.type);
    if (BCType_IsInt32(lhs->type.type) && BCType_IsInt32(rhs->type.type))
    {
        if (lhs->vType == BCValueType_Immediate && rhs->vType == BCValueType_Immediate)
        {
            uint32_t resultValue = lhs->imm32.imm32 + rhs->imm32.imm32;
            jit_movi(JIT_R0, resultValue);
        }
        else
        {
            // LoadValue(r1, lhs);
            // LoadValue(r2, rhs);

            jit_addr(JIT_R0, JIT_R1, JIT_R2);
    /*
            MarkDirty(self, (register_index_t){(uint16_t)r0, 0xFFFF});
            //MarkUnused(r1);
            //MarkUnused(r2);
    */

            // jit_stxi_i(result->stackAddr.addr, JIT_FP, r0);
            self->Registers.FreeBitfield = 0x63;
        }
    }

    else
        assert(0);

}

static inline void Lightning_Sub3(Lightning* self, BCValue *result, const BCValue* lhs, const BCValue* rhs)
{
    assert(0);
}

static inline void Lightning_Mul3(Lightning* self, BCValue *result, const BCValue* lhs, const BCValue* rhs)
{
    assert(0);
}

static inline void Lightning_Div3(Lightning* self, BCValue *result, const BCValue* lhs, const BCValue* rhs)
{
    assert(0);
}

static inline void Lightning_Udiv3(Lightning* self, BCValue *result, const BCValue* lhs, const BCValue* rhs)
{
    assert(0);
}

static inline void Lightning_And3(Lightning* self, BCValue *result, const BCValue* lhs, const BCValue* rhs)
{
    assert(0);
}

static inline void Lightning_Or3(Lightning* self, BCValue *result, const BCValue* lhs, const BCValue* rhs)
{
    assert(0);
}

static inline void Lightning_Xor3(Lightning* self, BCValue *result, const BCValue* lhs, const BCValue* rhs)
{
    assert(0);
}

static inline void Lightning_Lsh3(Lightning* self, BCValue *result, const BCValue* lhs, const BCValue* rhs)
{
    assert(0);
}

static inline void Lightning_Rsh3(Lightning* self, BCValue *result, const BCValue* lhs, const BCValue* rhs)
{
    assert(0);
}

static inline void Lightning_Mod3(Lightning* self, BCValue *result, const BCValue* lhs, const BCValue* rhs)
{
    assert(0);
}

static inline void Lightning_Umod3(Lightning* self, BCValue *result, const BCValue* lhs, const BCValue* rhs)
{
    assert(0);
}

static inline void Lightning_Not(Lightning* self, BCValue *result, const BCValue* val)
{
    assert(0);
}

static inline void Lightning_LoadFramePointer(Lightning* self, BCValue *result, const int32_t offset)
{
    assert(0);
}

static inline void Lightning_Call(Lightning* self, BCValue *result, const BCValue* fn, const BCValue* args, uint32_t n_args)
{
    assert(0);
}

static inline BCLabel Lightning_GenLabel(Lightning* self)
{
    assert(0);
}

static inline void Lightning_Jmp(Lightning* self, BCLabel target)
{
    assert(0);
}

static inline BCAddr Lightning_BeginJmp(Lightning* self)
{
    assert(0);
}

static inline void Lightning_EndJmp(Lightning* self, BCAddr atIp, BCLabel target)
{
    assert(0);
}

static inline CndJmpBegin Lightning_BeginCndJmp(Lightning* self, const BCValue* cond, bool ifTrue)
{
    assert(0);
}

static inline void Lightning_EndCndJmp(Lightning* self, const CndJmpBegin *jmp, BCLabel target)
{
    assert(0);
}

static inline void Lightning_Load8(Lightning* self, BCValue *dest, const BCValue* from)
{
    assert(0);
}

static inline void Lightning_Store8(Lightning* self, BCValue *dest, const BCValue* value)
{
    assert(0);
}

static inline void Lightning_Load16(Lightning* self, BCValue *dest, const BCValue* from)
{
    assert(0);
}

static inline void Lightning_Store16(Lightning* self, BCValue *dest, const BCValue* value)
{
    assert(0);
}

static inline void Lightning_Load32(Lightning* self, BCValue *dest, const BCValue* from)
{
    assert(0);
}

static inline void Lightning_Store32(Lightning* self, BCValue *dest, const BCValue* value)
{
    assert(0);
}

static inline void Lightning_Load64(Lightning* self, BCValue *dest, const BCValue* from)
{
    assert(0);
}

static inline void Lightning_Store64(Lightning* self, BCValue *dest, const BCValue* value)
{
    assert(0);
}

static inline void Lightning_Throw(Lightning* self, const BCValue* e)
{
    assert(0);
}

static inline void Lightning_PushCatch(Lightning* self)
{
    assert(0);
}

static inline void Lightning_PopCatch(Lightning* self)
{
    assert(0);
}

static inline void Lightning_Ret(Lightning* self, const BCValue* val)
{
    if (val->vType == BCValueType_Immediate)
    {
        if (BCType_IsInt32(val->type.type))
        {
            jit_reti(val->imm32.imm32);
        }
        else if (val->type.type == BCTypeEnum_f23)
        {
            jit_reti_f(*(float*)val->imm32.imm32);
        }
        else if (val->type.type == BCTypeEnum_f52)
        {
            jit_reti_d(*(double*)&val->imm64.imm64);
        }
    }
    else
    {
        jit_retr(JIT_R(0));
    }

}

static inline void Lightning_IToF32(Lightning* self, BCValue *result, const BCValue* rhs)
{
    assert(0);
}

static inline void Lightning_IToF64(Lightning* self, BCValue *result, const BCValue* rhs)
{
    assert(0);
}

static inline void Lightning_F32ToI(Lightning* self, BCValue *result, const BCValue* rhs)
{
    assert(0);
}

static inline void Lightning_F64ToI(Lightning* self, BCValue *result, const BCValue* rhs)
{
    assert(0);
}

static inline void Lightning_F32ToF64(Lightning* self, BCValue *result, const BCValue* rhs)
{
    assert(0);
}

static inline void Lightning_F64ToF32(Lightning* self, BCValue *result, const BCValue* rhs)
{
    assert(0);
}

static inline void Lightning_Memcmp(Lightning* self, BCValue *result, const BCValue* lhs, const BCValue* rhs)
{
    assert(0);
}

static inline void Lightning_Realloc(Lightning* self, BCValue *result, const BCValue* lhs, const BCValue* rhs, const uint32_t size)
{
    assert(0);
}

static inline BCValue Lightning_Run(Lightning* self, uint32_t fnIdx, const BCValue* args, uint32_t n_args, BCHeap* heap)
{
    uint32_t fnResult;
    BCValue result = {BCValueType_Unknown};

    jit_disassemble();
    void* marshaled_args = 0;
    fnResult = self->FunctionPtrs[fnIdx](heap, "", marshaled_args);
    result = imm32(fnResult);

    return result;
}

static inline uint32_t Lightning_sizeof_instance(Lightning* self)
{
    return sizeof (Lightning);
}

static inline void Lightning_clear_instance(Lightning* self)
{
    assert(0);
}

static inline void Lightning_init_instance(Lightning* self)
{
    init_jit("");
}

static inline void Lightning_fini_instance(Lightning* self)
{
    jit_destroy_state();

}

static inline void Lightning_ReadI32(Lightning* self, const BCValue* val, const ReadI32_cb_t readCb, void* userCtx)
{
    assert(0);
}

static inline void Lightning_set_alloc_memory(Lightning* self, alloc_fn_t alloc_fn, void* userCtx)
{
    self->allocFn = alloc_fn;
    self->allocCtx = userCtx;
}


static inline void Lightning_set_get_typeinfo(Lightning* self, get_typeinfo_fn_t get_typeinfo_fn, void* userCtx)
{
    assert(0);
}

const BackendInterface Lightning_interface = {
    /*.name = */ "Lightning",

    /*.Initialize = */(Initialize_t) Lightning_Initialize,
    /*.InitializeV = */(InitializeV_t) Lightning_InitializeV,
    /*.Finalize = */(Finalize_t) Lightning_Finalize,

    /*.BeginFunction = */(BeginFunction_t) Lightning_BeginFunction,
    /*.EndFunction = */(EndFunction_t) Lightning_EndFunction,

    /*.GenTemporary = */(GenTemporary_t) Lightning_GenTemporary,
    /*.DestroyTemporary = */(DestroyTemporary_t) Lightning_DestroyTemporary,
    /*.GenLocal = */(GenLocal_t) Lightning_GenLocal,
    /*.DestroyLocal = */(DestroyLocal_t) Lightning_DestroyLocal,
    /*.GenParameter = */(GenParameter_t) Lightning_GenParameter,
    /*.EmitFlag = */(EmitFlag_t) Lightning_EmitFlag,
    /*.Alloc = */(Alloc_t) Lightning_Alloc,
    /*.Assert = */(Assert_t) Lightning_Assert,
    /*.MemCpy = */(MemCpy_t) Lightning_MemCpy,
    /*.File = */(File_t) Lightning_File,
    /*.Line = */(Line_t) Lightning_Line,
    /*.Comment = */(Comment_t) Lightning_Comment,
    /*.Prt = */(Prt_t) Lightning_Prt,
    /*.Set = */(Set_t) Lightning_Set,
    /*.Ult3 = */(Ult3_t) Lightning_Ult3,
    /*.Ule3 = */(Lt3_t) Lightning_Ule3,
    /*.Lt3 = */(Lt3_t) Lightning_Lt3,
    /*.Le3 = */(Le3_t) Lightning_Le3,
    /*.Ugt3 = */(Ugt3_t) Lightning_Ugt3,
    /*.Uge3 = */(Uge3_t) Lightning_Uge3,
    /*.Gt3 = */(Gt3_t) Lightning_Gt3,
    /*.Ge3 = */(Ge3_t) Lightning_Ge3,
    /*.Eq3 = */(Eq3_t) Lightning_Eq3,
    /*.Neq3 = */(Neq3_t) Lightning_Neq3,
    /*.Add3 = */(Add3_t) Lightning_Add3,
    /*.Sub3 = */(Sub3_t) Lightning_Sub3,
    /*.Mul3 = */(Mul3_t) Lightning_Mul3,
    /*.Div3 = */(Div3_t) Lightning_Div3,
    /*.Udiv3 = */(Udiv3_t) Lightning_Udiv3,
    /*.And3 = */(And3_t) Lightning_And3,
    /*.Or3 = */(Or3_t) Lightning_Or3,
    /*.Xor3 = */(Xor3_t) Lightning_Xor3,
    /*.Lsh3 = */(Lsh3_t) Lightning_Lsh3,
    /*.Rsh3 = */(Rsh3_t) Lightning_Rsh3,
    /*.Mod3 = */(Mod3_t) Lightning_Mod3,
    /*.Umod3 = */(Umod3_t) Lightning_Umod3,
    /*.Not = */(Not_t) Lightning_Not,
    /*.LoadFramePointer = */(LoadFramePointer_t) Lightning_LoadFramePointer,
    /*.Call = */(Call_t) Lightning_Call,
    /*.GenLabel = */(GenLabel_t) Lightning_GenLabel,
    /*.Jmp = */(Jmp_t) Lightning_Jmp,
    /*.BeginJmp = */(BeginJmp_t) Lightning_BeginJmp,
    /*.EndJmp = */(EndJmp_t) Lightning_EndJmp,
    /*.BeginCndJmp = */(BeginCndJmp_t) Lightning_BeginCndJmp,
    /*.EndCndJmp = */(EndCndJmp_t) Lightning_EndCndJmp,
    /*.Load8 = */(Load8_t) Lightning_Load8,
    /*.Store8 = */(Store8_t) Lightning_Store8,
    /*.Load16 = */(Load16_t) Lightning_Load16,
    /*.Store16 = */(Store16_t) Lightning_Store16,
    /*.Load32 = */(Load32_t) Lightning_Load32,
    /*.Store32 = */(Store32_t) Lightning_Store32,
    /*.Load64 = */(Load64_t) Lightning_Load64,
    /*.Store64 = */(Store64_t) Lightning_Store64,
    /*.Throw = */(Throw_t) Lightning_Throw,
    /*.PushCatch = */(PushCatch_t) Lightning_PushCatch,
    /*.PopCatch = */(PopCatch_t) Lightning_PopCatch,
    /*.Ret = */(Ret_t) Lightning_Ret,
    /*.IToF32 = */(IToF32_t) Lightning_IToF32,
    /*.IToF64 = */(IToF64_t) Lightning_IToF64,
    /*.F32ToI = */(F32ToI_t) Lightning_F32ToI,
    /*.F64ToI = */(F64ToI_t) Lightning_F64ToI,
    /*.F32ToF64 = */(F32ToF64_t) Lightning_F32ToF64,
    /*.F64ToF32 = */(F64ToF32_t) Lightning_F64ToF32,
    /*.Memcmp = */(Memcmp_t) Lightning_Memcmp,
    /*.Realloc = */(Realloc_t) Lightning_Realloc,
    /*.Run = */(run_t) Lightning_Run,
    /*.ReadI32 = */(ReadI32_t) Lightning_ReadI32,

    /*.sizeof_instance = */(sizeof_instance_t) Lightning_sizeof_instance,
    /*.clear_instance = */(clear_instance_t) Lightning_clear_instance,
    /*.init_instance = */(init_instance_t) Lightning_init_instance,
    /*.fini_instance = */(fini_instance_t) Lightning_fini_instance,

    /*.set_alloc_memory = */(set_alloc_memory_t) Lightning_set_alloc_memory,
    /*.set_get_typeinfo = */(set_get_typeinfo_t) 0, //Lightning_set_get_typeinfo,
};
