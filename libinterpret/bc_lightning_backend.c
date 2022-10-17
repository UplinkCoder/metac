#define _jit self->Jit
#define AllocMemory(SIZE, FN) \
    self->allocMemory(self->allocCtx, SIZE, FN)

#include "3rd_party/lightning/include/lightning.h"
#include "../os/compat.h"
#include "backend_interface_funcs.h"
#include "bc_common.h"

#include "../os/bsf.h"

#define NREGS 6

#undef offsetof

#define offsetof(st, m) \
    ((size_t)((char *)&((st *)0)->m - (char *)0))

typedef struct register_index_t
{
    uint16_t reg;
    uint16_t paired;
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

typedef struct RuntimeContext
{
    size_t rSpill[NREGS];
    union
    {
        struct
        {
            uint32_t rtArg0;
            uint32_t rtArg1;
        };
        int64_t rtLongArg0;
    };

    union
    {
        struct
        {
            uint32_t rtArg2;
            uint32_t rtArg3;
        };
        int64_t rtLongArg1;
    };

    uint32_t currentLine;
    const char* currentFile;

    uint8_t* framePointer;
    uint8_t* stackPointer;

    uint8_t* stackDataBegin;
    uint8_t* stackDataLength;

    uint8_t* heapDataBegin;
    uint32_t* heapDataLength;
    // bc function to be determined.
    void* functions;

    //
    uint* heapSizeP;
    uint* stackSizeP;

    uint32_t breakpoints[4];

    BCValue returnValue;
} RuntimeContext;

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

    BCValue* Parameters;
    uint32_t ParametersCount;
    uint32_t ParametersCapacity;

    BCValue* Externals;
    uint32_t ExternalsCount;
    uint32_t ExternalsCapacity;

    jit_node_t *heapArg;
    jit_node_t *LstackAlloc;
    uint32_t stackOffset;

    uint32_t (*FunctionPtrs[512]) (RuntimeContext* rtCtx, int32_t* stackP,
                                   const char* fargs, ...);
    uint32_t nFunctions;

    alloc_fn_t allocMemory;
    get_typeinfo_fn_t getTypeInfo;

    void* allocCtx;
    void* getTypeInfoCtx;

    uint32_t currentLine;
    const char* currentFile;
} Lightning;

// cmp ops
#define HASH_EQ  0xdf // ==
#define HASH_NEQ 0x3a // !=
#define HASH_LT  0xcf // <
#define HASH_LE  0xa8 // <=
#define HASH_GT  0x38 // >
#define HASH_GE  0x46 // <=
#define HASH_ULT 0x60 // u<
#define HASH_ULE 0x23 // u<=
#define HASH_UGT 0x97 // u>
#define HASH_UGE 0xcd // u>=

// math ops
#define HASH_SUB  0xa3 // -
#define HASH_ADD  0x4b // +
#define HASH_MOD  0x6c // %
#define HASH_MUL  0x48 // *
#define HASH_DIV  0x54 // /
#define HASH_UMOD 0xc3 // u%
#define HASH_UDIV 0xfb // u/
#define HASH_UMUL 0xe7 // u*

// logic ops
#define HASH_AND 0x98 // &
#define HASH_OR  0x73 // |
#define HASH_COM 0x84 // ~
#define HASH_XOR 0x5a // ^

//float conversion ops
#define HASH_F23TOF52 0x71 // f23tof52
#define HASH_F52TOF23 0xa7 // f52tof23
#define HASH_F23TOI 0x3d // f23toi
#define HASH_ITOF23 0xe8 // itof23
#define HASH_F52TOI 0x1f // f52toi
#define HASH_ITOF52 0x7b // f52toi

static inline bool RuntimeContext_isPointerToStack(RuntimeContext* self, uint unrealPointer)
{
    return (unrealPointer & stackAddrMask) == stackAddrMask;
}

void* RuntimeContext_toRealPointer(RuntimeContext* self, uint32_t unrealPointer)
{
    void* realPointer = self->heapDataBegin + unrealPointer;

    if (RuntimeContext_isPointerToStack(self, unrealPointer))
    {
        uint32_t stackOffset = (unrealPointer & ~stackAddrMask);
        realPointer = self->stackDataBegin + stackOffset;
    }

    return realPointer;
}
static inline void RegisterStatus_Init(register_status_t* self)
{
    self->DirtyRegisterBitfield = 0;
    self->FreeBitfield = ((1 << NREGS) - 1);
    self->UnusedBitfield = 0;
    memset(self->FrameOffsets, 0, sizeof(self->FrameOffsets));
}

static inline void MarkDirty(Lightning* self, register_index_t reg)
{
    assert(reg.paired == 0xFFFF);
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

static inline register_index_t LoadRegValue(Lightning* self,
                                            register_index_t target, BCValue* val)
{
    switch(BCTypeEnum_basicTypeSize(val->type.type))
    {
        default:
            assert(0);

        case 1 :
            jit_ldxi_c(target.reg - 1, JIT_FP, -val->stackAddr.addr);
        break;
        case 2 :
            jit_ldxi_s(target.reg - 1, JIT_FP, -val->stackAddr.addr);
        break;
        case 4 :
            jit_ldxi_i(target.reg - 1, JIT_FP, -val->stackAddr.addr);
        break;
        case 8 :
#ifdef _32BIT
#else
            jit_ldxi_l(target.reg - 1, JIT_FP, -val->stackAddr.addr);
#endif
        break;
    }
}

static inline register_index_t StoreRegValue(Lightning* self,
                                             BCValue* result, register_index_t target)
{
    assert(BCValue_isStackValueOrParameter(result));

    switch(BCTypeEnum_basicTypeSize(result->type.type))
    {
        default:
            assert(0);

        case 1 :
            jit_stxi_c(-result->stackAddr.addr, JIT_FP, target.reg - 1);
        break;
        case 2 :
            jit_stxi_s(-result->stackAddr.addr, JIT_FP, target.reg - 1);
        break;
        case 4 :
            jit_stxi_i(-result->stackAddr.addr, JIT_FP, target.reg - 1);
        break;
        case 8 :
            jit_stxi_i(-result->stackAddr.addr, JIT_FP, target.reg - 1);
            jit_stxi_i(-result->stackAddr.addr + 4, JIT_FP, target.paired - 1);
        break;
    }
}
static const register_index_t r0Index = {JIT_R0 + 1, 0};
static const register_index_t r1Index = {JIT_R1 + 1, 0};

static inline void PrintS2R(Lightning* self, const char* f, jit_gpr_t reg, jit_gpr_t reg2)
{
    jit_prepare();
    jit_pushargi((jit_pointer_t) f);
    jit_ellipsis();
    jit_pushargr(reg);
    jit_pushargr(reg2);
    jit_finishi(printf);
}


static inline void Lightning_Initialize(Lightning* self, uint32_t n_args, ...)
{
    RegisterStatus_Init(&self->Registers);
    self->Jit = jit_new_state();
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

    jit_prolog();
    // self->getFrameSize(fd);
    //TODO implement a way to get the assumed frame size of a function
    jit_frame(256);
    self->heapArg = jit_arg();
    jit_getarg(JIT_V2, self->heapArg);

    self->FrameSize = 4;

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
        self->FrameSize += 4;
    }

    return result;
}

static inline void Lightning_DestroyTemporary(Lightning* self, BCValue* tmp)
{
    //assert(0);
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
    BCValue p;

    p.type = bct;
    p.vType = BCValueType_Parameter;
    p.parameterIndex = ++self->ParametersCount;
    p.stackAddr.addr = self->FrameSize;

    self->FrameSize += align4(BCTypeEnum_basicTypeSize(bct.type));
    p.name = name;

    return p;
}

static inline BCValue Lightning_GenExternal(Lightning* self, BCType bct, const char* name)
{
    BCValue p;

    p.type = bct;
    p.vType = BCValueType_External;
    p.externalIndex = ++self->ExternalsCount;
    p.stackAddr.addr = self->FrameSize;

    self->FrameSize += align4(BCTypeEnum_basicTypeSize(bct.type));
    p.name = name;

    return p;
}


static inline BCValue Lightning_MapExternal (Lightning* self,
                                             void* memory, uint32_t sz)
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
    self->currentFile = filename;
}

static inline void Lightning_Line(Lightning* self, uint32_t line)
{
    self->currentLine = line;
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
    if (rhs->vType == BCValueType_Immediate)
    {
        jit_movi(JIT_R1, rhs->imm32.imm32);
    }
    else
    {
        LoadRegValue(self, r1Index, rhs);
    }

    StoreRegValue(self, lhs, r1Index);
}

static inline void Lightning_CmpOp3(Lightning* self, BCValue *result, const BCValue* lhs, const BCValue* rhs, unsigned char op)
{
    if (BCType_IsInt32(lhs->type.type) && BCType_IsInt32(rhs->type.type))
    {
        if (lhs->vType == BCValueType_Immediate)
        {
            jit_movi(JIT_R0, lhs->imm32.imm32);
        }
        else
        {
            LoadRegValue(self, r0Index, lhs);
        }

        if (rhs->vType == BCValueType_Immediate)
        {
            switch(op)
            {
                default: assert(0);

                case HASH_LE:
                    jit_lei(JIT_R0, JIT_R0, rhs->imm32.imm32);
                break;
                case HASH_ULE:
                    jit_lei_u(JIT_R0, JIT_R0, rhs->imm32.imm32);
                break;
                case HASH_LT:
                    jit_lti(JIT_R0, JIT_R0, rhs->imm32.imm32);
                break;
                case HASH_ULT:
                    jit_lti_u(JIT_R0, JIT_R0, rhs->imm32.imm32);
                break;

                case HASH_GE:
                    jit_gei(JIT_R0, JIT_R0, rhs->imm32.imm32);
                break;
                case HASH_UGE:
                    jit_gei_u(JIT_R0, JIT_R0, rhs->imm32.imm32);
                break;
                case HASH_GT:
                    jit_gti(JIT_R0, JIT_R0, rhs->imm32.imm32);
                break;
                case HASH_UGT:
                    jit_gti_u(JIT_R0, JIT_R0, rhs->imm32.imm32);
                break;

                case HASH_EQ:
                    jit_eqi(JIT_R0, JIT_R0, rhs->imm32.imm32);
                break;

                case HASH_NEQ:
                    jit_eqi(JIT_R0, JIT_R0, rhs->imm32.imm32);
                    jit_eqi(JIT_R0, JIT_R0, 0);
                break;

            }
        }
        else
        {
            LoadRegValue(self, r1Index, rhs);

            switch(op)
            {
                default: assert(0);

                case HASH_LE:
                    jit_ler(JIT_R0, JIT_R0, JIT_R1);
                break;
                case HASH_ULE:
                    jit_lei_u(JIT_R0, JIT_R0, JIT_R1);
                break;
                case HASH_LT:
                    jit_ltr(JIT_R0, JIT_R0, JIT_R1);
                break;
                case HASH_ULT:
                    jit_lti_u(JIT_R0, JIT_R0, JIT_R1);
                break;
                
                case HASH_GE:
                    jit_ger(JIT_R0, JIT_R0, JIT_R1);
                break;
                case HASH_UGE:
                    jit_gei_u(JIT_R0, JIT_R0, JIT_R1);
                break;
                case HASH_GT:
                    jit_gtr(JIT_R0, JIT_R0, JIT_R1);
                break;
                case HASH_UGT:
                    jit_gti_u(JIT_R0, JIT_R0, JIT_R1);
                break;

                case HASH_EQ:
                    jit_eqr(JIT_R0, JIT_R0, JIT_R1);
                break;

                case HASH_NEQ:
                    jit_eqr(JIT_R0, JIT_R0, JIT_R1);
                    jit_eqi(JIT_R0, JIT_R0, 0);
                break;
            }
        }
    }
    
    StoreRegValue(self, result, r0Index);
}


static inline void Lightning_Ult3(Lightning* self, BCValue *result, const BCValue* lhs, const BCValue* rhs)
{
    Lightning_CmpOp3(self, result, lhs, rhs, HASH_ULT);
}

static inline void Lightning_Ule3(Lightning* self, BCValue *result, const BCValue* lhs, const BCValue* rhs)
{
    Lightning_CmpOp3(self, result, lhs, rhs, HASH_ULE);
}

static inline void Lightning_Lt3(Lightning* self, BCValue *result, const BCValue* lhs, const BCValue* rhs)
{
    Lightning_CmpOp3(self, result, lhs, rhs, HASH_LT);
}

static inline void Lightning_Le3(Lightning* self, BCValue *result, const BCValue* lhs, const BCValue* rhs)
{
    Lightning_CmpOp3(self, result, lhs, rhs, HASH_LE);
}

static inline void Lightning_Ugt3(Lightning* self, BCValue *result, const BCValue* lhs, const BCValue* rhs)
{
    Lightning_CmpOp3(self, result, lhs, rhs, HASH_UGT);
}

static inline void Lightning_Uge3(Lightning* self, BCValue *result, const BCValue* lhs, const BCValue* rhs)
{
    Lightning_CmpOp3(self, result, lhs, rhs, HASH_UGE);
}

static inline void Lightning_Gt3(Lightning* self, BCValue *result, const BCValue* lhs, const BCValue* rhs)
{
    Lightning_CmpOp3(self, result, lhs, rhs, HASH_GT);
}

static inline void Lightning_Ge3(Lightning* self, BCValue *result, const BCValue* lhs, const BCValue* rhs)
{
    Lightning_CmpOp3(self, result, lhs, rhs, HASH_GE);
}

static inline void Lightning_Eq3(Lightning* self, BCValue *result, const BCValue* lhs, const BCValue* rhs)
{
    Lightning_CmpOp3(self, result, lhs, rhs, HASH_EQ);
}

static inline void Lightning_Neq3(Lightning* self, BCValue *result, const BCValue* lhs, const BCValue* rhs)
{
    Lightning_CmpOp3(self, result, lhs, rhs, HASH_NEQ);
}

static inline void PrintString(Lightning* self, const char* text)
{
    jit_prepare();
    jit_pushargi((jit_pointer_t)text);
    jit_ellipsis();

    jit_finishi(printf);
}

static inline void PrintPointerR(Lightning* self, uint32_t line, jit_gpr_t reg)
{
    jit_prepare();
    jit_pushargi((jit_pointer_t)"[%d] %p\n");
    jit_ellipsis();
    jit_pushargi((jit_pointer_t) line);
    jit_pushargr(reg);
    jit_finishi(printf);
}

static inline void Lightning_LogicOp3(Lightning* self, BCValue *result, const BCValue* lhs, const BCValue* rhs, unsigned char op)
{
    if (BCType_IsInt32(lhs->type.type) && BCType_IsInt32(rhs->type.type))
    {
        if (rhs->vType == BCValueType_Immediate)
        {
            switch(op)
            {
                default: assert(0);
                case HASH_COM:
                    jit_comr(JIT_R0, rhs->imm32.imm32);
                break;
            }
        }
    }
}

static inline void Lightning_MathOp2(Lightning* self, BCValue *result,const BCValue* rhs, char op)
{
    if (BCType_IsInt32(result->type.type) && BCType_IsInt32(rhs->type.type))
    {
        if (rhs->vType == BCValueType_Immediate)
        {
            jit_movi(JIT_R0, rhs->imm32.imm32);
        }
        else
        {
            LoadRegValue(self, r0Index, rhs);
        }

        switch(op)
        {
            case HASH_COM:
            {
                jit_comr(JIT_R0, JIT_R0);
            } break;
        }

        StoreRegValue(self, result, r0Index);
    }
    else
        assert(0);
}


static inline void Lightning_MathOp3(Lightning* self, BCValue *result, const BCValue* lhs, const BCValue* rhs, unsigned char op)
{
    assert(result->type.type == lhs->type.type && lhs->type.type == rhs->type.type);
    if (BCType_IsInt32(lhs->type.type) && BCType_IsInt32(rhs->type.type))
    {
        if (lhs->vType == BCValueType_Immediate)
        {
            jit_movi(JIT_R0, lhs->imm32.imm32);
        }
        else
        {
            LoadRegValue(self, r0Index, lhs);
        }

        if (rhs->vType == BCValueType_Immediate)
        {
            switch(op)
            {
                default: assert(0);

                case HASH_ADD:
                    jit_addi(JIT_R0, JIT_R0, rhs->imm32.imm32);
                break;
                case HASH_SUB:
                    jit_subi(JIT_R0, JIT_R0, rhs->imm32.imm32);
                break;
                case HASH_MUL:
                    jit_muli(JIT_R0, JIT_R0, rhs->imm32.imm32);
                break;
                case HASH_DIV:
                    jit_divi(JIT_R0, JIT_R0, rhs->imm32.imm32);
                break;
                case HASH_MOD:
                    jit_remi(JIT_R0, JIT_R0, rhs->imm32.imm32);
                break;
            }
        }
        else
        {
            LoadRegValue(self, r1Index, rhs);

            switch(op)
            {
                default: assert(0);

                case HASH_ADD:
                    jit_addr(JIT_R0, JIT_R0, JIT_R1);
                break;
                case HASH_SUB:
                    jit_subr(JIT_R0, JIT_R0, JIT_R1);
                break;
                case HASH_MUL:
                    jit_mulr(JIT_R0, JIT_R0, JIT_R1);
                break;
                case HASH_DIV:
                    jit_divr(JIT_R0, JIT_R0, JIT_R1);
                break;
                case HASH_MOD:
                    jit_remr(JIT_R0, JIT_R0, JIT_R1);
                break;
            }
        }

        StoreRegValue(self, result, r0Index);
    }
    else
        assert(0);
}


static inline void Lightning_Add3(Lightning* self, BCValue *result, const BCValue* lhs, const BCValue* rhs)
{
    assert(result->type.type == lhs->type.type && lhs->type.type == rhs->type.type);
    if (BCType_IsInt32(lhs->type.type) && BCType_IsInt32(rhs->type.type))
    {
        Lightning_MathOp3(self, result, lhs, rhs, HASH_ADD);
    }
    else
        assert(0);

}

static inline void Lightning_Sub3(Lightning* self, BCValue *result, const BCValue* lhs, const BCValue* rhs)
{
    if (BCType_IsInt32(lhs->type.type) && BCType_IsInt32(rhs->type.type))
    {
        Lightning_MathOp3(self, result, lhs, rhs, HASH_SUB);
    }
    else
        assert(0);
}

static inline void Lightning_Mul3(Lightning* self, BCValue *result, const BCValue* lhs, const BCValue* rhs)
{
    if (BCType_IsInt32(lhs->type.type) && BCType_IsInt32(rhs->type.type))
    {
        Lightning_MathOp3(self, result, lhs, rhs, HASH_MUL);
    }
    else
        assert(0);
}

static inline void Lightning_Div3(Lightning* self, BCValue *result, const BCValue* lhs, const BCValue* rhs)
{
    if (BCType_IsInt32(lhs->type.type) && BCType_IsInt32(rhs->type.type))
    {
        Lightning_MathOp3(self, result, lhs, rhs, HASH_DIV);
    }
    else
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
    if (BCType_IsInt32(lhs->type.type) && BCType_IsInt32(rhs->type.type))
    {
        Lightning_MathOp3(self, result, lhs, rhs, HASH_MOD);
    }
    else
        assert(0);
}

static inline void Lightning_Umod3(Lightning* self, BCValue *result, const BCValue* lhs, const BCValue* rhs)
{
    assert(0);
}

static inline void Lightning_Not(Lightning* self, BCValue *result, const BCValue* val)
{
    Lightning_MathOp2(self, result, val, HASH_COM);
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


static inline void Lightning_Load(Lightning* self, BCValue *dest, const BCValue* from, uint32_t sz)
{

    assert(BCValue_isStackValueOrParameter(dest));

    jit_ldxi(JIT_R1, JIT_V2, offsetof(BCHeap, heapData));

    if (from->vType == BCValueType_Immediate)
    {
        switch(sz)
        {
            default: assert(0);
            case 1:
                jit_ldxi_c(JIT_R0, JIT_R1, from->imm32.imm32);
            break;
            case 2:
                jit_ldxi_s(JIT_R0, JIT_R1, from->imm32.imm32);
            break;
            case 4:
                jit_ldxi_i(JIT_R0, JIT_R1, from->imm32.imm32);
            break;
            case 8:
                assert(0);
        }

    }
    else
    {
        jit_ldxi(JIT_R2, JIT_FP, -from->stackAddr.addr);
        switch(sz)
        {
            default: assert(0);
            case 1:
                jit_ldxr_c(JIT_R0, JIT_R1, JIT_R2);
            break;
            case 2:
                jit_ldxr_s(JIT_R0, JIT_R1, JIT_R2);
            break;
            case 4:
                jit_ldxr_i(JIT_R0, JIT_R1, JIT_R2);
            break;
            case 8:
                assert(0);
        }
    }

    jit_stxi(-dest->stackAddr.addr, JIT_FP, JIT_R0);
}

static inline void Lightning_Store(Lightning* self, BCValue *dest, const BCValue* value, uint32_t sz)
{
    if (value->vType == BCValueType_Immediate)
    {
        jit_movi(JIT_R0, value->imm32.imm32);
    }
    else
    {
        LoadRegValue(self, r0Index, value);
    }

    jit_ldxi(JIT_R1, JIT_V2, offsetof(BCHeap, heapData));
    if (dest->vType == BCValueType_Immediate)
    {
        switch(sz)
        {
            default: assert(0);
            case 1:
                jit_stxi_c(dest->imm32.imm32, JIT_R1, JIT_R0);
            break;
            case 2:
                jit_stxi_s(dest->imm32.imm32, JIT_R1, JIT_R0);
            break;
            case 4:
                jit_stxi_i(dest->imm32.imm32, JIT_R1, JIT_R0);
            break;
            case 8:
                assert(0);
        }
    }
    else
    {
        jit_ldxi(JIT_R2, JIT_FP, -dest->stackAddr.addr);
        switch(sz)
        {
            default: assert(0);
            case 1:
                jit_stxr_c(JIT_R2, JIT_R1, JIT_R0);
            break;
            case 2:
                jit_stxr_s(JIT_R2, JIT_R1, JIT_R0);
            break;
            case 4:
                jit_stxr_i(JIT_R2, JIT_R1, JIT_R0);
            break;
            case 8:
                assert(0);
        }
    }
}


static inline void Lightning_Load8(Lightning* self, BCValue *dest, const BCValue* from)
{
    Lightning_Load(self, dest, from, 1);
}

static inline void Lightning_Store8(Lightning* self, BCValue *dest, const BCValue* value)
{
    Lightning_Store(self, dest, value, 1);
}

static inline void Lightning_Load16(Lightning* self, BCValue *dest, const BCValue* from)
{
    Lightning_Load(self, dest, from, 2);
}

static inline void Lightning_Store16(Lightning* self, BCValue *dest, const BCValue* value)
{
    Lightning_Store(self, dest, value, 2);
}


static inline void Lightning_Load32(Lightning* self, BCValue *dest, const BCValue* from)
{
    Lightning_Load(self, dest, from, 4);
}

static inline void Lightning_Store32(Lightning* self, BCValue *dest, const BCValue* value)
{
    Lightning_Store(self, dest, value, 4);
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
            jit_reti_f(*(float*)&val->imm32.imm32);
        }
        else if (val->type.type == BCTypeEnum_f52)
        {
            jit_reti_d(*(double*)&val->imm64.imm64);
        }
    }
    else
    {
        LoadRegValue(self, r0Index, val);
        jit_retr(JIT_R0);
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
    char parameterString[64];

    void* marshaled_args = self->allocMemory(self->allocCtx, sizeof(double) * n_args, 0);
    for(uint32_t i = 0; i < n_args; i++)
    {
        assert((args + i)->vType == BCValueType_Immediate);
        switch((args + i)->type.type)
        {
            default: assert(!"ParameterType not handled");

            case BCTypeEnum_u8:
            case BCTypeEnum_u16:
            case BCTypeEnum_u32:
            case BCTypeEnum_i8:
            case BCTypeEnum_i16:
            case BCTypeEnum_i32:
            case BCTypeEnum_c8:
            case BCTypeEnum_c16:
            case BCTypeEnum_c32:
                (*(int32_t*) marshaled_args) = (args + i)->imm32.imm32;
            break;
        }
        marshaled_args += (sizeof(double));
    }
    jit_disassemble();
    fnResult = self->FunctionPtrs[fnIdx](heap, "", marshaled_args);
    result = imm32(fnResult);

    return result;
}

static inline uint32_t Lightning_sizeof_instance(Lightning* self)
{
    return sizeof (Lightning);
}

static inline void Lightning_clear_instance(Lightning* instance)
{
    instance->allocMemory = 0;
    instance->nFunctions = 0;
}

static inline void Lightning_init_instance(Lightning* self)
{
    if (!self->allocMemory)
    {
        self->allocMemory = alloc_with_malloc;
    }

    init_jit("");
#define INITIAL_PARAMETER_CAPACITY 16
    self->Parameters = (BCValue*) self->allocMemory(self->allocCtx,
                          sizeof(BCValue) * INITIAL_PARAMETER_CAPACITY, 0);
    self->ParametersCapacity = INITIAL_PARAMETER_CAPACITY;
    self->ParametersCount = 0;
#define INITIAL_EXTERNAL_CAPACITY 8
    self->Externals = (BCValue*) self->allocMemory(self->allocCtx,
                          sizeof(BCValue) * INITIAL_PARAMETER_CAPACITY, 0);
    self->ExternalsCapacity = INITIAL_EXTERNAL_CAPACITY;
    self->ExternalsCount = 0;

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
    self->allocMemory = alloc_fn;
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
    /*.GenExternal = */(GenParameter_t) Lightning_GenExternal,
    /*.MapExternal = */(MapExternal_t) Lightning_MapExternal,
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
