#define _jit self->Jit
#define AllocMemory(SIZE, FN) \
    self->allocMemory(self->allocCtx, SIZE, FN)

#include "lightning.h"
#include "../os/compat.h"
#include "backend_interface_funcs.h"
#include "bc_common.h"
#include "bc_lightning_debug.h"

#define NREGS 6

#undef offsetof

#define offsetof(st, m) \
    ((size_t)((char *)&((st *)0)->m - (char *)0))

#define LIGHTNING_LABEL_CAPACITY 4096
#define LIGHTNING_READI32_CAPACITY 64

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

typedef struct Lightning_External {
  void* addr;
  uint32_t size;
  uint32_t mapAddr;
} Lightning_External;

typedef struct Lightning_ExternalFunc {
  void* addr;
  BCType* parameterTypes;
  uint32_t ParameterTypesCount;
  BCType returnType;

  uint32_t mapAddr;
} Lightning_ExternalFunc;

typedef struct Lightning_Functiontype {
    BCType* parameterTypes;
    uint32_t ParameterTypesCount;
    BCType returnType;
} Lightning_Functiontype;

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
    uint32_t* heapCapacityP;
    // bc function to be determined.
    void* functions;

    //
    uint* heapSizeP;
    uint* stackSizeP;

    uint32_t breakpoints[4];

    BCValue returnValue;

    Lightning_External* externals;
    uint32_t externalsCount;
    uint32_t externalsCapacity;

    Lightning_ExternalFunc* externalFuncs;
    uint32_t externalFuncsCount;
    uint32_t externalFuncsCapacity;


    uint32_t MapAddr;

    struct Lightning* instance;
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
    _Bool Finalized;
    uint32_t FrameSize;
    int32_t CondAddr;
    uint32_t CurrentFunctionIdx;
    uint8_t* codePage;
    uint32_t coagePageLeft;

    BCValue* Parameters;
    uint32_t ParametersCount;
    uint32_t ParametersCapacity;

    BCValue* Externals;
    uint32_t ExternalsCount;
    uint32_t ExternalsCapacity;

    BCValue* ExternalFuncs;
    uint32_t ExternalFuncsCount;
    uint32_t ExternalFuncsCapacity;


    jit_node_t *RuntimeContextArg;
    jit_node_t *ArgumentsArg;
    jit_node_t *NumberOfArgumentsArg;

    uint32_t stackOffset;

    jit_node_t* FunctionLabels[512];
    uint32_t (*FunctionPtrs[512]) (RuntimeContext* rtCtx, const int64_t* args, uint32_t nArgs);
    uint32_t nFunctions;

    jit_node_t* Labels[LIGHTNING_LABEL_CAPACITY];
    uint32_t LabelCount;
    jit_node_t* PendingJumps[LIGHTNING_LABEL_CAPACITY];
    uint32_t PendingJumpCount;

    ReadI32_cb_t ReadI32Callbacks[LIGHTNING_READI32_CAPACITY];
    void* ReadI32UserCtxs[LIGHTNING_READI32_CAPACITY];
    uint32_t ReadI32Count;

    alloc_fn_t allocMemory;
    get_typeinfo_fn_t getTypeInfo;

    void* allocCtx;
    void* getTypeInfoCtx;

    uint32_t currentLine;
    const char* currentFile;

    // DWARF recorder, written by Dbg_Write (see bc_lightning_debug.h)
    DbgRecorder Dbg;
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
#define HASH_LSH  0xab // <<
#define HASH_RSH  0x2b // >>

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

static inline uint8_t* RuntimeContext_toRealPointer(RuntimeContext* self,
                                                    uint32_t unrealPointer)
{
    uint32_t offset = 0;
    address_kind_t kind = ClassifyAddress(unrealPointer, &offset);
    switch (kind)
    {
        case AddressKind_Heap:
            return self->heapDataBegin + offset;
        case AddressKind_Stack:
            return self->framePointer + offset;
        case AddressKind_External:
            for (uint32_t i = 0; i < self->externalsCount; i++)
            {
                Lightning_External* external = self->externals + i;
                if (unrealPointer >= external->mapAddr
                 && unrealPointer - external->mapAddr < external->size)
                {
                    return (uint8_t*)external->addr + (unrealPointer - external->mapAddr);
                }
            }
            break;
        default:
            break;
    }

    assert(!"Invalid or unmapped VM address");
    return 0;
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

static inline bool ConvertsToPointer(BCTypeEnum bcTypeEnum)
{
    bool result = false;

    if (bcTypeEnum == BCTypeEnum_Struct)
    {
        result = true;
    }
    if (bcTypeEnum == BCTypeEnum_Ptr)
    {
        result = true;
    }

    return result;
}

static inline void LoadRegValue(Lightning* self,
                                register_index_t target, const BCValue* val)
{
    uint32_t sz = 0;

    if (BCType_isBasicBCType(val->type))
    {
        sz = BCTypeEnum_basicTypeSize(val->type.type);
    }
    else if (val->type.type == BCTypeEnum_Struct
          || val->type.type == BCTypeEnum_Tuple)
    {
        sz = 4;
    }
    else if (ConvertsToPointer(val->type.type))
    {
        sz = sizeof(void*);
    }

    switch(sz)
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
            jit_stxi_i(target.reg - 1, JIT_FP, -val->stackAddr.addr);
            jit_stxi_i(target.paired - 1, JIT_FP, -val->stackAddr.addr + 4);
#else
            jit_ldxi_l(target.reg - 1, JIT_FP, -val->stackAddr.addr);
#endif
        break;
    }
}

static inline void StoreRegValue(Lightning* self,
                                 BCValue* result, register_index_t target)
{
    assert(BCValue_isStackValueOrParameter(result));
    uint32_t sz = 0;
    if (BCType_isBasicBCType(result->type))
    {
        sz = BCTypeEnum_basicTypeSize(result->type.type);
    }
    else if (result->type.type == BCTypeEnum_Struct
          || result->type.type == BCTypeEnum_Tuple)
    {
        sz = 4;
    }
    else if (ConvertsToPointer(result->type.type))
    {
        sz = sizeof(void*);
    }
    switch(sz)
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
#ifdef _32BIT
            jit_stxi_i(-result->stackAddr.addr, JIT_FP, target.reg - 1);
            jit_stxi_i(-result->stackAddr.addr + 4, JIT_FP, target.paired - 1);
#else
            jit_stxi_l(-result->stackAddr.addr, JIT_FP, target.reg - 1);
#endif
        break;
    }
}
// immediates of 8-byte types use the full imm64, everything else is sign-extended from imm32
static inline int64_t Lightning_ImmediateBits(const BCValue* val)
{
    if (BCType_isBasicBCType(val->type)
        && BCTypeEnum_basicTypeSize(val->type.type) == 8)
    {
        return val->imm64.imm64;
    }
    return (int64_t) val->imm32.imm32;
}

static const register_index_t r0Index = {JIT_R0 + 1, 0};
static const register_index_t r1Index = {JIT_R1 + 1, 0};
static const register_index_t r2Index = {JIT_R2 + 1, 0};

static inline void PrintS2R(Lightning* self, const char* f, jit_gpr_t reg, jit_gpr_t reg2)
{
    jit_prepare();
    jit_pushargi((intptr_t) f);
    jit_ellipsis();
    jit_pushargr(reg);
    jit_pushargr(reg2);
    jit_finishi(printf);
}


static uint64_t Lightning_NodeAddr(void* ctx, void* node)
{
    Lightning* self = ctx;
    return (uint64_t)(uintptr_t) jit_address((jit_node_t*) node);
}

// returns 0 on success; the instance must be finalized so addresses are known
static int Lightning_WriteDebugElf(Lightning* self, const char* path)
{
    if (!self->Finalized)
        return -1;

    jit_word_t codeSize = 0;
    const uint8_t* code = (const uint8_t*) jit_get_code(&codeSize);
    return Dbg_Write(&self->Dbg, path, code, (uint64_t) codeSize, Lightning_NodeAddr, self);
}

static uint8_t Lightning_DbgBaseType(BCType bct)
{
    if (BCType_isBasicBCType(bct))
    {
        switch (bct.type)
        {
            case BCTypeEnum_i8:  return DbgBaseType_I8;
            case BCTypeEnum_i16: return DbgBaseType_I16;
            case BCTypeEnum_i32: return DbgBaseType_I32;
            case BCTypeEnum_i64: return DbgBaseType_I64;
            case BCTypeEnum_u8:  return DbgBaseType_U8;
            case BCTypeEnum_u16: return DbgBaseType_U16;
            case BCTypeEnum_u32: return DbgBaseType_U32;
            case BCTypeEnum_u64: return DbgBaseType_U64;
            case BCTypeEnum_c8:  return DbgBaseType_U8;
            case BCTypeEnum_c16: return DbgBaseType_U16;
            case BCTypeEnum_c32: return DbgBaseType_U32;
            case BCTypeEnum_f23: return DbgBaseType_F32;
            case BCTypeEnum_f52: return DbgBaseType_F64;
            default: break;
        }
    }
    // pointers and aggregates are shown as an unsigned word
    return DbgBaseType_U64;
}

static void Lightning_DbgAddVar(Lightning* self, const char* name, BCType bct, int32_t fpOffset, uint8_t isParam)
{
    Dbg_AddVar(&self->Dbg, self->CurrentFunctionIdx, name, Lightning_DbgBaseType(bct), fpOffset, isParam);
}

static inline void Lightning_Initialize(Lightning* self, uint32_t n_args, ...)
{
    RegisterStatus_Init(&self->Registers);
    self->Jit = jit_new_state();
    Dbg_RecorderInit(&self->Dbg);
}

static inline void Lightning_InitializeV(Lightning* self, uint32_t n_args, va_list args)
{
    assert(0);
}

static inline void Lightning_Finalize(Lightning* self)
{
    if (self->Finalized)
        return;
    self->Finalized = 1;

    void* base = jit_emit();
    for(uint32_t i = 1; i < self->nFunctions + 1; i++)
    {
        self->FunctionPtrs[i] = jit_address(self->FunctionLabels[i]);
    }
    // jit_clear_state();
    const char* debugPath = getenv("BC_LIGHTNING_DEBUG_ELF");
    if (debugPath)
        Lightning_WriteDebugElf(self, debugPath);
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
    self->LabelCount = 0;
    self->PendingJumpCount = 0;

    self->FunctionLabels[fnIdx] = jit_indirect();
    Dbg_BeginFunc(&self->Dbg, fnIdx, self->FunctionLabels[fnIdx]);
    jit_prolog();
    // self->getFrameSize(fd);
    //TODO implement a way to get the assumed frame size of a function
    jit_frame(256);
    self->RuntimeContextArg = jit_arg();
    self->ArgumentsArg = jit_arg();
    self->NumberOfArgumentsArg = jit_arg();

    jit_getarg(JIT_V0, self->RuntimeContextArg);
    jit_getarg(JIT_V1, self->ArgumentsArg);

    self->FrameSize = 4;
    // the last comparison's result, read back by EmitFlag
    self->FrameSize += 4;
    self->CondAddr = self->FrameSize;

    return fnIdx;
}

static inline void* Lightning_EndFunction(Lightning* self, uint32_t fnIdx)
{
    assert(self->InFunction);
    assert(self->CurrentFunctionIdx == fnIdx);
    self->InFunction = 0;
    jit_epilog();
    // marks the end of the function for its DWARF subprogram range
    Dbg_EndFunc(&self->Dbg, jit_note(NULL, 0));
    return self->FunctionLabels[fnIdx];
}

static inline BCValue Lightning_GenTemporary(Lightning* self, BCType bct)
{
    BCValue result = { BCValueType_Temporary };

    result.type = bct;
    result.temporaryIndex = ++self->TemporaryCount;

    // the slot spans [FP - addr, FP - addr + size), so reserve before taking the offset
    if (BCType_isBasicBCType(bct))
    {
        self->FrameSize += align4(BCTypeEnum_basicTypeSize(bct.type));
    }
    else
    {
        self->FrameSize += 4;
    }
    result.stackAddr.addr = self->FrameSize;

    return result;
}

static inline void Lightning_DestroyTemporary(Lightning* self, BCValue* tmp)
{
    //assert(0);
}

static inline BCValue Lightning_GenLocal(Lightning* self, BCType bct, const char* name)
{
    BCValue result = Lightning_GenTemporary(self, bct);
    result.vType = BCValueType_Local;
    result.name = name;
    Lightning_DbgAddVar(self, name, bct, (int32_t) result.stackAddr.addr, 0);
    return result;
}

static inline void Lightning_DestroyLocal(Lightning* self, BCValue* local)
{
}

static inline BCValue Lightning_GenParameter(Lightning* self, BCType bct, const char* name)
{
    BCValue p;

    p.type = bct;
    p.vType = BCValueType_Parameter;
    p.parameterIndex = ++self->ParametersCount;

    self->FrameSize += align4(BCTypeEnum_basicTypeSize(bct.type));
    p.stackAddr.addr = self->FrameSize;
    p.name = name;
    Lightning_DbgAddVar(self, name, bct, (int32_t) p.stackAddr.addr, 1);

    // arguments arrive as 8-byte words, parameterIndex is 1-based
    jit_ldxi_l(JIT_R0, JIT_V1, (p.parameterIndex - 1) * sizeof(int64_t));
    StoreRegValue(self, &p, r0Index);

    return p;
}

static inline BCValue Lightning_GenExternal(Lightning* self, BCType bct, const char* name)
{
    BCValue p;

    p.type = bct;
    p.vType = BCValueType_External;
    p.externalIndex = ++self->ExternalsCount;

    self->FrameSize += align4(sizeof(void*));
    p.stackAddr.addr = self->FrameSize;
    p.name = name;

    return p;
}


static uint32_t Runtime_MapExternal(RuntimeContext* ctx, void* memory, uint32_t sz)
{
    uint32_t result = 0;

    if (ctx->externalsCount < ctx->externalsCapacity)
    {
        uint32_t mapAddr = ctx->MapAddr;
        ctx->MapAddr += align4(sz);

        Lightning_External external = {
            memory, sz, mapAddr
        };

        ctx->externals[ctx->externalsCount++] = external;
        result = mapAddr;
    }
    else
    {
        assert(0);
    }

    return result;
}

static inline void Lightning_MapExternal (Lightning* self, BCValue* result,
                                          void* memory, uint32_t sz)
{
    jit_prepare();
    jit_pushargr(JIT_V0);
    jit_pushargi((intptr_t)result);
    jit_pushargi((intptr_t)memory);
    jit_pushargi(sz);
    jit_finishi((jit_pointer_t) Runtime_MapExternal);
    jit_retval(JIT_R0);
    StoreRegValue(self, result, r0Index);
}

static inline BCValue Lightning_GenExternalFunc(Lightning* self, BCType bct, const char* name)
{
    BCValue p;
    assert(bct.type == BCTypeEnum_Function);

    p.type = bct;
    p.vType = BCValueType_ExternalFunction;
    p.externalIndex = ++self->ExternalFuncsCount;

    self->FrameSize += 4;
    p.stackAddr.addr = self->FrameSize;
    p.name = name;

    return p;
}

static uint32_t Runtime_MapExternalFunc(RuntimeContext* ctx, void* funcP, uint32_t funcTypeIdx)
{
    uint32_t result = 0;

    if (ctx->externalFuncsCount < ctx->externalFuncsCapacity)
    {
        uint32_t funcIdx = ctx->externalFuncsCount++;

        uint32_t nParams;
        BCType* parameterTypes;
        BCType returnType;

        Lightning_ExternalFunc externalFunc = {
            funcP, parameterTypes, nParams, returnType, funcIdx
        };

        ctx->externalFuncs[funcIdx] = externalFunc;
        result = funcIdx;
    }
    else
    {
        assert(0);
    }

    return result;
}

// static uint32_t Runtime_RegisterFunctionType(RuntimeContext* ctx, )

static inline BCValue Lightning_MapExternalFunc(Lightning* self, BCValue* result, BCValue* funcP)
{
    jit_prepare();
    jit_pushargr(JIT_V0);
    LoadRegValue(self, r1Index, result);
    jit_pushargr(JIT_R1);
    LoadRegValue(self, r2Index, result);
    jit_pushargr(JIT_R2);
    jit_finishi((jit_pointer_t) Runtime_MapExternalFunc);
    jit_retval(JIT_R0);
    StoreRegValue(self, result, r0Index);
}

static inline void Lightning_EmitFlag(Lightning* self, BCValue* lhs)
{
    assert(BCValue_isStackValueOrParameter(lhs));
    jit_ldxi_i(JIT_R0, JIT_FP, -self->CondAddr);
    StoreRegValue(self, lhs, r0Index);
}

uint32_t Runtime_Alloc(RuntimeContext* ctx, uint32_t size)
{
    uint32_t result = 0;

    uint32_t heapSize = *ctx->heapSizeP;
    uint32_t heapCapacity = *ctx->heapCapacityP;

    if (heapSize + size < heapCapacity)
    {
        (*ctx->heapSizeP) = heapSize + size;
        result = heapSize;
    }

    return result;
}

static inline void Lightning_Alloc(Lightning* self, BCValue *heapPtr, const BCValue* size)
{
    jit_prepare();
    jit_pushargr(JIT_V0);
    LoadRegValue(self, r1Index, size);
    jit_pushargr(JIT_R1);
    jit_finishi(Runtime_Alloc);
    jit_retval(JIT_R0);
    StoreRegValue(self, heapPtr, r0Index);
}

static inline void Lightning_Assert(Lightning* self, const BCValue* value, const BCValue* err)
{
    assert(0);
}

#if 0
static inline void Lightning_PushArg(Lightning* self, const BCValue* value)
{
    assert(self->InCall);
    if (self->LastUsed <= JIT_R2)
    {
        switch (self->LastUsed)
        {
            case 0xffff:
                LoadRegValue(self, r0Index, value);
                self->LastUsed = JIT_R0;
            break;
            case JIT_R0:
                LoadRegValue(self, r1Index, value);
                self->LastUsed = JIT_R1;
            break;
            case JIT_R1:
                LoadRegValue(self, r2Index, value);
                self->LastUsed = JIT_R2;
            break;
        }
    }
}
#endif

static inline void Lightning_BeginCall(Lightning* self, const BCValue fn)
{

}

static inline void Runtime_MemCpy(RuntimeContext* ctx,
                                  uint32_t dst, uint32_t src, uint32_t size)
{
    memcpy(RuntimeContext_toRealPointer(ctx, dst),
           RuntimeContext_toRealPointer(ctx, src), size);
}

static inline void Lightning_MemCpy(Lightning* self,
                                    const BCValue* dst, const BCValue* src,
                                    const BCValue* size)
{
    jit_prepare();
    jit_pushargr(JIT_V0);

    LoadRegValue(self, r0Index, dst);
    jit_pushargr(JIT_R0);
    LoadRegValue(self, r1Index, src);
    jit_pushargr(JIT_R1);
    LoadRegValue(self, r2Index, size);
    jit_pushargr(JIT_R2);

    jit_finishi(Runtime_MemCpy);
}

static inline void Lightning_File(Lightning* self, const char* filename)
{
    self->currentFile = filename;
    Dbg_SetFile(&self->Dbg, filename);
}

static inline void Lightning_Line(Lightning* self, uint32_t line)
{
    self->currentLine = line;
    if (!Dbg_IsActive(&self->Dbg))
        return;
    Dbg_AddMark(&self->Dbg, jit_note(NULL, 0), line);
}

static inline void Lightning_Comment(Lightning* self, const char* comment)
{
}

void Runtime_Prt(RuntimeContext* ctx, uint64_t value, uint32_t isString)
{
    if (isString)
    {
        uint8_t* str = RuntimeContext_toRealPointer(ctx, (uint32_t)value);
        printf("%.*s", (int)str[0], (char*)(str + 1));
    }
    else
    {
        printf("%lld", (long long)value);
    }
}

static inline void Lightning_Prt(Lightning* self, const BCValue* value, bool isString)
{
    if (value->vType == BCValueType_Immediate)
    {
        jit_movi(JIT_R1, value->imm32.imm32);
    }
    else
    {
        LoadRegValue(self, r1Index, value);
    }

    jit_prepare();
    jit_pushargr(JIT_V0);
    jit_pushargr(JIT_R1);
    jit_pushargi(isString ? 1 : 0);
    jit_finishi((jit_pointer_t)Runtime_Prt);
}

static inline void Lightning_Set(Lightning* self, BCValue *lhs, const BCValue* rhs)
{
    if (rhs->vType == BCValueType_Immediate)
    {
        jit_movi(JIT_R1, Lightning_ImmediateBits(rhs));
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
                    jit_ler_u(JIT_R0, JIT_R0, JIT_R1);
                break;
                case HASH_LT:
                    jit_ltr(JIT_R0, JIT_R0, JIT_R1);
                break;
                case HASH_ULT:
                    jit_ltr_u(JIT_R0, JIT_R0, JIT_R1);
                break;

                case HASH_GE:
                    jit_ger(JIT_R0, JIT_R0, JIT_R1);
                break;
                case HASH_UGE:
                    jit_ger_u(JIT_R0, JIT_R0, JIT_R1);
                break;
                case HASH_GT:
                    jit_gtr(JIT_R0, JIT_R0, JIT_R1);
                break;
                case HASH_UGT:
                    jit_gtr_u(JIT_R0, JIT_R0, JIT_R1);
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

    jit_stxi_i(-self->CondAddr, JIT_FP, JIT_R0);
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

static inline void PrintfString(Lightning* self, const char* text)
{
    jit_prepare();
    jit_pushargi((intptr_t)text);
    jit_ellipsis();

    jit_finishi(printf);
}

static inline void PrintPointerR(Lightning* self, uint32_t line, jit_gpr_t reg)
{
    jit_prepare();
    jit_pushargi((intptr_t)"[%d] %p\n");
    jit_ellipsis();
    jit_pushargi((intptr_t) line);
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

static inline void Lightning_MathOp2(Lightning* self, BCValue *result,const BCValue* rhs, unsigned char op)
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
    // assert(result->type.type == lhs->type.type && lhs->type.type == rhs->type.type);
    if ((ConvertsToPointer(lhs->type.type) || BCType_IsInt32(lhs->type.type)) && BCType_IsInt32(rhs->type.type))
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
                case HASH_UMOD:
                    jit_remi_u(JIT_R0, JIT_R0, rhs->imm32.imm32);
                break;
                case HASH_LSH:
                    jit_lshi(JIT_R0, JIT_R0, rhs->imm32.imm32);
                break;
                case HASH_RSH:
                    jit_rshi(JIT_R0, JIT_R0, rhs->imm32.imm32);
                break;
                case HASH_AND:
                    jit_andi(JIT_R0, JIT_R0, rhs->imm32.imm32);
                break;
                case HASH_OR:
                    jit_ori(JIT_R0, JIT_R0, rhs->imm32.imm32);
                break;
                case HASH_XOR:
                    jit_xori(JIT_R0, JIT_R0, rhs->imm32.imm32);
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
                case HASH_UMOD:
                    jit_remr_u(JIT_R0, JIT_R0, JIT_R1);
                break;
                case HASH_LSH:
                    jit_lshr(JIT_R0, JIT_R0, JIT_R1);
                break;
                case HASH_RSH:
                    jit_rshr(JIT_R0, JIT_R0, JIT_R1);
                break;
                case HASH_AND:
                    jit_andr(JIT_R0, JIT_R0, JIT_R1);
                break;
                case HASH_OR:
                    jit_orr(JIT_R0, JIT_R0, JIT_R1);
                break;
                case HASH_XOR:
                    jit_xorr(JIT_R0, JIT_R0, JIT_R1);
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
    // assert(result->type.type == lhs->type.type && lhs->type.type == rhs->type.type);

    if ((ConvertsToPointer(lhs->type.type) || BCType_IsInt32(lhs->type.type)) && BCType_IsInt32(rhs->type.type))
    {
        Lightning_MathOp3(self, result, lhs, rhs, HASH_ADD);
    }
    else
        assert(0);

}

static inline void Lightning_Sub3(Lightning* self, BCValue *result, const BCValue* lhs, const BCValue* rhs)
{
    if ((ConvertsToPointer(lhs->type.type) || BCType_IsInt32(lhs->type.type)) && BCType_IsInt32(rhs->type.type))
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
    if (BCType_IsInt32(lhs->type.type) && BCType_IsInt32(rhs->type.type))
    {
        Lightning_MathOp3(self, result, lhs, rhs, HASH_UDIV);
    }
    else
        assert(0);
}

static inline void Lightning_And3(Lightning* self, BCValue *result, const BCValue* lhs, const BCValue* rhs)
{
    if (BCType_IsInt32(lhs->type.type) && BCType_IsInt32(rhs->type.type))
    {
        Lightning_MathOp3(self, result, lhs, rhs, HASH_AND);
    }
    else
        assert(0);
}

static inline void Lightning_Or3(Lightning* self, BCValue *result, const BCValue* lhs, const BCValue* rhs)
{
    if (BCType_IsInt32(lhs->type.type) && BCType_IsInt32(rhs->type.type))
    {
        Lightning_MathOp3(self, result, lhs, rhs, HASH_OR);
    }
    else
        assert(0);
}

static inline void Lightning_Xor3(Lightning* self, BCValue *result, const BCValue* lhs, const BCValue* rhs)
{
    if (BCType_IsInt32(lhs->type.type) && BCType_IsInt32(rhs->type.type))
    {
        Lightning_MathOp3(self, result, lhs, rhs, HASH_XOR);
    }
    else
        assert(0);
}

static inline void Lightning_Lsh3(Lightning* self, BCValue *result, const BCValue* lhs, const BCValue* rhs)
{
    if (BCType_IsInt32(lhs->type.type) && BCType_IsInt32(rhs->type.type))
    {
        Lightning_MathOp3(self, result, lhs, rhs, HASH_LSH);
    }
    else
        assert(0);
}

static inline void Lightning_Rsh3(Lightning* self, BCValue *result, const BCValue* lhs, const BCValue* rhs)
{
    if (BCType_IsInt32(lhs->type.type) && BCType_IsInt32(rhs->type.type))
    {
        Lightning_MathOp3(self, result, lhs, rhs, HASH_RSH);
    }
    else
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
    if (BCType_IsInt32(lhs->type.type) && BCType_IsInt32(rhs->type.type))
    {
        Lightning_MathOp3(self, result, lhs, rhs, HASH_UMOD);
    }
    else
        assert(0);
}

static inline void Lightning_Not(Lightning* self, BCValue *result, const BCValue* val)
{
    Lightning_MathOp2(self, result, val, HASH_COM);
}

static inline void Lightning_LoadFramePointer(Lightning* self, BCValue *result, const int32_t offset)
{
    // The interpreter emits this as a context manipulation that its executor does not handle, so it is a no-op here too.
}

static inline void Lightning_Call(Lightning* self, BCValue *result, const BCValue* fn, const BCValue* args, uint32_t n_args)
{
    // reserve the argument words in this frame; the block is [FP - base, FP - base + 8 * n_args)
    self->FrameSize += sizeof(int64_t) * n_args;
    const int32_t argBlockBase = self->FrameSize;

    jit_addi(JIT_R1, JIT_FP, -argBlockBase);
    for (uint32_t i = 0; i < n_args; i++)
    {
        if ((args + i)->vType == BCValueType_Immediate)
        {
            jit_movi(JIT_R0, Lightning_ImmediateBits(args + i));
        }
        else
        {
            LoadRegValue(self, r0Index, args + i);
        }
        jit_stxi_l(i * sizeof(int64_t), JIT_R1, JIT_R0);
    }

    // FunctionPtrs is 1-based and read at runtime so calls to functions emitted later still resolve
    if (fn->vType == BCValueType_Immediate)
    {
        jit_movi(JIT_R0, (jit_word_t) &self->FunctionPtrs[fn->imm32.imm32]);
        jit_ldr(JIT_R0, JIT_R0);
    }
    else
    {
        LoadRegValue(self, r0Index, fn);
        jit_lshi(JIT_R0, JIT_R0, 3);
        jit_addi(JIT_R0, JIT_R0, (jit_word_t) self->FunctionPtrs);
        jit_ldr(JIT_R0, JIT_R0);
    }

    jit_prepare();
    jit_pushargr(JIT_V0);
    jit_pushargr(JIT_R1);
    jit_pushargi(n_args);
    jit_finishr(JIT_R0);

    if (result)
    {
        jit_retval(JIT_R0);
        StoreRegValue(self, result, r0Index);
    }
}

static inline BCLabel Lightning_GenLabel(Lightning* self)
{
    assert(self->LabelCount < LIGHTNING_LABEL_CAPACITY);
    BCLabel label = {0};
    label.addr.addr = self->LabelCount;
    self->Labels[self->LabelCount++] = jit_note(NULL, 0);
    return label;
}

static inline void Lightning_Jmp(Lightning* self, BCLabel target)
{
    jit_patch_at(jit_jmpi(), self->Labels[target.addr.addr]);
}

static inline BCAddr Lightning_BeginJmp(Lightning* self)
{
    assert(self->PendingJumpCount < LIGHTNING_LABEL_CAPACITY);
    BCAddr at = {self->PendingJumpCount};
    self->PendingJumps[self->PendingJumpCount++] = jit_jmpi();
    return at;
}

static inline void Lightning_EndJmp(Lightning* self, BCAddr atIp, BCLabel target)
{
    jit_patch_at(self->PendingJumps[atIp.addr], self->Labels[target.addr.addr]);
}

static inline CndJmpBegin Lightning_BeginCndJmp(Lightning* self, const BCValue* cond, bool ifTrue)
{
    assert(self->PendingJumpCount < LIGHTNING_LABEL_CAPACITY);

    if (cond->vType == BCValueType_Immediate)
    {
        jit_movi(JIT_R0, cond->imm32.imm32);
    }
    else
    {
        LoadRegValue(self, r0Index, cond);
    }

    CndJmpBegin result = {0};
    result.at.addr = self->PendingJumpCount;
    result.cond = (BCValue*) cond;
    result.ifTrue = ifTrue;

    // jump when the condition is non-zero for ifTrue, zero otherwise
    self->PendingJumps[self->PendingJumpCount++] = ifTrue
        ? jit_bnei(JIT_R0, 0)
        : jit_beqi(JIT_R0, 0);

    return result;
}

static inline void Lightning_EndCndJmp(Lightning* self, const CndJmpBegin *jmp, BCLabel target)
{
    jit_patch_at(self->PendingJumps[jmp->at.addr], self->Labels[target.addr.addr]);
}

static inline uint64_t Runtime_Load(RuntimeContext* ctx, uint32_t unrealPtr,
                                    uint32_t sz)
{
    uint64_t value = 0;
    assert(sz == 1 || sz == 2 || sz == 4 || sz == 8);
    memcpy(&value, RuntimeContext_toRealPointer(ctx, unrealPtr), sz);
    return value;
}

static inline void Runtime_Store(RuntimeContext* ctx, uint32_t unrealPtr,
                                 uint64_t value, uint32_t sz)
{
    assert(sz == 1 || sz == 2 || sz == 4 || sz == 8);
    memcpy(RuntimeContext_toRealPointer(ctx, unrealPtr), &value, sz);
}

static inline void Lightning_Load(Lightning* self, BCValue *dest, const BCValue* from, uint32_t sz)
{

    assert(BCValue_isStackValueOrParameter(dest));

    if (from->vType == BCValueType_Immediate)
    {
        jit_movi(JIT_R2, from->imm32.imm32);
    }
    else
    {
        assert(BCValue_isStackValueOrParameter(from));
        jit_ldxi(JIT_R2, JIT_FP, -from->stackAddr.addr);
    }

    jit_prepare();
    jit_pushargr(JIT_V0);
    jit_pushargr(JIT_R2);
    jit_pushargi(sz);
    jit_finishi((jit_pointer_t)Runtime_Load);
    jit_retval(JIT_R0);
    StoreRegValue(self, dest, r0Index);
}

static inline void Lightning_Store(Lightning* self, BCValue *dest, const BCValue* value, uint32_t sz)
{
    if (dest->vType == BCValueType_Immediate)
    {
        jit_movi(JIT_R1, dest->imm32.imm32);
    }
    else
    {
        assert(BCValue_isStackValueOrParameter(dest));
        LoadRegValue(self, r1Index, dest);
    }

    if (value->vType == BCValueType_Immediate)
    {
        if (sz == 8)
            jit_movi(JIT_R0, value->imm64.imm64);
        else
            jit_movi(JIT_R0, value->imm32.imm32);
    }
    else
    {
        LoadRegValue(self, r0Index, value);
    }

    jit_prepare();
    jit_pushargr(JIT_V0);
    jit_pushargr(JIT_R1);
    jit_pushargr(JIT_R0);
    jit_pushargi(sz);
    jit_finishi((jit_pointer_t)Runtime_Store);
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
    Lightning_Load(self, dest, from, 8);
}

static inline void Lightning_Store64(Lightning* self, BCValue *dest, const BCValue* value)
{
    Lightning_Store(self, dest, value, 8);
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

// Values live in the frame; each conversion goes through JIT_F0 and writes its result back to the frame at once.
static inline void Lightning_IToF32(Lightning* self, BCValue *result, const BCValue* rhs)
{
    assert(BCValue_isStackValueOrParameter(result));
    assert(BCValue_isStackValueOrParameter(rhs));
    jit_ldxi_i(JIT_R0, JIT_FP, -rhs->stackAddr.addr);
    jit_extr_f(JIT_F0, JIT_R0);
    jit_stxi_f(-result->stackAddr.addr, JIT_FP, JIT_F0);
}

static inline void Lightning_IToF64(Lightning* self, BCValue *result, const BCValue* rhs)
{
    assert(BCValue_isStackValueOrParameter(result));
    assert(BCValue_isStackValueOrParameter(rhs));
    jit_ldxi_i(JIT_R0, JIT_FP, -rhs->stackAddr.addr);
    jit_extr_d(JIT_F0, JIT_R0);
    jit_stxi_d(-result->stackAddr.addr, JIT_FP, JIT_F0);
}

static inline void Lightning_F32ToI(Lightning* self, BCValue *result, const BCValue* rhs)
{
    assert(BCValue_isStackValueOrParameter(result));
    assert(BCValue_isStackValueOrParameter(rhs));
    jit_ldxi_f(JIT_F0, JIT_FP, -rhs->stackAddr.addr);
    jit_truncr_f_i(JIT_R0, JIT_F0);
    jit_stxi_i(-result->stackAddr.addr, JIT_FP, JIT_R0);
}

static inline void Lightning_F64ToI(Lightning* self, BCValue *result, const BCValue* rhs)
{
    assert(BCValue_isStackValueOrParameter(result));
    assert(BCValue_isStackValueOrParameter(rhs));
    jit_ldxi_d(JIT_F0, JIT_FP, -rhs->stackAddr.addr);
    jit_truncr_d_i(JIT_R0, JIT_F0);
    jit_stxi_i(-result->stackAddr.addr, JIT_FP, JIT_R0);
}

static inline void Lightning_F32ToF64(Lightning* self, BCValue *result, const BCValue* rhs)
{
    assert(BCValue_isStackValueOrParameter(result));
    assert(BCValue_isStackValueOrParameter(rhs));
    jit_ldxi_f(JIT_F0, JIT_FP, -rhs->stackAddr.addr);
    jit_extr_f_d(JIT_F0, JIT_F0);
    jit_stxi_d(-result->stackAddr.addr, JIT_FP, JIT_F0);
}

static inline void Lightning_F64ToF32(Lightning* self, BCValue *result, const BCValue* rhs)
{
    assert(BCValue_isStackValueOrParameter(result));
    assert(BCValue_isStackValueOrParameter(rhs));
    jit_ldxi_d(JIT_F0, JIT_FP, -rhs->stackAddr.addr);
    jit_extr_d_f(JIT_F0, JIT_F0);
    jit_stxi_f(-result->stackAddr.addr, JIT_FP, JIT_F0);
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

    RuntimeContext rtContext = {0};


    BCValue result = {BCValueType_Unknown};
    char parameterString[64];

    void* marshaled_args = self->allocMemory(self->allocCtx, sizeof(double) * n_args, 0);

    {
        rtContext.instance = self;
        rtContext.heapDataBegin = heap ? heap->heapData : 0;
        rtContext.heapCapacityP = heap ? &heap->heapMax : 0;
        rtContext.heapSizeP = heap ? &heap->heapSize : 0;

        rtContext.externals = self->allocMemory(self->allocCtx, sizeof(Lightning_External) * 8, 0);
        rtContext.externalsCount = 0;
        rtContext.externalsCapacity = 8;

        rtContext.MapAddr = AddressFrom(AddressKind_External, 0);
    }

    for(uint32_t i = 0; i < n_args; i++)
    {
        assert((args + i)->vType == BCValueType_Immediate);
        ((int64_t*) marshaled_args)[i] = Lightning_ImmediateBits(args + i);
    }
    // jit_disassemble();  // Skip disassembly to avoid BFD dependency
    fnResult = self->FunctionPtrs[fnIdx](&rtContext, marshaled_args, n_args);
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
                          sizeof(BCValue) * INITIAL_EXTERNAL_CAPACITY, 0);
    self->ExternalsCapacity = INITIAL_EXTERNAL_CAPACITY;
    self->ExternalsCount = 0;

}

static inline void Lightning_fini_instance(Lightning* self)
{
    Dbg_RecorderFree(&self->Dbg);

    jit_destroy_state();

}

void Runtime_ReadI32(RuntimeContext* ctx, uint32_t cbIdx, int32_t value)
{
    Lightning* instance = ctx->instance;
    instance->ReadI32Callbacks[cbIdx](value, instance->ReadI32UserCtxs[cbIdx]);
}

static inline void Lightning_ReadI32(Lightning* self, const BCValue* val, const ReadI32_cb_t readCb, void* userCtx)
{
    assert(self->ReadI32Count < LIGHTNING_READI32_CAPACITY);
    uint32_t cbIdx = self->ReadI32Count++;
    self->ReadI32Callbacks[cbIdx] = readCb;
    self->ReadI32UserCtxs[cbIdx] = userCtx;

    if (val->vType == BCValueType_Immediate)
    {
        jit_movi(JIT_R1, val->imm32.imm32);
    }
    else
    {
        LoadRegValue(self, r1Index, val);
    }

    jit_prepare();
    jit_pushargr(JIT_V0);
    jit_pushargi(cbIdx);
    jit_pushargr(JIT_R1);
    jit_finishi((jit_pointer_t)Runtime_ReadI32);
}

static inline void Lightning_set_alloc_memory(Lightning* self, alloc_fn_t alloc_fn, void* userCtx)
{
    self->allocMemory = alloc_fn;
    self->allocCtx = userCtx;
}


static inline void Lightning_set_get_typeinfo(Lightning* self, get_typeinfo_fn_t get_typeinfo_fn, void* userCtx)
{
    self->getTypeInfo = get_typeinfo_fn;
    self->getTypeInfoCtx = userCtx;
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
    /*.GenExternal = */(GenExternal_t) Lightning_GenExternal,
    /*.MapExternal = */(MapExternal_t) Lightning_MapExternal,
    /*.GenExternalFunc = */(GenExternalFunc_t) Lightning_GenExternalFunc,
    /*.MapExternalFunc = */(MapExternalFunc_t) Lightning_MapExternalFunc,
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
    /*.ReadString = */0,
};
