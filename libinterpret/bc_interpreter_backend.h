/*
 * Written By Stefan Koch in 2016 - 2022
*/
#ifndef _BC_INTERPRETER_BACKEND_H_
#define _BC_INTERPRETER_BACKEND_H_

typedef struct RetainedCall
{
    BCValue fn;
    BCValue* args;
    uint32_t n_args;

    uint32_t callerId;
    BCAddr callerIp;
    StackAddr callerSp;
} RetainedCall;

#define IS_CMP_INST(INST) \
    (((INST) >= FIRST_REG_CMP & (INST) <= LAST_REG_CMP) || \
     ((INST) >= FIRST_IMM_CMP & (INST) <= LAST_IMM_CMP) || \
     ((INST) >= FIRST_F32_CMP & (INST) <= LAST_F32_CMP) || \
     ((INST) >= FIRST_F64_CMP & (INST) <= LAST_F64_CMP))


typedef enum LongInst
{
    LongInst_PrintValue,
    LongInst_RelJmp,
    LongInst_Ret32,
    LongInst_Ret64,
    LongInst_RetS32,
    LongInst_RetS64,
    LongInst_Not,

    LongInst_Flg, // writes the conditionFlag into [lw >> 16]
    //End Former ShortInst

    LongInst_Jmp,
    LongInst_JmpFalse,
    LongInst_JmpTrue,
    LongInst_JmpZ,
    LongInst_JmpNZ,

    LongInst_PushCatch,
    LongInst_PopCatch,
    LongInst_Throw,

    // 2 StackOperands
    LongInst_Add,
    LongInst_Sub,
    LongInst_Div,
    LongInst_Mul,
    LongInst_Mod,
#define FIRST_REG_CMP LongInst_Eq
    LongInst_Eq, //sets condflags
    LongInst_Neq, //sets condflag
    LongInst_Lt, //sets condflags
    LongInst_Le,
    LongInst_Gt, //sets condflags
    LongInst_Ge,
    LongInst_Ult,
    LongInst_Ule,
    LongInst_Ugt,
    LongInst_Uge,
#define LAST_REG_CMP LongInst_Uge
    LongInst_Udiv,
    LongInst_Umod,
    LongInst_And,
    LongInst_And32,
    LongInst_Or,
    LongInst_Xor,
    LongInst_Xor32,
    LongInst_Lsh,
    LongInst_Rsh,
    LongInst_Set,

    LongInst_Memcmp,
    LongInst_Assert,

    // immediate operand
    LongInst_ImmAdd,
    LongInst_ImmSub,
    LongInst_ImmDiv,
    LongInst_ImmMul,
    LongInst_ImmMod,
#define FIRST_IMM_CMP LongInst_ImmEq
    LongInst_ImmEq,
    LongInst_ImmNeq,
    LongInst_ImmLt,
    LongInst_ImmLe,
    LongInst_ImmGt,
    LongInst_ImmGe,
    LongInst_ImmUlt,
    LongInst_ImmUle,
    LongInst_ImmUgt,
    LongInst_ImmUge,
#define LAST_IMM_CMP LongInst_ImmUge
    LongInst_ImmUdiv,
    LongInst_ImmUmod,
    LongInst_ImmAnd,
    LongInst_ImmAnd32,
    LongInst_ImmOr,
    LongInst_ImmXor,
    LongInst_ImmXor32,
    LongInst_ImmLsh,
    LongInst_ImmRsh,

#define FLT32_BEGIN LongInst_FAdd32
    LongInst_FAdd32,
    LongInst_FSub32,
    LongInst_FDiv32,
    LongInst_FMul32,
#ifdef WITH_FMOD
    LongInst_FMod32,
#endif
#define FIRST_F32_CMP LongInst_FEq32
    LongInst_FEq32,
    LongInst_FNeq32,
    LongInst_FLt32,
    LongInst_FLe32,
    LongInst_FGt32,
    LongInst_FGe32,
#define LAST_F32_CMP LongInst_FGe32
#define FLT32_END LongInst_FGe32

    LongInst_F32ToF64,
    LongInst_F32ToI,
    LongInst_IToF32,

#define FLT64_BEGIN LongInst_FAdd32
    LongInst_FAdd64,
    LongInst_FSub64,
    LongInst_FDiv64,
    LongInst_FMul64,
#ifdef WITH_FMOD
    LongInst_FMod64,
#endif
#define FIRST_F64_CMP LongInst_FEq64
    LongInst_FEq64,
    LongInst_FNeq64,
    LongInst_FLt64,
    LongInst_FLe64,
    LongInst_FGt64,
    LongInst_FGe64,
#define LAST_F64_CMP LongInst_FGe64
#define FLT64_END LongInst_FGe64

    LongInst_F64ToF32,
    LongInst_F64ToI,
    LongInst_IToF64,

    LongInst_SetHighImm32,
    LongInst_SetImm32,
    LongInst_SetImm8,

    LongInst_ContextManip,
    LongInst_Call,

#define HEAP_LOAD_BEGIN LongInst_HeapLoad8
    LongInst_HeapLoad8,
    LongInst_HeapLoad16,
    LongInst_HeapLoad32,
    LongInst_HeapLoad64,
#define HEAP_LOAD_END LongInst_HeapLoad64

#define HEAP_STORE_BEGIN LongInst_HeapStore8
    LongInst_HeapStore8,
    LongInst_HeapStore32, ///Heap[align4(SP[hi & 0xFFFF)] = SP[hi >> 16]]
    LongInst_HeapStore16,
    LongInst_HeapStore64,
#define HEAP_STORE_END LongInst_HeapStore64

    LongInst_SetMode,

    LongInst_Alloc, /// SP[hi & 0xFFFF] = heapSize; heapSize += SP[hi >> 16]
    LongInst_MapExternal,
    LongInst_MapExternalFunc,
    LongInst_MemCpy,
    LongInst_Realloc,

    LongInst_BuiltinCall, // call a builtin.
    LongInst_Comment,
    LongInst_Line,
    LongInst_File,

    LongInst_ReadI32,

    LongInst_max
} LongInst;

#define INSTMASK 0x7F


#include "bc_common.h"
#include "backend_interface_funcs.h"

typedef enum BCFunctionTypeEnum
{
    BCFunctionTypeEnum_Undef,
    BCFunctionTypeEnum_Builtin,
    BCFunctionTypeEnum_Bytecode,
    BCFunctionTypeEnum_Compiled,
} BCFunctionTypeEnum;

typedef struct BCFunction
{
    void* funcDecl;
    uint32_t fn;

    BCFunctionTypeEnum type;
    uint16_t nArgs;
    uint16_t maxStackUsed;

    uint32_t bytecode_start; // should be const but currently we need to assign to this;
    uint32_t bytecode_end;
} BCFunction;

typedef struct BCGen
{
    uint32_t* byteCodeArrayExtra;
    uint32_t byteCodeCount;
    uint32_t byteCodeExtraCapacity;

    /// ip starts at 4 because 0 should be an invalid address;
    uint32_t ip;
    uint32_t sp;

    uint8_t parameterCount;
    uint16_t temporaryCount;
    uint16_t externalCount;

    BCFunction* functions;
    uint32_t functionCount;
    uint32_t functionCapacity;

    uint32_t functionIdx;
    void* fd;
    bool insideFunction;

    BCLocal* locals;
    uint32_t localCount;
    uint32_t localCapacity;

    RetainedCall* calls;
    uint32_t callsCount;
    uint32_t callsCapacity;

    ReadI32_ctx_t* contexts;
    uint32_t contextCount;
    uint32_t contextCapacity;

    alloc_fn_t allocFn;
    void* allocCtx;

    get_typeinfo_fn_t getTypeInfoFn;
    void* getTypeInfoCtx;

    bool finalized;
    uint32_t byteCodeArray[512];
} BCGen;

typedef struct byte_code_array_t
{
    int32_t* Code;
    uint32_t Count;
} byte_code_array_t;

byte_code_array_t* BCGen_Code(BCGen* self);

#endif
