/**
 * Written By Stefan Koch in 2016 - 2022
*/
#ifndef _BC_INTERPRETER_C_
#define _BC_INTERPRETER_C_

#include <assert.h>
#include <math.h>
#include <stdlib.h>
#include <string.h>

#include "bc_common.h"
#include "backend_interface_funcs.h"
#include "bc_interpreter_backend.h"

#ifndef cast
#define cast(T) (T)
#endif

#ifndef NDEBUG
#  define DEBUG(...) __VA_ARGS__
#else
#  define DEBUG(...)
#endif

static int16_t BCGen_isShortJump(const int32_t offset)
{
    assert(offset != 0);//, "A Jump to the Jump itself is invalid");

    const bool wasNegative = (offset < 0);
    int abs_offset = wasNegative ? offset * -1 : offset;

    if (abs_offset < (1 << 15))
    {
        return (cast(uint16_t)(wasNegative ? abs_offset *= -1 : abs_offset));
    }
    else
    {
        return 0;
    }
}

static inline uint32_t BCGen_ShortInst16(const LongInst i, const uint16_t _imm)
{
    int16_t imm = (int16_t) _imm;
    return (i | imm << 16);
}

static inline uint32_t BCGen_ShortInst16Ex(const LongInst i, uint8_t ex, uint16_t imm)
{
    return i | ex << 8 | imm << 16;
}

static const int max_call_depth = 2000;

#define INITIAL_LOCALS_CAPACITY 2048
#define INITIAL_CALLS_CAPACITY 2048

static inline void BCGen_set_alloc_memory(BCGen* self, alloc_fn_t alloc_fn, void* userCtx)
{
    self->allocFn = alloc_fn;
    self->allocCtx = userCtx;
}

static inline void BCGen_Init(BCGen* self)
{
    if (!self->allocFn)
    {
        self->allocFn = alloc_with_malloc;
    }

    self->byteCodeArrayExtra = 0;
    self->byteCodeCount = 0;
    self->byteCodeExtraCapacity = 0;

    self->locals = (BCLocal*) self->allocFn(self->allocCtx,
        sizeof(BCLocal) * INITIAL_LOCALS_CAPACITY, 0);
    self->localCount = 0;
    self->localCapacity = INITIAL_LOCALS_CAPACITY;

    self->parameterCount = 0;
    self->temporaryCount = 0;

    self->ip = 4;
    self->sp = 4;

    self->insideFunction = 0;
    self->functionIdx = 0;

    self->calls = (RetainedCall*) self->allocFn(self->allocCtx,
        sizeof(RetainedCall) * INITIAL_CALLS_CAPACITY, 0);
    self->callCount = 0;
    self->callCapacity = INITIAL_CALLS_CAPACITY;

    self->functions = (BCFunction*)self->allocFn(self->allocCtx,
        sizeof(BCFunction) * INITIAL_LOCALS_CAPACITY, 0);
    self->functionCount = 0;
    self->functionCapacity = INITIAL_LOCALS_CAPACITY;

    self->contexts = (ReadI32_ctx_t*) self->allocFn(self->allocCtx,
        sizeof(ReadI32_ctx_t) * 64, 0);
    self->contextCount = 0;
    self->contextCapacity = 64;

    self->finalized = false;
}

static inline void BCGen_Fini(BCGen* self)
{
    self->allocFn(self->allocCtx, FREE_SIZE, self->locals);
    self->allocFn(self->allocCtx, FREE_SIZE, self->calls);
    self->allocFn(self->allocCtx, FREE_SIZE, self->functions);
    self->allocFn(self->allocCtx, FREE_SIZE, self->contexts);
}

typedef enum CtxM {
    CtxM_Undef,

    CtxM_GetFramePointer,
    CtxM_SetFramePointer,

    CtxM_AddframePointer,
    CtxM_SubframePointer,

    CtxM_max
} CtxM;

static const char* CtxM_strings[] = {
    "Invalid (Undef)"
  , "GetFramePointer"
  , "SetFramePointer"
  , "AddframePointer"
  , "SubframePointer"
  , "Invalid (max)"
};

const char* CtxM_toChars(CtxM manip)
{
    const char* result =  "Invalid (greater than max)";

    if ((int) manip < CtxM_max)
        result = CtxM_strings[manip];

    return result;
}

void BCGen_new_instance(BCGen** pResult)
{
    BCGen* gen = (BCGen*) malloc(sizeof(BCGen));
    BCGen_Init(gen);
    *pResult = gen;

    return ;
}

void BCGen_clear_instance(BCGen* instance)
{
    instance->allocFn = 0;
}
void BCGen_init_instance(BCGen* instance)
{
    BCGen_Init(instance);
}

uint32_t BCGen_sizeof_instance(void)
{
    return sizeof(BCGen);
}

void BCGen_fini_instance(BCGen* instance)
{
    BCGen_Fini(instance);
}

typedef struct Catch
{
    uint32_t ip;
    uint32_t stackDepth;
} Catch;

typedef struct ReturnAddr
{
    uint32_t ip;
    uint32_t fnId;
    int64_t* fp;
    int64_t* retval;
} ReturnAddr;

#define MAX_CALL_DEPTH 2000
#define LOCAL_STACK_SIZE 2048

typedef struct BCInterpreter {
    int64_t* fp;
    int64_t* sp;
    uint32_t ip;
    uint32_t mode;

    uint32_t n_return_addrs;

    uint32_t callDepth;
    uint32_t fnIdx;

    uint32_t stackTop;

    uint32_t* stackExtra;
    uint32_t stackExtraCapacity;

    uint32_t lastLine;

    BCValue cRetval;

    int64_t stack[LOCAL_STACK_SIZE];
    ReturnAddr returnAddrs[MAX_CALL_DEPTH];
} BCInterpreter;

bool BCInterpreter_Return(BCInterpreter* self)
{
    if (self->n_return_addrs)
    {
        ReturnAddr returnAddr = self->returnAddrs[--self->n_return_addrs];
        self->fnIdx = returnAddr.fnId;
        self->ip = returnAddr.ip;

        self->fp = returnAddr.fp;
        self->callDepth--;
        BCValue cRetval = self->cRetval;

        if (cRetval.vType == BCValueType_Exception)
        {
            assert(!"Execptions are currently unhandeld");
            //return HandleExp();
        }
        if (cRetval.vType == BCValueType_Error || cRetval.vType == BCValueType_Bailout)
        {
            return true;
        }
        if (cRetval.type.type == BCTypeEnum_i64 || cRetval.type.type == BCTypeEnum_u64 || cRetval.type.type == BCTypeEnum_f52)
        {
            (*returnAddr.retval) = cRetval.imm64.imm64;
        }
        else
        {
            (*returnAddr.retval) = cRetval.imm32.imm32;
        }
        return false;
    }
    else
    {
        return true;
    }

    assert(0);
    return true;
}

byte_code_array_t BCGen_CodeBytes(BCGen* self)
{
    byte_code_array_t result;

    result.Count = self->byteCodeCount;
    result.Code = (int32_t*)self->byteCodeArray;

    return result;
}

#ifdef PRINT_CODE
void BCGen_PrintCode(BCGen* self, uint32_t start, uint32_t end)
{
    uint32_t* codeP = self->byteCodeArray;

    if (self->byteCodeCount > ARRAY_SIZE(self->byteCodeArray))
    {
        codeP = self->byteCodeArrayExtra;
    }

    uint32_t ip = 0;

    uint32_t lw;
    uint32_t hi;
    while (ip < end)
    {
        printf("%d: ", ip);
        lw = codeP[ip++];
        hi = codeP[ip++];

        const int32_t imm32c = (int32_t) hi;

        const uint8_t  opSpecial   = ((lw >> 8) & 0xFF);
        const uint32_t opRefOffset = (lw >> 16) & 0xFFFF;
        const uint32_t lhsOffset   = hi & 0xFFFF;
        const uint32_t rhsOffset   = (hi >> 16) & 0xFFFF;

        if (!lw)
        {
            printf("NOP NOP\n");
            continue;
        }

        switch (cast(LongInst)(lw & INSTMASK))
        {
        case LongInst_ImmAdd:
            {
                printf("LongInst_ImmAdd R[%d] += %d\n", opRefOffset / 4, imm32c);
            }
            break;
        case LongInst_ImmSub:
            {
                printf("LongInst_ImmSub R[%d] -= %d\n", opRefOffset / 4, imm32c);
            }
            break;
        case LongInst_ImmMul:
            {
                printf("LongInst_ImmMul R[%d] *= %d\n", opRefOffset / 4, imm32c);
            }
            break;
        case LongInst_ImmDiv:
            {
                printf("LongInst_ImmDiv R[%d] /= %d\n", opRefOffset / 4, imm32c);
            }
            break;
        case LongInst_ImmUdiv:
            {
                printf("LongInst_ImmUdiv R[%d] /= %d\n", opRefOffset / 4, imm32c);
            }
            break;

        case LongInst_ImmAnd:
            {
                printf("LongInst_ImmAnd R[%d] &= %u\n", opRefOffset / 4, hi);
            }
            break;
        case LongInst_ImmAnd32:
            {
                printf("LongInst_ImmAnd32 R[%d] &= %u\n", opRefOffset / 4, hi);
            }
            break;
        case LongInst_ImmOr:
            {
                printf("LongInst_ImmOr R[%d] |= %u\n", opRefOffset / 4, hi);
            }
            break;
        case LongInst_ImmXor:
            {
                printf("LongInst_ImmXor R[%d] ^= %u\n", opRefOffset / 4, hi);
            }
            break;
        case LongInst_ImmXor32:
            {
                printf("LongInst_ImmXor32 R[%d] ^= %u\n", opRefOffset / 4, hi);
            }
            break;
        case LongInst_ImmLsh:
            {
                printf("LongInst_ImmLsh R[%d] <<= %u\n", opRefOffset / 4, hi);
            }
            break;
        case LongInst_ImmRsh:
            {
                printf("LongInst_ImmRsh R[%d] >>= %u\n", opRefOffset / 4, hi);
            }
            break;
        case LongInst_ImmMod:
            {
                printf("LongInst_ImmMod R[%d] %%= %d\n", opRefOffset / 4, imm32c);
            }
            break;
        case LongInst_ImmUmod:
            {
                printf("LongInst_ImmUmod R[%d] %%= %u\n", opRefOffset / 4, hi);
            }
            break;
        case LongInst_SetImm8:
            {
                printf("LongInst_SetImm8 R[%d] = %u\n", opRefOffset / 4, hi);
                assert(hi <= UINT8_MAX);
            }
            break;
        case LongInst_SetImm32:
            {
                printf("LongInst_SetImm32 R[%d] = %u\n", opRefOffset / 4, hi);
            }
            break;
        case LongInst_SetHighImm32:
            {
                printf("LongInst_SetImm32High R[%d] |= (%u << 32)\n", opRefOffset / 4, hi);
            }
            break;
        case LongInst_ImmEq:
            {
                printf("LongInst_ImmEq R[%d] == %d\n", opRefOffset / 4, imm32c);
            }
            break;
        case LongInst_ImmNeq:
            {
                printf("LongInst_ImmNeq R[%d] != %d\n", opRefOffset / 4, imm32c);
            }
            break;
        case LongInst_ImmUlt:
            {
                printf("LongInst_ImmUlt R[%d] < %u\n", opRefOffset / 4, hi);
            }
            break;
        case LongInst_ImmUgt:
            {
                printf("LongInst_ImmUlt R[%d] > %u\n", opRefOffset / 4, hi);
            }
            break;
        case LongInst_ImmUle:
            {
                printf("LongInst_ImmUle R[%d] <= %u\n", opRefOffset / 4, hi);
            }
            break;
        case LongInst_ImmUge:
            {
                printf("LongInst_ImmUge R[%d] >= %u\n", opRefOffset / 4, hi);
            }
            break;
        case LongInst_ImmLt:
            {
                printf("LongInst_ImmLt R[%d] < %d\n", opRefOffset / 4, imm32c);
            }
            break;
        case LongInst_ImmGt:
            {
                printf("LongInst_ImmGt R[%d] > %d\n", opRefOffset / 4, imm32c);
            }
            break;
        case LongInst_ImmLe:
            {
                printf("LongInst_ImmLe R[%d] <= %d\n", opRefOffset / 4, imm32c);
            }
            break;
        case LongInst_ImmGe:
            {
                printf("LongInst_ImmGe R[%d] >= %d\n", opRefOffset / 4, imm32c);
            }
            break;
        case LongInst_Add:
            {
                printf("LongInst_Add R[%d] += R[%d]\n", lhsOffset / 4, rhsOffset / 4);
            }
            break;
        case LongInst_Sub:
            {
                printf("LongInst_Sub R[%d] -= R[%d]\n", lhsOffset / 4, rhsOffset / 4);
            }
            break;
        case LongInst_Mul:
            {
                printf("LongInst_Mul R[%d] *= R[%d]\n", lhsOffset / 4, rhsOffset / 4);
            }
            break;
        case LongInst_Div:
            {
                printf("LongInst_Div R[%d] /= R[%d]\n", lhsOffset / 4, rhsOffset / 4);
            }
            break;
        case LongInst_Udiv:
            {
                printf("LongInst_Udiv R[%d] /= R[%d]\n", lhsOffset / 4, rhsOffset / 4);
            }
            break;
        case LongInst_And:
            {
                printf("LongInst_And R[%d] &= R[%d]\n", lhsOffset / 4, rhsOffset / 4);
            }
            break;
        case LongInst_And32:
            {
                printf("LongInst_And32 R[%d] &= R[%d]\n", lhsOffset / 4, rhsOffset / 4);
            }
            break;
        case LongInst_Or:
            {
                printf("LongInst_Or R[%d] |= R[%d]\n", lhsOffset / 4, rhsOffset / 4);
            }
            break;
        case LongInst_Xor32:
            {
                printf("LongInst_Xor32 R[%d] ^= R[%d]\n", lhsOffset / 4, rhsOffset / 4);
            }
            break;
        case LongInst_Xor:
            {
                printf("LongInst_Xor R[%d] ^= R[%d]\n", lhsOffset / 4, rhsOffset / 4);
            }
            break;
        case LongInst_Lsh:
            {
                printf("LongInst_Lsh R[%d] <<= R[%d]\n", lhsOffset / 4, rhsOffset / 4);
            }
            break;
        case LongInst_Rsh:
            {
                printf("LongInst_Rsh R[%d] >>= R[%d]\n", lhsOffset / 4, rhsOffset / 4);
            }
            break;
        case LongInst_Mod:
            {
                printf("LongInst_Mod R[%d] %%= R[%d]\n", lhsOffset / 4, rhsOffset / 4);
            }
            break;
        case LongInst_Umod:
            {
                printf("LongInst_Umod R[%d] %%= R[%d]\n", lhsOffset / 4, rhsOffset / 4);
            }
            break;
        case LongInst_FGt32 :
            {
                printf("LongInst_FGt32 R[%d] > R[%d]\n", lhsOffset / 4, rhsOffset / 4);
            }
            break;
        case LongInst_FGe32 :
            {
                printf("LongInst_FGe32 R[%d] >= R[%d]\n", lhsOffset / 4, rhsOffset / 4);
            }
            break;
        case LongInst_FEq32 :
            {
                printf("LongInst_FEq32 R[%d] == R[%d]\n", lhsOffset / 4, rhsOffset / 4);
            }
            break;
        case LongInst_FNeq32 :
            {
                printf("LongInst_FNeq32 R[%d] != R[%d]\n", lhsOffset / 4, rhsOffset / 4);
            }
            break;
        case LongInst_FLt32 :
            {
                printf("LongInst_FLt32 R[%d] < R[%d]\n", lhsOffset / 4, rhsOffset / 4);
            }
            break;
        case LongInst_FLe32 :
            {
                printf("LongInst_FLe32 R[%d] <= R[%d]\n", lhsOffset / 4, rhsOffset / 4);
            }
            break;
        case LongInst_F32ToF64 :
            {
                printf("LongInst_F64To32 R[%d] = cast(double) R[%d]\n", lhsOffset / 4, rhsOffset / 4);
            }
            break;
        case LongInst_F32ToI :
            {
                printf("LongInst_FToI32 R[%d] = cast(int) R[%d]\n", lhsOffset / 4, rhsOffset / 4);
            }
            break;
        case LongInst_IToF32 :
            {
                printf("LongInst_IToF32 R[%d] = cast(float) R[%d]\n", lhsOffset / 4, rhsOffset / 4);
            }
            break;

        case LongInst_FAdd32:
            {
                printf("LongInst_FAdd32 R[%d] += R[%d]\n", lhsOffset / 4, rhsOffset / 4);
            }
            break;
        case LongInst_FSub32:
            {
                printf("LongInst_FSub32 R[%d] -= R[%d]\n", lhsOffset / 4, rhsOffset / 4);
            }
            break;
        case LongInst_FMul32:
            {
                printf("LongInst_FMul32 R[%d] *= R[%d]\n", lhsOffset / 4, rhsOffset / 4);
            }
            break;
        case LongInst_FDiv32:
            {
                printf("LongInst_FDiv32 R[%d] /= R[%d]\n", lhsOffset / 4, rhsOffset / 4);
            }
            break;
#ifdef WITH_FMOD
        case LongInst_FMod32:
            {
                printf("LongInst_FMod32 R[%d] %%= R[%d]\n", lhsOffset / 4, rhsOffset / 4);
            }
            break;
#endif
        case LongInst_FEq64 :
            {
                printf("LongInst_FEq64 R[%d] == R[%d]\n", lhsOffset / 4, rhsOffset / 4);
            }
            break;
        case LongInst_FNeq64 :
            {
                printf("LongInst_FNeq64 R[%d] != R[%d]\n", lhsOffset / 4, rhsOffset / 4);
            }
            break;
        case LongInst_FLt64 :
            {
                printf("LongInst_FLt64 R[%d] < R[%d]\n", lhsOffset / 4, rhsOffset / 4);
            }
            break;
        case LongInst_FLe64 :
            {
                printf("LongInst_FLe64 R[%d] <= R[%d]\n", lhsOffset / 4, rhsOffset / 4);
            }
            break;
        case LongInst_FGt64 :
            {
                printf("LongInst_FGt64 R[%d] > R[%d]\n", lhsOffset / 4, rhsOffset / 4);
            }
            break;
        case LongInst_FGe64 :
            {
                printf("LongInst_FGe64 R[%d] >= R[%d]\n", lhsOffset / 4, rhsOffset / 4);
            }
            break;
        case LongInst_F64ToF32 :
            {
                printf("LongInst_F64ToF32 R[%d] = cast(float) R[%d]\n", lhsOffset / 4, rhsOffset / 4);
            }
            break;
        case LongInst_F64ToI :
            {
                printf("LongInst_F64ToI R[%d] = cast(int64_t) R[%d]\n", lhsOffset / 4, rhsOffset / 4);
            }
            break;
        case LongInst_IToF64 :
            {
                printf("LongInst_IToF64 R[%d] = cast(double) R[%d]\n", lhsOffset / 4, rhsOffset / 4);
            }
            break;
        case LongInst_FAdd64:
            {
                printf("LongInst_FAdd64 R[%d] == R[%d]\n", lhsOffset / 4, rhsOffset / 4);
            }
            break;
        case LongInst_FSub64:
            {
                printf("LongInst_FSub64 R[%d] -= R[%d]\n", lhsOffset / 4, rhsOffset / 4);
            }
            break;
        case LongInst_FMul64:
            {
                printf("LongInst_FMul64 R[%d] *= R[%d]\n", lhsOffset / 4, rhsOffset / 4);
            }
            break;
        case LongInst_FDiv64:
            {
                printf("LongInst_FDiv64 R[%d] /= R[%d]\n", lhsOffset / 4, rhsOffset / 4);
            }
            break;
#ifdef WITH_FMOD
        case LongInst_FMod64:
            {
                printf("LongInst_FMod64 R[%d] %%= R[%d]\n", lhsOffset / 4, rhsOffset / 4);
            }
            break;
#endif
        case LongInst_Assert:
            {
                printf("LongInst_Assert(R[%d])\n", opRefOffset / 4);
            }
            break;
        case LongInst_Eq:
            {
                printf("LongInst_Eq R[%d] == R[%d]\n", lhsOffset / 4, rhsOffset / 4);
            }
            break;

        case LongInst_Neq:
            {
                printf("LongInst_Neq R[%d] == R[%d]\n", lhsOffset / 4, rhsOffset / 4);
            }
            break;
        case LongInst_Set:
            {
                printf("LongInst_Set R[%d] = R[%d]\n", lhsOffset / 4, rhsOffset / 4);
            }
            break;

        case LongInst_Ult:
            {
                printf("LongInst_Ult R[%d] < R[%d]\n", lhsOffset / 4, rhsOffset / 4);
            }
            break;
        case LongInst_Ugt:
            {
                printf("LongInst_Ngt R[%d] > R[%d]\n", lhsOffset / 4, rhsOffset / 4);
            }
            break;
        case LongInst_Ule:
            {
                printf("LongInst_Ule R[%d] <= R[%d]\n", lhsOffset / 4, rhsOffset / 4);
            }
            break;
        case LongInst_Uge:
            {
                printf("LongInst_Uge R[%d] >= R[%d]\n", lhsOffset / 4, rhsOffset / 4);
            }
            break;
        case LongInst_Lt:
            {
                printf("LongInst_Neq R[%d] < R[%d]\n", lhsOffset / 4, rhsOffset / 4);
            }
            break;
        case LongInst_Gt:
            {
                printf("LongInst_Gt R[%d] > R[%d]\n", lhsOffset / 4, rhsOffset / 4);
            }
            break;
        case LongInst_Le:
            {
                printf("LongInst_Le R[%d] <= R[%d]\n", lhsOffset / 4, rhsOffset / 4);
            }
            break;
        case LongInst_Ge:
            {
                printf("LongInst_Ge R[%d] >= R[%d]\n", lhsOffset / 4, rhsOffset / 4);
            }
            break;
#if 0
        case LongInst_PushCatch:
            {
                printf("LongInst_PushCatch\n");
                debug
                {
                    printf("PushCatch is executing\n");
                }
                Catch catch_ = Catch(ip, callDepth);
                catches ~= catch_;
            }
            break;

            case LongInst_PopCatch:
            {
                printf("LongInst_PopCatch\n");
                debug { if (!__ctfe) writeln("Poping a Catch"); }
                catches = catches[0 .. $-1];
            }
            break;

            case LongInst_Throw:
            {
                printf("LongInst_Throw\n");
                uint32_t expP = ((*opRef) & UINT32_MAX);
                auto expTypeIdx = heapPtr->heapData[expP + ClassMetaData.TypeIdIdxOffset];
                auto expValue = BCValue(HeapAddr(expP), BCType(BCTypeEnum_Class, expTypeIdx));
                expValue.vType = BCValueType.Exception;

                cRetval = expValue;
                if (HandleExp())
                    return cRetval;
            }
            break;
#endif
        case LongInst_Jmp:
            {
                printf("LongInst_Jmp :%u\n", hi);
            }
            break;
        case LongInst_JmpNZ:
            {
                printf("LongInst_JmpNZ :%u\n", hi);
            }
            break;
        case LongInst_JmpZ:
            {
                printf("LongInst_JmpZ :%u\n", hi);
            }
            break;
        case LongInst_JmpFalse:
            {
                printf("LongInst_JmpFalse :%u\n", hi);
            }
            break;
        case LongInst_JmpTrue:
            {
                printf("LongInst_JmpTrue\n :%u\n", hi);
            }
            break;
        case LongInst_HeapLoad8:
            {
                printf("LongInst_HeapLoad8 R[%d] = HEAP[R[%d]]\n", lhsOffset / 4, rhsOffset / 4);
            }
            break;
        case LongInst_HeapStore8:
            {
                printf("LongInst_HeapStore8 HEAP[R[%d]] = R[%d]\n", lhsOffset / 4, rhsOffset / 4);
            }
            break;
        case LongInst_HeapLoad16:
            {
                printf("LongInst_HeapLoad16 R[%d] = HEAP[R[%d]]\n", lhsOffset / 4, rhsOffset / 4);
            }
            break;
        case LongInst_HeapStore16:
            {
                printf("LongInst_HeapStore16 HEAP[R[%d]] = R[%d]\n", lhsOffset / 4, rhsOffset / 4);
            }
            break;
        case LongInst_HeapLoad32:
            {
                printf("LongInst_HeapLoad32 R[%d] = HEAP[R[%d]]\n", lhsOffset / 4, rhsOffset / 4);
            }
            break;
        case LongInst_HeapStore32:
            {
                printf("LongInst_HeapStore32 HEAP[R[%d]] = R[%d]\n", lhsOffset / 4, rhsOffset / 4);
            }
            break;
        case LongInst_HeapLoad64:
            {
                 printf("LongInst_HeapLoad64 R[%d] = HEAP[R[%d]]\n", lhsOffset / 4, rhsOffset / 4);
            }
            break;
        case LongInst_HeapStore64:
            {
                printf("LongInst_HeapStore64 HEAP[R[%d]] = R[%d]\n", lhsOffset / 4, rhsOffset / 4);
            }
            break;
        case LongInst_Ret32:
            {
                if (hi)
                    printf("LongInst_Ret32 %u\n", (hi == INT32_MIN ? 0 : hi));
                else
                    printf("LongInst_Ret32 R[%d]\n", opRefOffset / 4);
            }
            break;
        case LongInst_RetS32:
            {
                if (hi)
                    printf("LongInst_RetS32 %d\n", (hi == INT32_MIN ? 0 : hi));
                else
                    printf("LongInst_RetS32 R[%d]\n", opRefOffset / 4);
            }
            break;
        case LongInst_RetS64:
            {
                printf("LongInst_RetS64 R[%d]\n", opRefOffset / 4);
            }
            break;
        case LongInst_Ret64:
            {
                printf("LongInst_Ret64 R[%d]\n", opRefOffset / 4);
            }
            break;
        case LongInst_RelJmp:
            {
                printf("LongInst_RelJmp : %d\n", (int16_t)(lw >> 16) - 2);
            }
            break;
        case LongInst_PrintValue:
            {
                bool isString = (opSpecial != 0);
                printf("LongInst_Print%s\n", (isString ? "String" : "Value"));
            }
            break;
        case LongInst_Not:
            {
                printf("LongInst_Not R[%d]\n", opRefOffset / 4);
            }
            break;
        case LongInst_Flg:
            {
                printf("LongInst_Flg R[%d]\n", opRefOffset / 4);
            }
            break;
        case LongInst_BuiltinCall:
            {
                printf("LongInst_BuiltinCall\n");
                assert(0);//, "Unsupported right now: BCBuiltin");
            }
            break;

        case LongInst_Realloc:
            {
                const uint32_t result = opRefOffset / 4;
                const uint32_t ptr =  lhsOffset / 4;
                const uint32_t newSize =  rhsOffset / 4;

                printf("LongInst_Realloc\n");
#if 0
                if (*rhs == 0 && *lhsRef == 0)
                {
                    *opRef = 0;
                }
                else
                {
                    const llbasep = &heapPtr->heapData[_lhs + SliceDescriptor.LengthOffset];
                    const rlbasep = &heapPtr->heapData[_rhs + SliceDescriptor.LengthOffset];

                    const lhs_length = _lhs ? loadu32(llbasep) : 0;
                    const rhs_length = _rhs ? loadu32(rlbasep) : 0;

                    if (const newLength = lhs_length + rhs_length)
                    {
                        // TODO if lhs.capacity bla bla
                        const lhsBase = loadu32(&heapPtr->heapData[_lhs + SliceDescriptor.BaseOffset]);
                        const rhsBase = loadu32(&heapPtr->heapData[_rhs + SliceDescriptor.BaseOffset]);

                        const resultPtr = heapPtr.heapSize;

                        const resultLengthP = resultPtr + SliceDescriptor.LengthOffset;
                        const resultBaseP   = resultPtr + SliceDescriptor.BaseOffset;
                        const resultBase    = resultPtr + SliceDescriptor.Size;

                        const allocSize = (newLength * elemSize) + SliceDescriptor.Size;
                        const heapSize  = heapPtr.heapSize;

                        if(heapSize + allocSize  >= heapPtr.heapMax)
                        {
                            if (heapPtr.heapMax >= 2 ^^ 31)
                                assert(0, "!!! HEAP OVERFLOW !!!");
                            else
                            {
                                // we will now resize the heap to 8 times its former size
                                const newHeapSize =
                                    ((allocSize < heapPtr.heapMax * 4) ?
                                    heapPtr.heapMax * 8 :
                                    align4(cast(uint32_t)(heapPtr.heapMax + allocSize)) * 4);

                                auto newHeap = new ubyte[](newHeapSize);
                                newHeap[0 .. heapSize] = heapPtr->heapData[0 .. heapSize];
                                if (!__ctfe) heapPtr->heapData.destroy();

                                heapPtr->heapData = newHeap;
                                heapPtr.heapMax = newHeapSize;
                            }
                        }

                        heapPtr.heapSize += allocSize;

                        const scaled_lhs_length = (lhs_length * elemSize);
                        const scaled_rhs_length = (rhs_length * elemSize);
                        const result_lhs_end    = resultBase + scaled_lhs_length;

                        storeu32(&heapPtr->heapData[resultBaseP],  resultBase);
                        storeu32(&heapPtr->heapData[resultLengthP], newLength);

                        heapPtr->heapData[resultBase .. result_lhs_end] =
                            heapPtr->heapData[lhsBase .. lhsBase + scaled_lhs_length];

                        heapPtr->heapData[result_lhs_end ..  result_lhs_end + scaled_rhs_length] =
                            heapPtr->heapData[rhsBase .. rhsBase + scaled_rhs_length];

                        *opRef = resultPtr;
                    }
                }
#endif
            }
            break;

        case LongInst_Call:
            {
                printf("LongInst_Call R[%d] = ?\n", (opRefOffset / 4));
            }
            break;
        case LongInst_ContextManip:
            {
                CtxM manip = cast(CtxM) opSpecial;
                printf("LongInst_ContextManip {%s, R[%u], %d}",
                       CtxM_toChars(manip), lhsOffset / 4, imm32c);
            }
            break;
        case LongInst_Alloc:
            {
                printf("LongInst_Alloc R[%d] = ALLOC(R[%d])\n", lhsOffset / 4, rhsOffset / 4);
            }
            break;
        case LongInst_MemCpy:
            {
                //TODO verify this printout
                printf("LongInst_MemCpy(R[%d], R[%d], R[%d])\n", lhsOffset / 4 , rhsOffset / 4 , opRefOffset / 4);
            }
            break;

        case LongInst_Comment:
            {
                printf("LongInst_Comment [length:%d]\n", hi);
                uint32_t nWords = ((hi + 7) & ~7) / 4;
                printf(" %.*s", (int) hi, (const char*) (codeP + ip));
                ip += nWords;
            }
            break;
        case LongInst_Memcmp:
            {
                printf("LongInst_MemCmp (lhs: R[%d], rhs: R[%d], size: R[%d])\n",
                    lhsOffset / 4, rhsOffset / 4, opRefOffset / 4
                );
            }
            break;
        case LongInst_File :
            {
                printf("LongInst_File \n");
            }
            break;
        case LongInst_Line :
            {
                printf("LongInst_Line \n");
            } break;
        case LongInst_SetMode :
            {
                printf("LongInst_SetMode \n");
            }
            break;
        }
    }
}
#endif // PRINT_CODE

static inline uint8_t* BCInterpreter_toRealPointer(const BCInterpreter* self, const BCHeap* heapPtr, uint32_t unrealAddress)
{
    uint8_t* result = heapPtr->heapData + unrealAddress;

    if (isStackAddress(unrealAddress))
    {
        assert((unrealAddress & 3) == 0);
        result = (uint8_t*) (self->fp + toStackOffset(unrealAddress / 4));
    }
    return result;
}

#define MODE_STICKY_MASK  ((0x3) << 7)
#define MODE_STICKY_TRUE  ((0x1) << 7)
#define MODE_STICKY_FALSE ((0x2) << 7)
#define MODE_CND_SET       0x7
BCValue BCGen_interpret(BCGen* self, uint32_t fnIdx, BCValue* args, uint32_t n_args, BCHeap* heapPtr)
{
    assert(self->finalized);

    BCInterpreter state = {0};
    BCFunction* f = self->functions + (fnIdx - 1);
    state.ip = f->bytecode_start;
    state.fp = state.stack;
    state.sp = state.stack;

    uint32_t* codeP = self->byteCodeArray;
    if (self->byteCodeCount > ARRAY_SIZE(self->byteCodeArray))
    {
        codeP = self->byteCodeArrayExtra;
    }

    {
        int argOffset = 1;
        int64_t* frameP = state.fp;
        for(uint32_t i = 0; i < n_args;i++)
        {
            BCValue* arg = args + i;
            assert(arg->vType == BCValueType_Immediate);

            switch (arg->type.type)
            {
                case BCTypeEnum_i32:
                case BCTypeEnum_i16:
                case BCTypeEnum_i8:
                {
                    frameP[argOffset++] = cast(int32_t)arg->imm32.imm32;
                }
                break;

                case BCTypeEnum_u32:
                case BCTypeEnum_f23:
                case BCTypeEnum_c8:
                case BCTypeEnum_u16:
                case BCTypeEnum_u8:
                {
                    frameP[argOffset++] = cast(uint32_t)arg->imm32.imm32;
                }
                break;

            case BCTypeEnum_i64:
                {
                    frameP[argOffset++] = arg->imm64.imm64;
                }
                break;

            case BCTypeEnum_u64:
            case BCTypeEnum_f52:
            {
                frameP[argOffset++] = arg->imm64.imm64;
            }
            break;

            case BCTypeEnum_Struct:
            case BCTypeEnum_Class:
            case BCTypeEnum_string8:
            case BCTypeEnum_Array:
                {
                    // This might need to be removed again?
                    frameP[argOffset++] = arg->heapAddr.addr;
                }
                break;
            default:
                //return -1;
                       assert(0);//, "unsupported Type " ~ enumToString(arg.type.type));
            }
        }
    }

    int cond = 1;

    for (;;)
    {
        const uint32_t lw = (codeP)[state.ip];
        const uint32_t hi = (codeP)[state.ip + 1];
        const int32_t imm32c = *(cast(int32_t*)&((codeP)[state.ip + 1]));
        state.ip += 2;

        if (IS_CMP_INST(lw & INSTMASK) && (state.mode & MODE_STICKY_MASK))
        {
            switch(state.mode & MODE_STICKY_MASK)
            {
                case MODE_STICKY_TRUE:
                {
                    if ((state.mode & MODE_CND_SET) && cond == 0)
                        continue;
                } break;
                case MODE_STICKY_FALSE:
                {
                    if ((state.mode & MODE_CND_SET) && (cond == 1 || cond == -1))
                        continue;
                } break;
            }
        }

        // consider splitting the framePointer in stackHigh and stackLow
        const uint8_t  opSpecial   = ((lw >> 8) & 0xFF);
        const uint32_t opRefOffset = (lw >> 16) & 0xFFFF;
        const uint32_t lhsOffset   = hi & 0xFFFF;
        const uint32_t rhsOffset   = (hi >> 16) & 0xFFFF;

        const int64_t* frameP = state.fp;

        int64_t* lhsRef = cast(int64_t*) &frameP[(lhsOffset / 4)];
        int64_t* rhs = cast(int64_t*) &frameP[(rhsOffset / 4)];
        int64_t* opRef = cast(int64_t*) &frameP[(opRefOffset / 4)];

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wstrict-aliasing"
        float flhs = 0;
        float frhs = 0;

        double drhs = 0;
        double dlhs = 0;

        int64_t rhs_value = 0;

        if ((lw & INSTMASK) >= FLT32_BEGIN && (lw & INSTMASK) <= FLT32_END)
        {
            uint32_t _lhs = (*lhsRef) & UINT32_MAX;
            flhs = *(float*)&_lhs;
            uint32_t _rhs = (*rhs) & UINT32_MAX;
            frhs = *(float*)&_rhs;
        }
        else if ((lw & INSTMASK) >= FLT64_BEGIN && (lw & INSTMASK) <= FLT64_END)
        {
            dlhs = *(double*)lhsRef;
            drhs = *(double*)rhs;
        }

        if (!lw)
        { // Skip NOPS
            continue;
        }

        switch (cast(LongInst)(lw & INSTMASK))
        {
        case LongInst_ImmAdd:
            {
                (*opRef) += imm32c;
            }
            break;

        case LongInst_ImmSub:
            {
                (*opRef) -= imm32c;
            }
            break;

        case LongInst_ImmMul:
            {
                (*opRef) *= imm32c;
            }
            break;

        case LongInst_ImmDiv:
            {
                (*opRef) /= imm32c;
            }
            break;

        case LongInst_ImmUdiv:
            {
                (*cast(uint64_t*)opRef) /= imm32c;
            }
            break;

        case LongInst_ImmAnd:
            {
                (*opRef) &= hi;
            }
            break;
        case LongInst_ImmAnd32:
            {
                *opRef = (cast(uint32_t)*opRef) & hi;
            }
            break;
        case LongInst_ImmOr:
            {
                (*opRef) |= hi;
            }
            break;
        case LongInst_ImmXor:
            {
                (*opRef) ^= hi;
            }
            break;
        case LongInst_ImmXor32:
            {
                *opRef = (cast(uint32_t)*opRef) ^ hi;
            }
            break;

        case LongInst_ImmLsh:
            {
                (*opRef) <<= hi;
            }
            break;
        case LongInst_ImmRsh:
            {
                (*cast(uint64_t*)opRef) >>= hi;
            }
            break;

        case LongInst_ImmMod:
            {
                (*opRef) %= imm32c;
            }
            break;
        case LongInst_ImmUmod:
            {
                (*cast(uint64_t*)opRef) %= imm32c;
            }
            break;

        case LongInst_SetImm8:
            {
                (*opRef) = hi;
                assert(hi <= UINT8_MAX);
            }
            break;
        case LongInst_SetImm32:
            {
                (*opRef) = hi;
            }
            break;
        case LongInst_SetHighImm32:
            {
                uint64_t hi64 = hi;
                *opRef = (*opRef & 0x00000000FFFFFFFF) | ((hi64) << 32UL);
            }
            break;
        case LongInst_ImmEq:
            {
               cond = ((*opRef) == imm32c);
            }
            break;
        case LongInst_ImmNeq:
            {
               cond = ((*opRef) != imm32c);
            }
            break;

        case LongInst_ImmUlt:
            {
               cond = (((int64_t)(*opRef)) < cast(uint32_t)hi);
            }
            break;
        case LongInst_ImmUgt:
            {
               cond = (((uint64_t)(*opRef)) > cast(uint32_t)hi);
            }
            break;
        case LongInst_ImmUle:
            {
               cond = (((uint64_t)(*opRef)) <= cast(uint32_t)hi);
            }
            break;
        case LongInst_ImmUge:
            {
               cond = (((uint64_t)(*opRef)) >= cast(uint32_t)hi);
            }
            break;

        case LongInst_ImmLt:
            {
               cond = ((*opRef) < imm32c);
            }
            break;
        case LongInst_ImmGt:
            {
               cond = ((*opRef) > imm32c);
            }
            break;
        case LongInst_ImmLe:
            {
               cond = ((*opRef) <= imm32c);
            }
            break;
        case LongInst_ImmGe:
            {
               cond = ((*opRef) >= imm32c);
            }
            break;

        case LongInst_Add:
            {
                (*lhsRef) += *rhs;
            }
            break;
        case LongInst_Sub:
            {
                (*lhsRef) -= *rhs;
            }
            break;
        case LongInst_Mul:
            {
                (*lhsRef) *= *rhs;
            }
            break;
        case LongInst_Div:
            {
                (*lhsRef) /= *rhs;
            }
            break;
        case LongInst_Udiv:
            {
                (*cast(uint64_t*)lhsRef) /= (*cast(uint64_t*)rhs);
            }
            break;
        case LongInst_And:
            {
                (*lhsRef) &= *rhs;
            }
            break;
        case LongInst_And32:
            {
               (*lhsRef) = (cast(uint32_t) *lhsRef) & (cast(uint32_t)*rhs);
            }
            break;
        case LongInst_Or:
            {
                (*lhsRef) |= *rhs;
            }
            break;
        case LongInst_Xor32:
            {
                (*lhsRef) = (cast(uint32_t) *lhsRef) ^ (cast(uint32_t)*rhs);
            }
            break;
        case LongInst_Xor:
            {
                (*lhsRef) ^= *rhs;
            }
            break;

        case LongInst_Lsh:
            {
                (*lhsRef) <<= *rhs;
            }
            break;
        case LongInst_Rsh:
            {
                (*cast(uint64_t*)lhsRef) >>= *rhs;
            }
            break;
        case LongInst_Mod:
            {
                (*lhsRef) %= *rhs;
            }
            break;
        case LongInst_Umod:
            {
                (*cast(uint64_t*)lhsRef) %= (*cast(uint64_t*)rhs);
            }
            break;
        case LongInst_FGt32 :
            {
               cond = (flhs > frhs);
            }
            break;
        case LongInst_FGe32 :
            {
               cond = (flhs >= frhs);
            }
            break;
        case LongInst_FEq32 :
            {
               cond = (flhs == frhs);
            }
            break;
        case LongInst_FNeq32 :
            {
               cond = (flhs != frhs);
            }
            break;
        case LongInst_FLt32 :
            {
               cond = (flhs < frhs);
            }
            break;
        case LongInst_FLe32 :
            {
               cond = (flhs <= frhs);
            }
            break;
        case LongInst_F32ToF64 :
            {
                uint32_t rhs32 = (*rhs & UINT32_MAX);
                float frhs_ = *cast(float*)&rhs32;
                double dlhs_ = frhs_;
                *lhsRef = *cast(int64_t*)&dlhs_;
            }
            break;
        case LongInst_F32ToI :
            {
                uint32_t rhs32 = (*rhs & UINT32_MAX);
                float frhs = *cast(float*)&rhs32;
                *lhsRef = cast(int32_t)frhs;
            }
            break;
        case LongInst_IToF32 :
            {
                float frhs = *rhs;
                uint32_t _lhs = *cast(uint32_t*)&frhs;
                *lhsRef = _lhs;
            }
            break;

        case LongInst_FAdd32:
            {
                flhs += frhs;

                *lhsRef = *(uint32_t*)&flhs;
            }
            break;
        case LongInst_FSub32:
            {
                flhs -= frhs;
                *lhsRef = *(uint32_t*)&flhs;
            }
            break;
        case LongInst_FMul32:
            {
                flhs *= frhs;
                *lhsRef = *(uint32_t*)&flhs;
            }
            break;
        case LongInst_FDiv32:
            {
                flhs /= frhs;
                *lhsRef = *(uint32_t*)&flhs;
            }
            break;
#ifdef WITH_FMOD
        case LongInst_FMod32:
            {
                flhs = fmodf(flhs, frhs);
                *lhsRef = *(uint32_t*)&flhs;
            }
            break;
#endif
        case LongInst_FEq64 :
            {
               cond = (dlhs == drhs);
            }
            break;
        case LongInst_FNeq64 :
            {
               cond = (dlhs < drhs);
            }
            break;
        case LongInst_FLt64 :
            {
               cond = (dlhs < drhs);
            }
            break;
        case LongInst_FLe64 :
            {
               cond = (dlhs <= drhs);
            }
            break;
        case LongInst_FGt64 :
            {
               cond = (dlhs > drhs);
            }
            break;
        case LongInst_FGe64 :
            {
               cond = (dlhs >= drhs);
            }
            break;

        case LongInst_F64ToF32 :
            {
                double drhs_ = *cast(double*)rhs;
                float flhs_ = drhs_;
                *lhsRef = *(uint32_t*)&flhs_;
            }
            break;
        case LongInst_F64ToI :
            {
                float drhs_ = *(double*)rhs;
                *lhsRef = (int64_t)drhs_;
            }
            break;
        case LongInst_IToF64 :
            {
                double drhs_ = (double)*rhs;
                *lhsRef = *(int64_t*)&drhs_;
            }
            break;

        case LongInst_FAdd64:
            {
                dlhs += drhs;
                *lhsRef = *(uint64_t*)&dlhs;
            }
            break;
        case LongInst_FSub64:
            {
                dlhs -= drhs;
                *lhsRef = *(uint64_t*)&dlhs;
            }
            break;
        case LongInst_FMul64:
            {
                dlhs *= drhs;
                *lhsRef = *(uint64_t*)&dlhs;
            }
            break;
        case LongInst_FDiv64:
            {
                dlhs /= drhs;

                *(cast(uint64_t*)lhsRef) = *cast(uint64_t*)&dlhs;
            }
            break;
#ifdef WITH_FMOD
        case LongInst_FMod64:
            {
                dlhs = fmod(dlhs, drhs);

                *(cast(uint64_t*)lhsRef) = *cast(uint64_t*)&dlhs;
            }
            break;
#endif
#pragma GCC diagnostic pop

        case LongInst_Assert:
            {
                if (*opRef == 0)
                {
                    BCValue retval = imm32(hi);
                    retval.vType = BCValueType_Error;
                    return retval;
                }
            }
            break;
        case LongInst_Eq:
            {
               cond = ((*lhsRef) == (*rhs));
            }
            break;

        case LongInst_Neq:
            {
               cond = ((*lhsRef) != *rhs);
            }
            break;

        case LongInst_Set:
            {
                (*lhsRef) = *rhs;
            }
            break;

        case LongInst_Ult:
            {
               cond = (((uint64_t)(*lhsRef)) < ((uint64_t)*rhs));
            }
            break;
        case LongInst_Ugt:
            {
               cond = ((uint64_t)(*lhsRef) > (uint64_t)*rhs);
            }
            break;
        case LongInst_Ule:
            {
               cond = (((uint64_t)(*lhsRef)) <= ((uint64_t)*rhs));
            }
            break;
        case LongInst_Uge:
            {
               cond = (((uint64_t)(*lhsRef)) >= ((uint64_t)*rhs));
            }
            break;

        case LongInst_Lt:
            {
               cond = ((*lhsRef) < *rhs);
            }
            break;
        case LongInst_Gt:
            {
               cond = ((*lhsRef) > *rhs);
            }
            break;
        case LongInst_Le:
            {
               cond = ((*lhsRef) <= *rhs);
            }
            break;
        case LongInst_Ge:
            {
               cond = ((*lhsRef) >= *rhs);
            }
            break;
#if 0
        case LongInst_PushCatch:
            {
                debug
                {
                    printf("PushCatch is executing\n");
                }
                Catch catch_ = Catch(ip, callDepth);
                catches ~= catch_;
            }
            break;

            case LongInst_PopCatch:
            {
                debug { if (!__ctfe) writeln("Poping a Catch"); }
                catches = catches[0 .. $-1];
            }
            break;

            case LongInst_Throw:
            {
                uint expP = ((*opRef) & UINT32_MAX);
                auto expTypeIdx = heapPtr->heapData[expP + ClassMetaData.TypeIdIdxOffset];
                auto expValue = BCValue(HeapAddr(expP), BCType(BCTypeEnum_Class, expTypeIdx));
                expValue.vType = BCValueType.Exception;

                cRetval = expValue;
                if (HandleExp())
                    return cRetval;
            }
            break;
#endif
        case LongInst_Jmp:
            {
                state.ip = hi;
            }
            break;
        case LongInst_JmpNZ:
            {
                if ((*opRef) != 0)
                {
                    state.ip = hi;
                }
            }
            break;
        case LongInst_JmpZ:
            {
                if ((*opRef) == 0)
                {
                    state.ip = hi;
                }
            }
            break;
        case LongInst_JmpFalse:
            {
                if (cond == 0)
                {
                    state.ip = hi;
                }
            }
            break;
        case LongInst_JmpTrue:
            {
                if (cond != 0)
                {
                    state.ip = hi;
                }
            }
            break;

        case LongInst_HeapLoad8:
            {
                assert(*rhs);//, "trying to deref null pointer inLine: " ~ itos(lastLine));
                (*lhsRef) = heapPtr->heapData[*rhs];
            }
            break;
        case LongInst_HeapStore8:
            {
                assert(*lhsRef);//, "trying to deref null pointer SP[" ~ itos(cast(int)((lhsRef - &frameP[0])*4)) ~ "] at : &" ~ itos (ip - 2));
                heapPtr->heapData[*lhsRef] = ((*rhs) & 0xFF);
            }
            break;

            case LongInst_HeapLoad16:
            {
                assert(*rhs);//, "trying to deref null pointer inLine: " ~ itos(lastLine));
                const uint32_t addr = (uint32_t)*rhs;
                (*lhsRef) =  heapPtr->heapData[addr]
                          | (heapPtr->heapData[addr + 1] << 8);

            }
            break;
            case LongInst_HeapStore16:
            {
                assert(*lhsRef);//, "trying to deref null pointer SP[" ~ itos(cast(int)((lhsRef - &frameP[0])*4)) ~ "] at : &" ~ itos (ip - 2));
                const uint32_t addr = (uint32_t)*lhsRef;
                heapPtr->heapData[addr    ] = ((*rhs     ) & 0xFF);
                heapPtr->heapData[addr + 1] = ((*rhs >> 8) & 0xFF);
            }
            break;

            case LongInst_HeapLoad32:
            {
                assert(*rhs); //, "trying to deref null pointer inLine: " ~ itos(lastLine));
                (*lhsRef) = loadu32(heapPtr->heapData + *rhs);
            }
            break;
        case LongInst_HeapStore32:
            {
                assert(*lhsRef);//, "trying to deref null pointer SP[" ~ itos(cast(int)((lhsRef - &frameP[0])*4)) ~ "] at : &" ~ itos (ip - 2));
                //(*(heapPtr->heapData.ptr + *lhsRef)) = (*rhs) & 0xFF_FF_FF_FF;
                storeu32((&heapPtr->heapData[*lhsRef]),  (*rhs) & UINT32_MAX);
            }
            break;

        case LongInst_HeapLoad64:
            {
                assert(*rhs);//, "trying to deref null pointer ");
                const uint32_t addr = (uint32_t)*rhs;
                const uint8_t* basePtr = heapPtr->heapData + addr;

                uint64_t value = loadu32(basePtr + 4);
                value <<= 32UL;
                value |= loadu32(basePtr);

                (*lhsRef) = value;
            }
            break;

        case LongInst_HeapStore64:
            {
                const uint32_t addr = (uint32_t)*lhsRef;
                assert(addr);//, "trying to deref null pointer SP[" ~ itos(cast(int)(lhsRef - &frameP[0])*4) ~ "] at : &" ~ itos (ip - 2));
                assert(addr < heapPtr->heapSize);//, "Store out of range at ip: &" ~ itos(ip - 2) ~ " atLine: " ~ itos(lastLine));

                uint8_t* basePtr = heapPtr->heapData + addr;
                const uint64_t value = *(uint64_t*)rhs;

                storeu32(basePtr,     value & UINT32_MAX);
                storeu32(basePtr + 4, cast(uint32_t)(value >> 32));
            }
            break;

        case LongInst_Ret32:
            {
                if (hi)
                    state.cRetval = imm32(hi == INT32_MIN ? 0 : hi);
                else
                    state.cRetval = imm32(*opRef & UINT32_MAX);

                if (BCInterpreter_Return(&state)) return state.cRetval;
            }
            break;
        case LongInst_RetS32:
            {
                if (hi)
                    state.cRetval = imm32_((hi == INT32_MIN ? 0 : hi), true);
                else
                    state.cRetval = imm32_(*opRef & UINT32_MAX, true);

                if (BCInterpreter_Return(&state)) return state.cRetval;
            }
            break;
        case LongInst_RetS64:
            {
                state.cRetval = imm64_(*opRef, true);
                if (BCInterpreter_Return(&state)) return state.cRetval;
            }
            break;

        case LongInst_Ret64:
            {
                state.cRetval = imm64_(*opRef, false);
                if (BCInterpreter_Return(&state)) return state.cRetval;
            }
            break;
        case LongInst_RelJmp:
            {
                state.ip += (cast(short)(lw >> 16)) - 2;
            }
            break;
        case LongInst_PrintValue:
            {
                if ((lw & UINT16_MAX) >> 8)
                {
                    long offset = *opRef;
                    uint8_t length = heapPtr->heapData[offset];
                    char* string_start = cast(char*)&heapPtr->heapData[offset + 1];
                }
                else
                {
                    //TODO readd print!
                }
            }
            break;
        case LongInst_Not:
            {
                (*opRef) = ~(*opRef);
            }
            break;
        case LongInst_Flg:
            {
                (*opRef) = cond;
            }
            break;

        case LongInst_BuiltinCall:
            {
                assert(0);//, "Unsupported right now: BCBuiltin");
            }
#if 0
        case LongInst_Realloc:
            {
                if (*rhs == 0 && *lhsRef == 0)
                {
                    *opRef = 0;
                }
                else
                {
                    const elemSize = opSpecial;
                    const uint _lhs =  *lhsRef & UINT32_MAX;
                    const uint _rhs =  *rhs & UINT32_MAX;

                    const llbasep = &heapPtr->heapData[_lhs + SliceDescriptor.LengthOffset];
                    const rlbasep = &heapPtr->heapData[_rhs + SliceDescriptor.LengthOffset];

                    const lhs_length = _lhs ? loadu32(llbasep) : 0;
                    const rhs_length = _rhs ? loadu32(rlbasep) : 0;

                    if (const newLength = lhs_length + rhs_length)
                    {
                        // TODO if lhs.capacity bla bla
                        const lhsBase = loadu32(&heapPtr->heapData[_lhs + SliceDescriptor.BaseOffset]);
                        const rhsBase = loadu32(&heapPtr->heapData[_rhs + SliceDescriptor.BaseOffset]);

                        const resultPtr = heapPtr.heapSize;

                        const resultLengthP = resultPtr + SliceDescriptor.LengthOffset;
                        const resultBaseP   = resultPtr + SliceDescriptor.BaseOffset;
                        const resultBase    = resultPtr + SliceDescriptor.Size;

                        const allocSize = (newLength * elemSize) + SliceDescriptor.Size;
                        const heapSize  = heapPtr.heapSize;

                        if(heapSize + allocSize  >= heapPtr.heapMax)
                        {
                            if (heapPtr.heapMax >= 2 ^^ 31)
                                assert(0, "!!! HEAP OVERFLOW !!!");
                            else
                            {
                                // we will now resize the heap to 8 times its former size
                                const newHeapSize =
                                    ((allocSize < heapPtr.heapMax * 4) ?
                                    heapPtr.heapMax * 8 :
                                    align4(cast(uint32_t)(heapPtr.heapMax + allocSize)) * 4);

                                auto newHeap = new ubyte[](newHeapSize);
                                newHeap[0 .. heapSize] = heapPtr->heapData[0 .. heapSize];
                                if (!__ctfe) heapPtr->heapData.destroy();

                                heapPtr->heapData = newHeap;
                                heapPtr.heapMax = newHeapSize;
                            }
                        }

                        heapPtr.heapSize += allocSize;

                        const scaled_lhs_length = (lhs_length * elemSize);
                        const scaled_rhs_length = (rhs_length * elemSize);
                        const result_lhs_end    = resultBase + scaled_lhs_length;

                        storeu32(&heapPtr->heapData[resultBaseP],  resultBase);
                        storeu32(&heapPtr->heapData[resultLengthP], newLength);

                        heapPtr->heapData[resultBase .. result_lhs_end] =
                            heapPtr->heapData[lhsBase .. lhsBase + scaled_lhs_length];

                        heapPtr->heapData[result_lhs_end ..  result_lhs_end + scaled_rhs_length] =
                            heapPtr->heapData[rhsBase .. rhsBase + scaled_rhs_length];

                        *opRef = resultPtr;
                    }
                }
            }
            break;
#endif
        case LongInst_Call:
            {
                assert(self->functions);//, "When calling functions you need functions to call");
                RetainedCall call = self->calls[(*rhs & UINT32_MAX) - 1];
                ReturnAddr returnAddr = {state.ip, state.fnIdx, state.fp, lhsRef};

                uint32_t fn = ((call.fn.vType == BCValueType_Immediate) ?
                    call.fn.imm32.imm32 :
                    frameP[call.fn.stackAddr.addr / 4]
                ) & UINT32_MAX;

                state.fnIdx = fn - 1;

                int64_t* newStack = state.sp;

                if (fn == skipFn)
                    continue;

                //foreach(size_t i,ref arg;call.args)
                for(int i = 0; i < call.n_args; i++)
                {
                    const BCValue* arg = call.args + i;
                    const int argOffset_ = (i * 1) + 1;
                    if(BCValue_isStackValueOrParameter(arg))
                    {
                        newStack[argOffset_] = frameP[arg->stackAddr.addr / 4];
                    }
                    else if (arg->vType == BCValueType_Immediate)
                    {
                        newStack[argOffset_] = arg->imm64.imm64;
                    }
                    else
                    {
                        assert(0);//, "Argument " ~ itos(cast(int)i) ~" ValueType unhandeled: " ~ enumToString(arg.vType));
                    }
                }

                if (state.callDepth++ == max_call_depth)
                {
                    BCValue bailoutValue;
                    bailoutValue.vType = BCValueType_Bailout;
                    bailoutValue.imm32.imm32 = 2000;
                    return bailoutValue;
                }

                {
                    state.returnAddrs[state.n_return_addrs++] = returnAddr;
                    state.stackTop = state.stackTop + (call.callerSp.addr / 4);
                    BCFunction* f =  self->functions + state.fnIdx;
                    state.ip = f->bytecode_start;
                }
            }
            break;

        case LongInst_Alloc:
            {
                const uint32_t allocSize = *rhs;
                const uint32_t heapSize = heapPtr->heapSize;

                if(heapSize + allocSize  >= heapPtr->heapMax)
                {
                    if (heapPtr->heapMax >= (1 << 31))
                        assert(0);//, "!!! HEAP OVERFLOW !!!");
                    else
                    {
                        // we will now resize the heap to 4 times its former size
                        const uint32_t newHeapSize =
                            ((allocSize < heapPtr->heapMax * 2) ?
                            heapPtr->heapMax * 4 :
                            align4(cast(uint32_t)(heapPtr->heapMax + allocSize)) * 2);

                        heapPtr->heapData = (uint8_t*)realloc(heapPtr->heapData, newHeapSize);
                        heapPtr->heapMax = newHeapSize;
                    }
                }

                *lhsRef = heapSize;
                heapPtr->heapSize += allocSize;
            }
            break;
        case LongInst_MemCpy:
            {
                uint32_t cpySize = cast(uint32_t) *opRef;
                uint32_t cpySrc = cast(uint32_t) *rhs;
                uint32_t cpyDst = cast(uint32_t) *lhsRef;

                if (cpySrc != cpyDst && cpySize != 0)
                {
                    // assert(cpySize, "cpySize == 0");
                    assert(cpySrc);//, "cpySrc == 0" ~ " inLine: " ~ itos(lastLine));

                    assert(cpyDst);//, "cpyDst == 0" ~ " inLine: " ~ itos(lastLine));

                    assert(cpyDst >= cpySrc + cpySize || cpyDst + cpySize <= cpySrc);
                    //, "Overlapping MemCpy is not supported --- src: " ~ itos(cpySrc)
                    //    ~ " dst: " ~ itos(cpyDst) ~ " size: " ~ itos(cpySize));

                    uint8_t* heapData = heapPtr->heapData;

                    uint8_t* cpyDstP = heapData + cpyDst;
                    uint8_t* cpySrcP = heapData + cpySrc;

                    memcpy(cpyDstP, cpySrcP, cpySize * sizeof(*heapPtr->heapData));
                    //heapPtr->heapData[cpyDst .. cpyDst + cpySize] = heapPtr->heapData[cpySrc .. cpySrc + cpySize];
                }
            }
            break;
        case LongInst_ReadI32:
            {
                assert(hi < self->contextCount);
                ReadI32_ctx_t ctx = self->contexts[hi];
                int32_t value = (int32_t)(*(int64_t*)opRef);
                ctx.cb(value, ctx.userCtx);
            }
            break;

        case LongInst_Comment:
            {
L_LongInst_Comment:
                state.ip += ((hi + 7) & ~7) / 4;
            }
            break;
#if 1
        case LongInst_Memcmp:
            {
                cond = 1;

                uint32_t size = cast(uint32_t) *opRef;
                uint32_t _lhs = cast(uint32_t) *lhsRef;
                uint32_t _rhs = cast(uint32_t) *rhs;

                assert(_lhs && _rhs);//, "trying to deref nullPointers");
                if (_lhs == _rhs)
                {
                    cond = 0;
                }
                else
                {
                    uint8_t* lhsP = BCInterpreter_toRealPointer(&state, heapPtr, _lhs);
                    uint8_t* rhsP = BCInterpreter_toRealPointer(&state, heapPtr, _rhs);

                    for(uint32_t i = 0; i < size; i++)
                    {
                        uint8_t lhsC = *lhsP++;
                        uint8_t rhsC = *rhsP++;

                        if (lhsC == rhsC)
                            continue;
                        else
                        {
                            cond = ((lhsC > rhsC) ? 1 : -1);
                            break;
                        }
                    }
                }
            }
            break;
#endif
        case LongInst_File :
            {
                goto L_LongInst_Comment;
            }
        case LongInst_Line :
            {
                uint32_t breakingOn;
                uint32_t line = hi;
                state.lastLine = line;
#if 0
                foreach(bl;breakLines)
                {
                    if (line == bl)
                    {
                        debug
                        if (!__ctfe)
                        {
                            import std.stdio;
                            writeln("breaking at: ", ip-2);

                        }
                        paused = true;
                    }
                    break;
                }
#endif
            } break;
        case LongInst_SetMode:
            {
                uint16_t mode = lw >> 16;
                int32_t value = (int32_t) hi;
            } break;
        }
    }
    BCValue bailoutValue;
Lbailout :
    bailoutValue.vType = BCValueType_Bailout;

    return bailoutValue;
}

static void inline BCGen_emit2_at(BCGen* self, uint32_t low, uint32_t high, uint32_t atIp)
{
    uint32_t* codeP;
    if (atIp < ARRAY_SIZE(self->byteCodeArray))
    {
        codeP = self->byteCodeArray + atIp;
    }
    else
    {
        atIp -= ARRAY_SIZE(self->byteCodeArray);
        if (atIp < self->byteCodeExtraCapacity)
        {
            codeP = self->byteCodeArrayExtra + atIp;
        }
        else
        {
            // Code area should be expanded
            assert (0);
        }
    }

    codeP[0] = low;
    codeP[1] = high;
}

#ifdef NOMACRO
static void inline BCGen_emit2(BCGen* self, uint32_t low, uint32_t high)
{
    uint32_t atIp = self->ip;
    self->ip += 2;

    BCGen_emit2_at(self, low, high, atIp);
}

static void inline BCGen_emitLongInstSA(BCGen* self, const LongInst i, const StackAddr stackAddrLhs, const BCAddr targetAddr)
{
    BCGen_emit2(self
              , i | stackAddrLhs.addr << 16
              , targetAddr.addr);
}

static inline void BCGen_emitLongInstSS(BCGen* self, const LongInst i, const StackAddr stackAddrLhs,
    const StackAddr stackAddrRhs)
{
    BCGen_emit2(self
              , i
              , stackAddrLhs.addr | stackAddrRhs.addr << 16);
}
#define uint13_t uint16_t
#define uint3_t uint8_t

static inline void BCGen_emitLongInstCtxM(BCGen* self, CtxM manip,
    StackAddr addr, int32_t offset32)
{
     BCGen_emit2(self
              , LongInst_ContextManip | ((uint8_t) manip) << 8 | addr.addr << 16
              , offset32);
}

static inline void BCGen_emitLongInstSI(BCGen* self, const LongInst i, const StackAddr stackAddrLhs, uint32_t rhs)
{
    BCGen_emit2(self
              , i | stackAddrLhs.addr << 16
              , rhs);
}

static inline void BCGen_emitLongInstSSS(BCGen* self, const LongInst i, const StackAddr stackAddrOp,
    const StackAddr stackAddrLhs, const StackAddr stackAddrRhs)
{
    BCGen_emit2(self
              , i | stackAddrOp.addr << 16
              , stackAddrLhs.addr | stackAddrRhs.addr << 16);
}

static inline void BCGen_emitLongInstA(BCGen* self, const LongInst i, const BCAddr targetAddr)
{
    BCGen_emit2(self
              , i
              , targetAddr.addr);
}
#else

#define BCGen_emit2(SELF, LOW, HIGH) do \
{ \
    uint32_t atIp = SELF->ip; \
    SELF->ip += 2; \
    BCGen_emit2_at(SELF, LOW, HIGH, atIp); \
} while (0)

#define BCGen_emitLongInstSA(SELF, I, LHS_ADDR, TARGET_ADDR) do \
{ \
    BCGen_emit2(SELF \
              , I | LHS_ADDR.addr << 16 \
              , TARGET.addr); \
} while (0)

#define BCGen_emitLongInstSS(SELF, I, LHS_ADDR, RHS_ADDR) do \
{ \
    BCGen_emit2(SELF \
              , I \
              , LHS_ADDR.addr | RHS_ADDR.addr << 16); \
} while (0)

#define BCGen_emitLongInstSI(SELF, I, LHS_ADDR, RHS) do \
{ \
    BCGen_emit2(SELF \
              , I | LHS_ADDR.addr << 16 \
              , RHS); \
} while (0)

#define BCGen_emitLongInstSSS(SELF, I, OP_ADDR, LHS_ADDR, RHS_ADDR) do \
{ \
    BCGen_emit2(SELF \
              , I | OP_ADDR.addr << 16 \
              , LHS_ADDR.addr | RHS_ADDR.addr << 16); \
} while (0)

#define BCGen_emitLongInstCtxM(SELF, CTX_M, ADDR, OFF32) do \
{ \
    BCGen_emit2(SELF \
              , LongInst_ContextManip | ((uint8_t)CTX_M) << 8 | ADDR.addr << 16 \
              , OFF32); \
} while (0)

#define BCGen_emitLongInstA(SELF, T, TARGET_ADDR) do \
{ \
    BCGen_emit2(SELF \
              , I \
              , TARGET_ADDR.addr); \
} while (0)
#endif

/// semi-public functions for the vtbl start here
BCValue BCGen_genTemporary(BCGen* self, BCType bct)
{
    BCValue result = { BCValueType_StackValue };

    result.type = bct;
    result.temporaryIndex = ++self->temporaryCount;
    result.stackAddr.addr = self->sp;

    if (BCType_isBasicBCType(bct))
    {
        self->sp += align4(BCTypeEnum_basicTypeSize(bct.type));
    }
    else
    {
        self->sp += 4;
    }

    return result;
}

void BCGen_destroyTemporary(BCGen* self, BCValue* tmp)
{
    assert(BCValue_isStackValueOrParameter(tmp));//, "tmporary has to be stack-value");
    uint32_t sz;

    if (BCType_isBasicBCType(tmp->type))
    {
        sz = align4(BCTypeEnum_basicTypeSize(tmp->type.type));
    }
    else
    {
        sz = 4;
    }

    if (self->sp - sz == tmp->stackAddr.addr)
    {
        // this is the last thing we pushed on
        // free the stack space immediately.
        self->sp -= sz;
    }
}

void BCGen_destroyLocal(BCGen* self, BCValue* local)
{
    assert(BCValue_isStackValueOrParameter(local));//, "tmporary has to be stack-value");
    uint32_t sz;

    if (BCType_isBasicBCType(local->type))
    {
        sz = align4(BCTypeEnum_basicTypeSize(local->type.type));
    }
    else
    {
        sz = 4;
    }

    if (self->sp - sz == local->stackAddr.addr)
    {
        // this is the last thing we pushed on
        // free the stack space immediately.
        self->sp -= sz;
    }
}

static inline void BCGen_Initialize(BCGen* self, uint32_t n_args, ...)
{
    self->callCount = 0;
    self->parameterCount = 0;
    self->temporaryCount = 0;
    self->localCount = 0;

    self->byteCodeArray[0] = 0;
    self->byteCodeArray[1] = 0;
    self->byteCodeArray[2] = 0;
    self->byteCodeArray[3] = 0;

    self->ip = 4;
    self->sp = 4;

    self->insideFunction = false;
    self->fd = 0;
}

static inline void BCGen_Finalize(BCGen* self)
{
    assert(self->ip < ARRAY_SIZE(self->byteCodeArray));
    self->finalized = true;
    // TODO write some kind of end marker into the bytecode
}


static inline uint32_t BCGen_beginFunction(BCGen* self, uint32_t fnIdx, void* fd)
{
    assert(self->fd == 0);

    if (!fnIdx)
        fnIdx = ++self->functionCount;

    // emit four zeros as start marker
    BCGen_emit2(self, 0, 0);
    BCGen_emit2(self, 0, 0);

    BCFunction* f = self->functions + (fnIdx - 1);
    f->bytecode_start = self->ip;

    self->insideFunction = true;
    self->functionIdx = fnIdx;
    self->fd = fd;

    return fnIdx;
}

static inline void BCGen_endFunction(BCGen* self, uint32_t fIdx)
{
    assert(self->insideFunction);
    assert(self->functionIdx == fIdx);
    self->insideFunction = false;

    self->localCount = 0;

    BCFunction *f = self->functions + (fIdx - 1);

    f->type = BCFunctionTypeEnum_Bytecode;
    f->maxStackUsed = self->sp;
    f->fn = self->functionIdx;
    f->nArgs = self->parameterCount;
    f->bytecode_end = self->ip;

    self->sp = 4;
    self->fd = 0;
    self->parameterCount = 0;
}

static inline BCValue BCGen_genLocal(BCGen* self, BCType bct, const char* name)
{
    uint32_t sp = self->sp;
    uint16_t localAddr = (uint16_t)sp;
    uint16_t localIdx = ++self->localCount;

    BCValue result = BCVALUE_INIT;
    result.type = bct;
    result.stackAddr.addr = localAddr;
    result.localIndex  = localIdx;
    result.vType = BCValueType_StackValue;

    if (BCType_isBasicBCType(bct))
    {
        sp += align4(BCTypeEnum_basicTypeSize(bct.type));
    }
    else
    {
        sp += 4;
    }
    BCLocal local = {localIdx, bct, {localAddr}, name};

    self->locals[localIdx - 1] = local;

    self->sp = sp;
    return result;
}

static inline BCValue BCGen_genParameter(BCGen* self, BCType bct, const char* name)
{
    BCValue p;

    p.type =  bct;
    p.vType = BCValueType_Parameter;
    p.parameterIndex = ++self->parameterCount;
    p.stackAddr.addr = self->sp;

    self->sp += 4;
    p.name = name;

    return p;
}

static inline uint32_t BCGen_beginJmp(BCGen* self)
{
    uint32_t atIp = self->ip;
    self->ip += 2;

    return atIp;
}
static inline void BCGen_emitArithInstruction(BCGen* self
                                            , LongInst inst
                                            , const BCValue* lhs
                                            , const BCValue* rhs
                                            , BCTypeEnum* resultTypeEnum);

static inline BCValue BCGen_castTo(BCGen* self, const BCValue* rhs, BCTypeEnum targetType)
{
    BCTypeEnum sourceType = rhs->type.type;

    if (sourceType == targetType)
        return *rhs;

    BCType type = {targetType};
    BCValue lhs = BCGen_genTemporary(self, type);

    assert(BCValue_isStackValueOrParameter(rhs));

    switch(targetType)
    {
        case BCTypeEnum_f52 :
            if (sourceType == BCTypeEnum_f23)
            {
                BCGen_emitLongInstSS(self, LongInst_F32ToF64, lhs.stackAddr, rhs->stackAddr);
            }
            else
            {
                BCGen_emitLongInstSS(self, LongInst_IToF64, lhs.stackAddr, rhs->stackAddr);
            }
        break;
        case BCTypeEnum_f23 :
            if (sourceType == BCTypeEnum_f52)
            {
                BCGen_emitLongInstSS(self, LongInst_F64ToF32, lhs.stackAddr, rhs->stackAddr);
            }
            else
            {
                BCGen_emitLongInstSS(self, LongInst_IToF32, lhs.stackAddr, rhs->stackAddr);
            }
        break;
        case BCTypeEnum_i32 :
        case BCTypeEnum_i64 :
            if (sourceType == BCTypeEnum_f23)
            {
                BCGen_emitLongInstSS(self, LongInst_F32ToI, lhs.stackAddr, rhs->stackAddr);
            }
            else if (sourceType == BCTypeEnum_f52)
            {
                BCGen_emitLongInstSS(self, LongInst_F64ToI, lhs.stackAddr, rhs->stackAddr);
            }
        break;
        default :
            assert(0); //, "me no have no cast for targetType " ~ enumToString(targetType));}
        //break;
    }

    return lhs;
}

static inline void BCGen_Set(BCGen* self, BCValue* lhs, const BCValue* rhs)
{
    assert(BCValue_isStackValueOrParameter(lhs));//, "Set lhs is has to be a StackValue. Not: " ~ enumToString(lhs.vType));
    assert(rhs->vType == BCValueType_Immediate || BCValue_isStackValueOrParameter(rhs));//, "Set rhs is has to be a StackValue or Imm not: " ~ rhs.vType.enumToString);

    if (rhs->vType == BCValueType_Immediate)
    {
        if(rhs->type.type == BCTypeEnum_i64 || rhs->type.type == BCTypeEnum_u64 || rhs->type.type == BCTypeEnum_f52)
        {
            BCGen_emitLongInstSI(self, LongInst_SetImm32, lhs->stackAddr, (rhs->imm64.imm64 & UINT32_MAX));
            BCGen_emitLongInstSI(self, LongInst_SetHighImm32, lhs->stackAddr, (rhs->imm64.imm64 >> 32));
        }
        else
        {
            BCGen_emitLongInstSI(self, LongInst_SetImm32, lhs->stackAddr, rhs->imm32.imm32);
        }
    }
    else if (!BCValue_eq(lhs, rhs)) // do not emit self assignments;
    {
        BCGen_emitArithInstruction(self, LongInst_Set, lhs, rhs, 0);
    }
}

static inline BCValue BCGen_pushTemporary(BCGen* self, const BCValue* val)
{
    if (!BCValue_isStackValueOrParameter(val))
    {
        BCValue stackref;

        stackref.vType = BCValueType_StackValue;
        stackref.stackAddr.addr = self->sp;
        stackref.temporaryIndex = ++self->temporaryCount;

        BCGen_Set(self, &stackref, val);
        stackref.type = val->type;

        self->sp += align4(BCTypeEnum_basicTypeSize(val->type.type));
        return stackref;
    }
    else
    {
        return *val;
    }
}

static inline void BCGen_emitArithInstruction(BCGen* self
                                            , LongInst inst
                                            , const BCValue* lhsP
                                            , const BCValue* rhsP
                                            , BCTypeEnum* resultTypeEnum)
{
    assert(inst >= LongInst_Add && inst < LongInst_ImmAdd); //,
//        "Instruction is not in Range for Arith Instructions");

    BCTypeEnum lhs_type_type = lhsP->type.type;
    BCTypeEnum rhs_type_type = rhsP->type.type;

    BCTypeEnum commonType = BCTypeEnum_commonTypeEnum(lhs_type_type, rhs_type_type);
    bool pushedLhs = 0, pushedRhs = 0;

    // FIXME Implement utf8 <-> utf32 conversion
    assert(commonType == BCTypeEnum_i32 || commonType == BCTypeEnum_i64
        || commonType == BCTypeEnum_u32 || commonType == BCTypeEnum_u64
        || commonType == BCTypeEnum_f23 || commonType == BCTypeEnum_c32
        || commonType == BCTypeEnum_c8  || commonType == BCTypeEnum_f52);//,
    //    "only i32, i64, f23, f52, is supported for now not: " ~ enumToString(commonType));
    //assert(lhs.type.type == rhs.type.type, enumToString(lhs.type.type) ~ " != " ~ enumToString(rhs.type.type));

    BCValueType lhs_vType = lhsP->vType;
    BCValueType rhs_vType = rhsP->vType;

    BCValue lhs;
    BCValue rhs;

    if (lhs_vType == BCValueType_Immediate)
    {
        lhs = BCGen_pushTemporary(self, lhsP);
        pushedLhs |= 1;
        lhsP = &lhs;
    }

    if (resultTypeEnum)
        *resultTypeEnum = commonType;

    if (lhs_type_type == BCTypeEnum_f23)
    {
        if(rhs_type_type == BCTypeEnum_i32 || rhs_type_type == BCTypeEnum_u32)
        {
            if (rhs_vType == BCValueType_Immediate)
            {
                float frhs = (float) rhsP->imm32.imm32;
                rhs = imm32(*(int32_t*)&frhs);
                rhsP = &rhs;
            }
            else
            {
                rhs = BCGen_castTo(self, rhsP, BCTypeEnum_f23);
                rhsP = &rhs;
            }
        }
        else if (rhs_type_type == BCTypeEnum_f23)
        {
            rhs = BCGen_pushTemporary(self, rhsP);
            pushedRhs |= true;
            rhsP = &rhs;
        }
        else if (rhs_type_type == BCTypeEnum_f52)
        {
            rhs = BCGen_castTo(self, rhsP, lhs_type_type);
            rhsP = &rhs;
        }
        else
            assert(0);//, "did not expect type " ~ enumToString(rhs.type.type) ~ "to be used in a float expression");
        if (inst != LongInst_Set)
        {
            int t = (int)inst;
            t += ((int)LongInst_FAdd32 - (int)LongInst_Add);
            inst = (LongInst)t;
        }
    }
    else if (lhs_type_type == BCTypeEnum_f52)
    {
        if(rhs_type_type != BCTypeEnum_f52)
        {
            // TOOD there was
            // assert (rhs.type.type == BCTypeEnum_f52)
            // here before .... check if this is an invariant
            rhs = BCGen_castTo(self, rhsP, BCTypeEnum_f52);
            rhsP = &rhs;
        }

        rhs = BCGen_pushTemporary(self, rhsP);
        if (inst != LongInst_Set)
        {
            int t = (int)inst;
            t += ((int)LongInst_FAdd64 - (int)LongInst_Add);
            inst = (LongInst)t;
        }
    }
    else if (rhs_vType == BCValueType_Immediate)
    {
        const int64_t imm64s = (BCTypeEnum_basicTypeSize(rhs_type_type) <= 8 ? cast(int64_t)rhsP->imm64.imm64 : 0);
        if  (BCTypeEnum_basicTypeSize(rhs_type_type) <= 4 || (imm64s <= INT32_MAX && imm64s > -INT32_MAX))
        {
            //Change the instruction into the corresponding Imm Instruction;
            if (inst != LongInst_Set)
            {
                int t = (int) inst;
                t += ((int)LongInst_ImmAdd - (int)LongInst_Add);
                inst = (LongInst)t;
            }
            else
            {
                inst = LongInst_SetImm32;
            }
            BCGen_emitLongInstSI(self, inst, lhsP->stackAddr, rhsP->imm32.imm32);
            goto Lreturn;
        }
        else
        {
            rhs = BCGen_pushTemporary(self, rhsP);
            pushedRhs |= true;
            rhsP = &rhs;
        }
    }

    if (BCValue_isStackValueOrParameter(rhsP))
    {
        BCGen_emitLongInstSS(self, inst, lhsP->stackAddr, rhsP->stackAddr);
    }
    else
    {
        assert(0);//, "Cannot handle: " ~ enumToString(rhs.vType));
    }
Lreturn:
    if (pushedRhs)
        BCGen_destroyTemporary(self, cast(BCValue*)rhsP);

    if (pushedLhs)
        BCGen_destroyTemporary(self, cast(BCValue*)lhsP);
}

static inline void BCGen_emitFlag(BCGen* self, BCValue* lhs)
{
    assert(BCValue_isStackValueOrParameter(lhs));
    BCGen_emit2(self, BCGen_ShortInst16(LongInst_Flg, lhs->stackAddr.addr), 0);
}

static inline void BCGen_Cmp(BCGen* self, BCValue *result, const BCValue *lhs, const BCValue *rhs, LongInst cmp)
{
    assert(!result
        || BCValue_isStackValueOrParameter(result));

    BCGen_emitArithInstruction(self, cmp, lhs, rhs, 0);

    if (result)
    {
        BCGen_emitFlag(self, result);
    }
}

#define BC_CMP_FUNC(OP) \
    static inline void BCGen_##OP##3(BCGen* self, BCValue *result, const BCValue* lhs, const BCValue* rhs) \
    { BCGen_Cmp(self, result, lhs, rhs, LongInst_##OP); }

BC_CMP_FUNC(Gt)
BC_CMP_FUNC(Ugt)
BC_CMP_FUNC(Ge)
BC_CMP_FUNC(Uge)
BC_CMP_FUNC(Lt)
BC_CMP_FUNC(Ult)
BC_CMP_FUNC(Le)
BC_CMP_FUNC(Ule)
BC_CMP_FUNC(Eq)
BC_CMP_FUNC(Neq)

static inline void BCGen_Arith(BCGen* self, BCValue *result, const BCValue* lhs, const BCValue* rhs, LongInst inst)
{
    assert(inst >= LongInst_Add && inst < LongInst_ImmAdd);
    assert(result->vType != BCValueType_Immediate); //, "Cannot add to Immediate");

    if (lhs != result || !BCValue_eq(lhs, result))
    {
        BCGen_Set(self, result, lhs);
    }

    assert(result->vType != BCValueType_Unknown);
    BCGen_emitArithInstruction(self, inst, result, rhs, &result->type.type);
}


#define BC_ARITH_FUNC(OP) \
    static inline void BCGen_##OP##3(BCGen* self, BCValue *result, const BCValue* lhs, const BCValue* rhs) \
    {     BCGen_Arith(self, result, lhs, rhs, LongInst_##OP); }

BC_ARITH_FUNC(Add)
BC_ARITH_FUNC(Sub)
BC_ARITH_FUNC(Mul)
BC_ARITH_FUNC(Div)
BC_ARITH_FUNC(Udiv)
BC_ARITH_FUNC(Mod)
BC_ARITH_FUNC(Umod)
BC_ARITH_FUNC(Or)
BC_ARITH_FUNC(Xor)
BC_ARITH_FUNC(Rsh)
BC_ARITH_FUNC(Lsh)
BC_ARITH_FUNC(And)

static inline void BCGen_Load_Store(BCGen* self, BCValue *to, const BCValue* from, LongInst inst)
{
    _Bool pushedFrom = 0;
    _Bool pushedTo = 0;
    BCValue fromV;
    BCValue toV;

    if (!BCValue_isStackValueOrParameter(from))
    {
        pushedFrom |= 1;
        fromV = BCGen_pushTemporary(self, from);
        from = &fromV;
    }

    if (!BCValue_isStackValueOrParameter(to))
    {
        pushedTo |= 1;
        toV = BCGen_pushTemporary(self, to);
        to = &toV;
    }

    BCGen_emitLongInstSS(self, inst, to->stackAddr, from->stackAddr);

    if (pushedTo) BCGen_destroyTemporary(self, (BCValue*)to);
    if (pushedFrom) BCGen_destroyTemporary(self, (BCValue*)from);
}

#define BC_STORE_FUNC(SZ) \
static inline void BCGen_Store##SZ(BCGen* self, BCValue *to, const BCValue* value) \
    { BCGen_Load_Store(self, to, value, LongInst_HeapStore##SZ); }

BC_STORE_FUNC(8)
BC_STORE_FUNC(16)
BC_STORE_FUNC(32)
BC_STORE_FUNC(64)

#define BC_LOAD_FUNC(SZ) \
static inline void BCGen_Load##SZ(BCGen* self, BCValue *to, const BCValue* from) \
    { BCGen_Load_Store(self, to, from, LongInst_HeapLoad##SZ); }

BC_LOAD_FUNC(8)
BC_LOAD_FUNC(16)
BC_LOAD_FUNC(32)
BC_LOAD_FUNC(64)

static inline void BCGen_MemCpy(BCGen* self, BCValue *dst, const BCValue* src, const BCValue* size)
{
    bool pushedSize = (size->vType == BCValueType_Immediate);
    bool pushedDst = (dst->vType == BCValueType_Immediate);
    bool pushedSrc = (src->vType == BCValueType_Immediate);

    BCValue newSize = BCGen_pushTemporary(self, size);
    BCValue newSrc = BCGen_pushTemporary(self, src);
    BCValue newDst = BCGen_pushTemporary(self, dst);

    BCGen_emitLongInstSSS(self, LongInst_MemCpy, newSize.stackAddr, newSrc.stackAddr, newDst.stackAddr);

    if (pushedDst) BCGen_destroyTemporary(self, &newDst);
    if (pushedSrc) BCGen_destroyTemporary(self, &newSrc);
    if (pushedSize) BCGen_destroyTemporary(self, &newSize);
}

static inline void BCGen_Ret(BCGen* self, const BCValue* val)
{
    LongInst inst = ((BCTypeEnum_basicTypeSize(val->type.type) == 8) ? LongInst_Ret64 : LongInst_Ret32);
    _Bool newValTemp = 0;
    BCValue newVal;
    uint32_t hi = 0;

    if (val->vType == BCValueType_Immediate)
    {
        if (inst == LongInst_Ret32 && val->imm32.imm32 != INT32_MIN)
        {
            if (val->imm32.imm32 == 0)
                hi = INT32_MIN;
            else
                hi = val->imm32.imm32;
        }
        else
        {
            newVal = BCGen_pushTemporary(self, val);
            val = &newVal;
            newValTemp |= 1;
        }
    }

    if (BCValue_isStackValueOrParameter(val)
     || (val->vType == BCValueType_Immediate))
    {
        BCGen_emit2(self, BCGen_ShortInst16(inst, val->stackAddr.addr), hi);
    }
    else
    {
        assert(0);//, "I cannot deal with this type of return" ~ enumToString(val.vType));
    }

    if (newValTemp)
        BCGen_destroyTemporary(self, (BCValue*)val);
}

static inline BCValue BCGen_run(BCGen* self, uint32_t fnIdx,
                                BCValue* args, uint32_t n_args, BCHeap* heap)
{
    BCValue result;

    assert(self->finalized);

    result = BCGen_interpret(self, fnIdx, args, n_args, heap);

    return result;
}

void BCGen_endJmp(BCGen* self, BCAddr atIp, BCLabel target)
{
    uint16_t offset = BCGen_isShortJump(target.addr.addr - atIp.addr);
    if (offset)
    {
        BCGen_emit2_at(self, BCGen_ShortInst16(LongInst_RelJmp, offset), 0, atIp.addr);
    }
    else
    {
        BCGen_emit2_at(self, LongInst_Jmp, target.addr.addr, atIp.addr);
    }
}

#undef FLT32_BEGIN
#undef FLT32_END

#undef FLT64_BEGIN
#undef FLT64_END

#if 0
{
    void Prt(BCValue value, bool isString = false)
    {
        if (value.vType == BCValueType_Immediate)
            value = BCGen_pushTemporary(self, value);

        byteCodeArray[ip] = ShortInst16Ex(LongInst_PrintValue, isString, value.stackAddr);
        byteCodeArray[ip + 1] = 0;
        ip += 2;
    }


    void Call(BCValue result, BCValue fn, BCValue[] args)
    {
        auto call_id = BCGen_pushTemporary(imm32(callCount + 1)).stackAddr;
        calls[callCount++] = RetainedCall(fn, args, functionIdx, ip, sp);
        emitLongInst(LongInst_Call, result.stackAddr, call_id);

    }

    void Throw(BCValue e)
    {
        assert(BCValue_isStackValueOrParameter(&e));
        byteCodeArray[ip] = ShortInst16(LongInst_Throw, e.stackAddr);
        byteCodeArray[ip + 1] = 0;
        ip += 2;
    }

    void PushCatch()
    {
        byteCodeArray[ip] = ShortInst16(LongInst_PushCatch, 0);
        byteCodeArray[ip + 1] = 0;
        ip += 2;
    }

    void PopCatch()
    {
        byteCodeArray[ip] = ShortInst16(LongInst_PopCatch, 0);
        byteCodeArray[ip + 1] = 0;
        ip += 2;
    }

/+
    void Push(BCValue v)
    {
        const sz = BCTypeEnum_basicTypeSize(v.typ.type);
        assert(sz >= 1 && sz <= 4);
        if (v.vType == BCValueType_Immediate)
        {
            byteCodeArray[ip] = LongInst_PushImm32;
            byteCodeArray[ip + 1] = v.imm32.imm32;
        }
        else
        {
            byteCodeArray[ip] = ShortInst16(LongInst_Push32, v.stackAddr);
            byteCodeArray[ip + 1] = 0;
        }
        ip += 2;
    }
+/

    void Memcmp(BCValue result, BCValue lhs, BCValue rhs)
    {
        assert(result.vType == BCValueType_Unknown
            || BCValue_isStackValueOrParameter(&result),
            "The result for this must be Empty or a StackValue not: " ~ enumToString(result.vType));
        if (lhs.vType == BCValueType_Immediate)
        {
            lhs = BCGen_pushTemporary(lhs);
            pushedLhs |= 1;
        }
        if (rhs.vType == BCValueType_Immediate)
        {
            rhs = BCGen_pushTemporary(rhs);
            pushedRhs |= 1;
        }
        assert(BCValue_isStackValueOrParameter(&lhs),
            "The lhs of Memcmp is not a StackValue " ~ enumToString(rhs.vType));
        assert(BCValue_isStackValueOrParameter(&rhs),
            "The rhs of Memcmp not a StackValue" ~ enumToString(rhs.vType));

        emitLongInst(LongInst_Memcmp, lhs.stackAddr, rhs.stackAddr);

        if (BCValue_isStackValueOrParameter(&result))
        {
            emitFlag(result);
        }
    }

    void Realloc(BCValue result, BCValue lhs, BCValue rhs, const uint size)
    {
        assert(size <= 255);

        assert(BCValue_isStackValueOrParameter(&result));

        lhs = BCGen_pushTemporary(lhs);
        rhs = BCGen_pushTemporary(rhs);
        emitLongInst(LongInst_Realloc, result.stackAddr, lhs.stackAddr, rhs.stackAddr);
        // Hack! we have no overload to store additional information in the 8 bit
        // after the inst so just dump it in there let's hope we don't overwrite
        // anything important
        byteCodeArray[ip-2] |= (size & 255) << 8;
    }

}
#endif

static inline void BCGen_InitializeV(BCGen* self, uint32_t n_args, va_list args)
{
    // ignore any additonal args
    // infact require n_args to be 0
    assert(n_args == 0);
    BCGen_Initialize(self, n_args);
}

static inline void BCGen_Alloc(BCGen* self, BCValue *heapPtr, const BCValue* size)
{
    BCValue newSize;
    assert(size->type.type == BCTypeEnum_u32);
    _Bool pushedSize = 0;

    if (size->vType == BCValueType_Immediate)
    {
        newSize = BCGen_pushTemporary(self, size);
        size = &newSize;
        pushedSize |= 1;
    }

    assert(BCValue_isStackValueOrParameter(size));
    assert(BCValue_isStackValueOrParameter(heapPtr));

    BCGen_emitLongInstSS(self, LongInst_Alloc, heapPtr->stackAddr, size->stackAddr);
    if (pushedSize)
        BCGen_destroyTemporary(self, cast(BCValue*)size);

}

static inline void BCGen_Assert(BCGen* self, const BCValue* value, const BCValue* err)
{
/*
    assert((err->vType == BCValueType_Error
        || err->vType == BCValueType_Immediate)
        && (err->type.type == BCTypeEnum_i32
        || err->type.type == BCTypeEnum_u32));
*/
    assert(BCValue_isStackValueOrParameter(value));
    {
        BCGen_emitLongInstSI(self, LongInst_Assert, value->stackAddr, err->imm32.imm32);
    }
}

void BCGen_outputBytes (BCGen* self, const uint8_t* bytes, uint32_t length)
{
    uint32_t idx = 0;

    while (length >= 8)
    {
        BCGen_emit2(self,
            bytes[idx+0] << 0 | bytes[idx+1] << 8 | bytes[idx+2] << 16 | bytes[idx+3] << 24,
            bytes[idx+4] << 0 | bytes[idx+5] << 8 | bytes[idx+6] << 16 | bytes[idx+7] << 24
        );

        idx += 8;
        length -= 8;
    }

    uint32_t lastField1 = 0;
    uint32_t lastField2 = 0;

    switch(length)
    {
        case 7 :
            lastField2 |= bytes[idx+6] << 16;
        case 6 :
            lastField2 |= bytes[idx+5] << 8;
        case 5 :
            lastField2 |= bytes[idx+4] << 0;
        case 4 :
            lastField1 |= bytes[idx+3] << 24;
        case 3 :
            lastField1 |= bytes[idx+2] << 16;
        case 2 :
            lastField1 |= bytes[idx+1] << 8;
        case 1 :
            lastField1 |= bytes[idx+0] << 0;
            BCGen_emit2(self, lastField1, lastField2);
        case 0 :
            break;
    }
}


static inline void BCGen_File(BCGen* self, const char* filename)
{
    uint32_t filenameLength = cast(uint32_t) strlen(filename);
    const StackAddr a = {0};
    BCGen_emitLongInstSI(self, LongInst_File, a, filenameLength);
    BCGen_outputBytes(self, cast(uint8_t*)filename, filenameLength);
}

static inline void BCGen_Line(BCGen* self, uint32_t line)
{
   const StackAddr a = {0};
   BCGen_emitLongInstSI(self, LongInst_Line, a, line);
}

static inline void BCGen_Comment(BCGen* self, const char* comment)
{
    uint32_t commentLength = cast(uint32_t) strlen(comment);
    const StackAddr a = {0};
    BCGen_emitLongInstSI(self, LongInst_Comment, a, commentLength);

    BCGen_outputBytes(self, cast(uint8_t*)comment, commentLength);
}

static inline void BCGen_Prt(BCGen* self, const BCValue* value, bool isString)
{
}

static inline void BCGen_Not(BCGen* self, BCValue *result, const BCValue* val)
{
    BCValue newVal;

    if (result != val && !BCValue_eq(result, val))
    {
        BCGen_Set(self, result, val);
        val = result;
    }
    else if (val->vType == BCValueType_Immediate)
    {
        newVal = BCGen_pushTemporary(self, val);
        val = &newVal;
    }

    BCGen_emit2(self, BCGen_ShortInst16(LongInst_Not, val->stackAddr.addr), 0);
}

static inline void BCGen_Call(BCGen* self, BCValue *result, const BCValue* fn, BCValue* args, uint32_t n_args)
{
    assert(BCValue_isStackValueOrParameter(result));
}

static inline BCLabel BCGen_genLabel(BCGen* self)
{
    BCLabel result = {{self->ip}};
    return result;
}

static inline void BCGen_Jmp(BCGen* self, BCLabel target)
{
    assert(target.addr.addr);
    if (self->ip != target.addr.addr && self->ip != target.addr.addr - 2)
    {
        BCAddr at = {BCGen_beginJmp(self)};
        BCGen_endJmp(self, at, target);
    }
}

static inline CndJmpBegin BCGen_beginCndJmp(BCGen* self, const BCValue* cond, bool ifTrue)
{
    assert(!cond ||
           cond->vType != BCValueType_Immediate);

    CndJmpBegin result = {{self->ip}, cast(BCValue*)cond, ifTrue};
    self->ip += 2;
    return result;
}

static inline void BCGen_ReadI32(BCGen* self, const BCValue* value, ReadI32_cb_t ReadI32_cb, void* userCtx)
{
    assert(self->contextCount < self->contextCapacity);

    ReadI32_ctx_t ctx = { .cb = ReadI32_cb,  .userCtx = userCtx };
    uint32_t ptr = self->contextCount;
    self->contexts[self->contextCount++] = ctx;
    assert(BCValue_isStackValueOrParameter(value));

    BCGen_emitLongInstSI(self, LongInst_ReadI32, value->stackAddr, ptr);
}

static inline void BCGen_endCndJmp(BCGen* self, const CndJmpBegin* jmp, BCLabel target)
{
    uint32_t atIp = jmp->at.addr;
    const BCValue* cond = jmp->cond;
    bool ifTrue = jmp->ifTrue;

    uint32_t low_word =  (uint32_t)(ifTrue ? LongInst_JmpTrue : LongInst_JmpFalse);
    uint32_t high_word = target.addr.addr;

    if (cond)
    {
        assert(BCValue_isStackValueOrParameter(cond));
        low_word = (ifTrue ? LongInst_JmpNZ : LongInst_JmpZ)
                   | (cond->stackAddr.addr << 16);
    }

    BCGen_emit2_at(self, low_word, high_word, atIp);
}

static inline void BCGen_Throw(BCGen* self, const BCValue* e)
{
}

static inline void BCGen_PushCatch(BCGen* self)
{
}

static inline void BCGen_PopCatch(BCGen* self)
{
}

static inline void BCGen_FPConv(BCGen* self, BCValue* result, const BCValue* rhs, LongInst Inst)
{
    assert(BCValue_isStackValueOrParameter(result));
    assert(BCValue_isStackValueOrParameter(rhs));

    BCGen_emitLongInstSS(self, Inst, result->stackAddr, rhs->stackAddr);
}

#define BC_FP_CONV_FUNC(OP) \
static inline void BCGen_##OP(BCGen* self, BCValue* result, const BCValue* rhs) \
    { BCGen_FPConv(self, result, rhs, LongInst_##OP); }

BC_FP_CONV_FUNC(IToF32)
BC_FP_CONV_FUNC(IToF64)
BC_FP_CONV_FUNC(F32ToI)
BC_FP_CONV_FUNC(F64ToI)
BC_FP_CONV_FUNC(F32ToF64)
BC_FP_CONV_FUNC(F64ToF32)

static inline void BCGen_Memcmp(BCGen* self, BCValue *result, const BCValue* lhs, const BCValue* rhs)
{
}

static inline void BCGen_Realloc(BCGen* self, BCValue *result, const BCValue* lhs, const BCValue* rhs, const uint32_t size)
{
}

static inline void BCGen_LoadFramePointer(BCGen* self, BCValue *result, const int32_t offset)
{
    assert(BCValue_isStackValueOrParameter(result));
    BCGen_emitLongInstCtxM(self, CtxM_GetFramePointer, result->stackAddr, offset);
}

#ifdef __cplusplus
extern "C"
#endif
const BackendInterface BCGen_interface = {
    /*.name = */ "Bytecode Interpreter (BCGen)",

    /*.Initialize =*/ (Initialize_t) BCGen_Initialize,

    /*.InitializeV =*/ (InitializeV_t) BCGen_InitializeV,
    /*.Finalize =*/ (Finalize_t) BCGen_Finalize,
    /*.BeginFunction =*/ (BeginFunction_t) BCGen_beginFunction,
    /*.EndFunction =*/ (EndFunction_t) BCGen_endFunction,
    /*.GenTemporary =*/ (GenTemporary_t) BCGen_genTemporary,
    /*.DestroyTemporary =*/ (DestroyTemporary_t) BCGen_destroyTemporary,
    /*.GenLocal =*/ (GenLocal_t) BCGen_genLocal,
    /*.DestroyLocal =*/ (DestroyLocal_t) BCGen_destroyLocal,
    /*.GenParameter =*/ (GenParameter_t) BCGen_genParameter,
    /*.EmitFlag =*/ (EmitFlag_t) BCGen_emitFlag,
    /*.Alloc =*/ (Alloc_t) BCGen_Alloc,
    /*.Assert =*/ (Assert_t) BCGen_Assert,
    /*.MemCpy =*/ (MemCpy_t) BCGen_MemCpy,
    /*.File =*/ (File_t) BCGen_File,
    /*.Line =*/ (Line_t) BCGen_Line,
    /*.Comment =*/ (Comment_t) BCGen_Comment,
    /*.Prt =*/ (Prt_t) BCGen_Prt,
    /*.Set =*/ (Set_t) BCGen_Set,
    /*.Ult3 =*/ (Ult3_t) BCGen_Ult3,
    /*.Ule3 =*/ (Ule3_t) BCGen_Ule3,
    /*.Lt3 =*/ (Lt3_t) BCGen_Lt3,
    /*.Le3 =*/ (Le3_t) BCGen_Le3,
    /*.Ugt3 =*/ (Ugt3_t) BCGen_Ugt3,
    /*.Uge3 =*/ (Uge3_t) BCGen_Uge3,
    /*.Gt3 =*/ (Gt3_t) BCGen_Gt3,
    /*.Ge3 =*/ (Ge3_t) BCGen_Ge3,
    /*.Eq3 =*/ (Eq3_t) BCGen_Eq3,
    /*.Neq3 =*/ (Neq3_t) BCGen_Neq3,
    /*.Add3 =*/ (Add3_t) BCGen_Add3,
    /*.Sub3 =*/ (Sub3_t) BCGen_Sub3,
    /*.Mul3 =*/ (Mul3_t) BCGen_Mul3,
    /*.Div3 =*/ (Div3_t) BCGen_Div3,
    /*.Udiv3 =*/ (Udiv3_t) BCGen_Udiv3,
    /*.And3 =*/ (And3_t) BCGen_And3,
    /*.Or3 =*/ (Or3_t) BCGen_Or3,
    /*.Xor3 =*/ (Xor3_t) BCGen_Xor3,
    /*.Lsh3 =*/ (Lsh3_t) BCGen_Lsh3,
    /*.Rsh3 =*/ (Rsh3_t) BCGen_Rsh3,
    /*.Mod3 =*/ (Mod3_t) BCGen_Mod3,
    /*.Umod3 =*/ (Umod3_t) BCGen_Umod3,
    /*.Not =*/ (Not_t) BCGen_Not,
    /*.LoadFramePointer =*/ (LoadFramePointer_t) BCGen_LoadFramePointer,
    /*.Call =*/ (Call_t) BCGen_Call,
    /*.GenLabel =*/ (GenLabel_t) BCGen_genLabel,
    /*.Jmp =*/ (Jmp_t) BCGen_Jmp,
    /*.BeginJmp =*/ (BeginJmp_t) BCGen_beginJmp,
    /*.EndJmp =*/ (EndJmp_t) BCGen_endJmp,
    /*.BeginCndJmp =*/ (BeginCndJmp_t) BCGen_beginCndJmp,
    /*.EndCndJmp =*/ (EndCndJmp_t) BCGen_endCndJmp,
    /*.Load8 =*/ (Load8_t) BCGen_Load8,
    /*.Store8 =*/ (Store8_t) BCGen_Store8,
    /*.Load16 =*/ (Load16_t) BCGen_Load16,
    /*.Store16 =*/ (Store16_t) BCGen_Store16,
    /*.Load32 =*/ (Load32_t) BCGen_Load32,
    /*.Store32 =*/ (Store32_t) BCGen_Store32,
    /*.Load64 =*/ (Load64_t) BCGen_Load64,
    /*.Store64 =*/ (Store64_t) BCGen_Store64,
    /*.Throw =*/ (Throw_t) BCGen_Throw,
    /*.PushCatch =*/ (PushCatch_t) BCGen_PushCatch,
    /*.PopCatch =*/ (PopCatch_t) BCGen_PopCatch,
    /*.Ret =*/ (Ret_t) BCGen_Ret,
    /*.IToF32 =*/ (IToF32_t) BCGen_IToF32,
    /*.IToF64 =*/ (IToF64_t) BCGen_IToF64,
    /*.F32ToI =*/ (F32ToI_t) BCGen_F32ToI,
    /*.F64ToI =*/ (F64ToI_t) BCGen_F64ToI,
    /*.F32ToF64 =*/ (F32ToF64_t) BCGen_F32ToF64,
    /*.F64ToF32 =*/ (F64ToF32_t) BCGen_F64ToF32,
    /*.Memcmp =*/ (Memcmp_t) BCGen_Memcmp,
    /*.Realloc =*/ (Realloc_t) BCGen_Realloc,
    /*.Run =*/ (run_t) BCGen_run,
    /*.ReadI32 =*/ (ReadI32_t) BCGen_ReadI32,

    /*.sizeof_instance =*/ BCGen_sizeof_instance,
    /*.clear_instance =*/ (clear_instance_t) BCGen_clear_instance,
    /*.init_instance =*/ (init_instance_t) BCGen_init_instance,
    /*.fini_instance =*/ (fini_instance_t) BCGen_fini_instance,

    /*.set_alloc_memory =*/ (set_alloc_memory_t) BCGen_set_alloc_memory,
    /*.set_get_typeinfo =*/ (set_get_typeinfo_t) 0,
};

#endif
