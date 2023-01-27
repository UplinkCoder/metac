#include "bc_common.h"
#include "backend_interface_funcs.h"
#include <stdlib.h>

typedef struct JsBackend
{
    char* Code;
    uint32_t CodeCount;
    uint32_t CodeCapacity;

    uint32_t CurrentFunctionId;

    const char* ParameterNames[32];
    BCType ParameterTypes[32];
    uint16_t ParamterCount;

    uint16_t TemporaryCount;
    uint16_t LabelCount;
    uint16_t staticBfUsed;

    bool InFunction;

    alloc_fn_t allocFn;
    void* allocCtx;

    get_typeinfo_fn_t getTypeInfoFn;
    void* getTypeInfoCtx;

    char staticBf[256];

} JsBackend;

BCTypeInfo* LookupTypeinfo(JsBackend* self, BCType bct)
{
    BCTypeInfo* result = 0;

    return result;
}

static inline int PrintF(JsBackend* self, const char* format, ...)
{
    va_list args;
    int n;
    int32_t sizeLeft = cast(int32_t) (self->CodeCapacity - self->CodeCount);
    va_start (args, format);

    if (sizeLeft < 1024)
    LGrow:
    {

        uint32_t newCapa = (self->CodeCapacity * 1.3f);
        self->Code = realloc(self->Code, newCapa);
        self->CodeCapacity = newCapa;
        sizeLeft = cast(int32_t) (self->CodeCapacity - self->CodeCount);
    }

/*
    n = vsnprintf(self->Code + self->CodeCount, sizeLeft,
                  format, args);

    if (n > sizeLeft)
    {
        goto LGrow;
    }

    self->CodeCount += n;
    self->Code += n;
*/
    vprintf(format, args);
    self->staticBfUsed = 0;

    va_end (args);


    return n;
}

static inline const char* GetVarName(JsBackend* self, BCValue* val)
{
    return 0;
}


static inline const char* GetValue(JsBackend* self, BCValue* val)
{
    const char* result = 0;

    uint32_t sz = 0;

    switch(val->type.type)
    {
        case BCTypeEnum_i32:
        {
            result = self->staticBf + self->staticBfUsed;
            sz = sprintf(result, "%d", val->imm32.imm32);
            self->staticBfUsed += sz + 1;
        }
        break;
    }

    return result;
}

static inline const char* GetFunctionName(JsBackend* self, BCValue* fn)
{
    return 0;
}

#define SMALLINT(T) \
    ((T.type >= BCTypeEnum_i8) & (T.type <= BCTypeEnum_i32))

#define FLOATINGP(T) \
    ((T.type == BCTypeEnum_f23) | (T.type == BCTypeEnum_f52))

static inline bool AreSmallIntegers(BCType t1, BCType t2)
{
    bool result = false;

    result = (SMALLINT(t1) & SMALLINT(t2));

    return result;
}

static inline bool DirectEmit(BCType t1, BCType t2)
{
    bool result = false;

    if (SMALLINT(t1) & SMALLINT(t2))
    {
        result = true;
    }
    else if (FLOATINGP(t1) & FLOATINGP(t2))
    {
        result = true;
    }

    return result;
}

static inline void JsBackend_Initialize(JsBackend* self, uint32_t n_args, ...)
{
    self->CodeCount = 0;
    self->CurrentFunctionId = 0;
    self->ParamterCount = 0;
    self->TemporaryCount = 0;
    self->InFunction = 0;
    self->LabelCount = 0;
}

const char* JsInitializer(JsBackend* self, BCType bct)
{
    BCTypeInfo* tInfo = LookupTypeinfo(self, bct);
}

static inline void JsBackend_InitializeV(JsBackend* self, uint32_t n_args, va_list args)
{
}

static inline void JsBackend_Finalize(JsBackend* self)
{
}

static inline uint32_t JsBackend_BeginFunction(JsBackend* self, uint32_t fnId, const void* fd)
{
    assert(self->CurrentFunctionId == 0);

}

static inline void* JsBackend_EndFunction(JsBackend* self, uint32_t fnIdx)
{
}

static inline BCValue JsBackend_GenTemporary(JsBackend* self, BCType bct)
{
    PrintF(self, "var tmp_%x = %s;\n", ++self->TemporaryCount, JsInitializer(self, bct));
}

static inline void JsBackend_DestroyTemporary(JsBackend* self, BCValue* tmp)
{
}

static inline BCValue JsBackend_GenLocal(JsBackend* self, BCType bct, const char* name)
{
    PrintF(self, "var %s = %s;\n", name, JsInitializer(self, bct));
}

static inline void JsBackend_DestroyLocal(JsBackend* self, BCValue* local)
{
}

static inline BCValue JsBackend_GenParameter(JsBackend* self, BCType bct, const char* name)
{
    assert(!self->InFunction);
    uint16_t parameterCount = self->ParamterCount++;
    BCValue result = {BCValueType_Parameter};

    self->ParameterNames[parameterCount] = name;
    self->ParameterTypes[parameterCount] = bct;

    result.parameterIndex = parameterCount;
    result.type = bct;

    return result;
}

static inline BCValue JsBackend_GenExternal(JsBackend* self, BCType bct, const char* name)
{
}

static inline void JsBackend_MapExternal(JsBackend* self, BCValue* result, void* memory, uint32_t sz)
{
}

static inline BCValue JsBackend_GenExternalFunc(JsBackend* self, BCType bct, const char* name)
{
}

static inline BCValue JsBackend_MapExternalFunc(JsBackend* self, BCValue* result, BCValue* funcP)
{
}

static inline void JsBackend_EmitFlag(JsBackend* self, BCValue* lhs)
{
    const char* varName = GetVarName(self, lhs);
    PrintF(self, "%s = _Flag;", varName);
}

static inline void JsBackend_Alloc(JsBackend* self, BCValue *heapPtr, const BCValue* size)
{
}

static inline void JsBackend_Assert(JsBackend* self, const BCValue* value, const BCValue* err)
{
    PrintF(self, "_Runtime.assert(%s, %s)");
}

static inline void JsBackend_MemCpy(JsBackend* self, const BCValue* dst, const BCValue* src, const BCValue* size)
{
    const char* dstV = GetVarName(self, dst);
    const char* srcV = GetVarName(self, src);
    const char* n = GetValue(self, size);

    PrintF(self, "_Runtime.memcpy(%s, %s, %s)", dstV, srcV, n);
}

static inline void JsBackend_File(JsBackend* self, const char* filename)
{
    PrintF(self, "_Runtime.setFile(\"%s\")", filename);
}

static inline void JsBackend_Line(JsBackend* self, uint32_t line)
{
    PrintF(self, "_Runtime.setLine(%u)", line);
}

static inline void JsBackend_Comment(JsBackend* self, const char* comment)
{
    PrintF(self, "\n/*%s*/\n", comment);
}

static inline void JsBackend_Prt(JsBackend* self, const BCValue* value, bool isString)
{
    assert(0);
}

static inline void JsBackend_Set(JsBackend* self, BCValue *lhs, const BCValue* rhs)
{
    const char* varName = GetVarName(self, lhs);
    const char* value = GetValue(self, rhs);

    PrintF(self, "%s = %s;\n", varName, value);
}

static inline void JsBackend_Ult3(JsBackend* self, BCValue *result, const BCValue* lhs, const BCValue* rhs)
{
    const char* e1 = GetValue(self, lhs);
    const char* e2 = GetValue(self, rhs);
    const char* targetVar = result ? GetValue(self, result) : "_Flag";

    PrintF(self, "%s = _Runtime.ult(%s, %s));\n", targetVar, e1, e2);
}

static inline void JsBackend_Ule3(JsBackend* self, BCValue *result, const BCValue* lhs, const BCValue* rhs)
{
    const char* e1 = GetValue(self, lhs);
    const char* e2 = GetValue(self, rhs);
    const char* targetVar = result ? GetValue(self, result) : "_Flag";

    PrintF(self, "%s = _Runtime.ule(%s, %s));\n", targetVar, e1, e2);
}

static inline void JsBackend_Lt3(JsBackend* self, BCValue *result, const BCValue* lhs, const BCValue* rhs)
{
    const char* e1 = GetValue(self, lhs);
    const char* e2 = GetValue(self, rhs);
    const char* targetVar = result ? GetValue(self, result) : "_Flag";

    if (DirectEmit(lhs->type, rhs->type))
    {
        PrintF(self, "%s = (%s < %s);\n", targetVar, e1, e2);
    }
    else
    {
        PrintF(self, "%s = _Runtime.lt(%s, %s));\n", targetVar, e1, e2);
    }
}

static inline void JsBackend_Le3(JsBackend* self, BCValue *result, const BCValue* lhs, const BCValue* rhs)
{
    const char* e1 = GetValue(self, lhs);
    const char* e2 = GetValue(self, rhs);
    const char* targetVar = result ? GetValue(self, result) : "_Flag";

    if (DirectEmit(lhs->type, rhs->type))
    {
        PrintF(self, "%s = (%s <= %s);\n", targetVar, e1, e2);
    }
    else
    {
        PrintF(self, "%s = _Runtime.le(%s, %s));\n", targetVar, e1, e2);
    }
}

static inline void JsBackend_Ugt3(JsBackend* self, BCValue *result, const BCValue* lhs, const BCValue* rhs)
{
    const char* e1 = GetValue(self, lhs);
    const char* e2 = GetValue(self, rhs);
    const char* targetVar = result ? GetValue(self, result) : "_Flag";

    PrintF(self, "%s = _Runtime.ugt(%s, %s));\n", targetVar, e1, e2);
}

static inline void JsBackend_Uge3(JsBackend* self, BCValue *result, const BCValue* lhs, const BCValue* rhs)
{
    const char* e1 = GetValue(self, lhs);
    const char* e2 = GetValue(self, rhs);
    const char* targetVar = result ? GetValue(self, result) : "_Flag";

    PrintF(self, "%s = _Runtime.uge(%s, %s));\n", targetVar, e1, e2);
}

static inline void JsBackend_Gt3(JsBackend* self, BCValue *result, const BCValue* lhs, const BCValue* rhs)
{
    const char* e1 = GetValue(self, lhs);
    const char* e2 = GetValue(self, rhs);
    const char* targetVar = result ? GetValue(self, result) : "_Flag";

    if (DirectEmit(lhs->type, rhs->type))
    {
        PrintF(self, "%s = (%s > %s);\n", targetVar, e1, e2);
    }
    else
    {
        PrintF(self, "%s = _Runtime.gt(%s, %s));\n", targetVar, e1, e2);
    }

}

static inline void JsBackend_Ge3(JsBackend* self, BCValue *result, const BCValue* lhs, const BCValue* rhs)
{
    const char* e1 = GetValue(self, lhs);
    const char* e2 = GetValue(self, rhs);
    const char* targetVar = result ? GetValue(self, result) : "_Flag";

    if (DirectEmit(lhs->type, rhs->type))
    {
        PrintF(self, "%s = (%s >= %s);\n", targetVar, e1, e2);
    }
    else
    {
        PrintF(self, "%s = _Runtime.ge(%s, %s));\n", targetVar, e1, e2);
    }

}

static inline void JsBackend_Eq3(JsBackend* self, BCValue *result, const BCValue* lhs, const BCValue* rhs)
{
    const char* e1 = GetValue(self, lhs);
    const char* e2 = GetValue(self, rhs);
    const char* targetVar = result ? GetValue(self, result) : "_Flag";

    if (DirectEmit(lhs->type, rhs->type))
    {
        PrintF(self, "%s = (%s == %s);\n", targetVar, e1, e2);
    }
    else
    {
        PrintF(self, "%s = _Runtime.ule(%s, %s));\n", targetVar, e1, e2);
    }

}

static inline void JsBackend_Neq3(JsBackend* self, BCValue *result, const BCValue* lhs, const BCValue* rhs)
{
    const char* e1 = GetValue(self, lhs);
    const char* e2 = GetValue(self, rhs);
    const char* targetVar = result ? GetValue(self, result) : "_Flag";

    if (DirectEmit(lhs->type, rhs->type))
    {
        PrintF(self, "%s = (%s != %s);\n", targetVar, e1, e2);
    }
    else
    {
        PrintF(self, "%s = _Runtime.ule(%s, %s));\n", targetVar, e1, e2);
    }
}

static inline void JsBackend_Add3(JsBackend* self, BCValue *result, const BCValue* lhs, const BCValue* rhs)
{
    const char* e1 = GetValue(self, lhs);
    const char* e2 = GetValue(self, rhs);
    const char* targetVar = result ? GetValue(self, result) : "_Flag";

    if (DirectEmit(lhs->type, rhs->type))
    {
        PrintF(self, "%s = (%s + %s);\n", targetVar, e1, e2);
    }
    else
    {
        PrintF(self, "%s = _Runtime.add(%s, %s));\n", targetVar, e1, e2);
    }
}

static inline void JsBackend_Sub3(JsBackend* self, BCValue *result, const BCValue* lhs, const BCValue* rhs)
{
    const char* e1 = GetValue(self, lhs);
    const char* e2 = GetValue(self, rhs);
    const char* targetVar = result ? GetValue(self, result) : "_Flag";

    if (DirectEmit(lhs->type, rhs->type))
    {
        PrintF(self, "%s = (%s - %s);\n", targetVar, e1, e2);
    }
    else
    {
        PrintF(self, "%s = _Runtime.sub(%s, %s));\n", targetVar, e1, e2);
    }
}

static inline void JsBackend_Mul3(JsBackend* self, BCValue *result, const BCValue* lhs, const BCValue* rhs)
{
    const char* e1 = GetValue(self, lhs);
    const char* e2 = GetValue(self, rhs);
    const char* targetVar = result ? GetValue(self, result) : "_Flag";

    if (DirectEmit(lhs->type, rhs->type))
    {
        PrintF(self, "%s = (%s * %s);\n", targetVar, e1, e2);
    }
    else
    {
        PrintF(self, "%s = _Runtime.mul(%s, %s));\n", targetVar, e1, e2);
    }
}

static inline void JsBackend_Div3(JsBackend* self, BCValue *result, const BCValue* lhs, const BCValue* rhs)
{
    const char* e1 = GetValue(self, lhs);
    const char* e2 = GetValue(self, rhs);
    const char* targetVar = result ? GetValue(self, result) : "_Flag";

    if (DirectEmit(lhs->type, rhs->type))
    {
        PrintF(self, "%s = (%s / %s);\n", targetVar, e1, e2);
    }
    else
    {
        PrintF(self, "%s = _Runtime.div(%s, %s));\n", targetVar, e1, e2);
    }
}

static inline void JsBackend_Udiv3(JsBackend* self, BCValue *result, const BCValue* lhs, const BCValue* rhs)
{
    const char* e1 = GetValue(self, lhs);
    const char* e2 = GetValue(self, rhs);
    const char* targetVar = result ? GetValue(self, result) : "_Flag";

    {
        PrintF(self, "%s = _Runtime.udiv(%s, %s));\n", targetVar, e1, e2);
    }
}

static inline void JsBackend_And3(JsBackend* self, BCValue *result, const BCValue* lhs, const BCValue* rhs)
{
    const char* e1 = GetValue(self, lhs);
    const char* e2 = GetValue(self, rhs);
    const char* targetVar = result ? GetValue(self, result) : "_Flag";

    if (DirectEmit(lhs->type, rhs->type))
    {
        PrintF(self, "%s = (%s & %s);\n", targetVar, e1, e2);
    }
}

static inline void JsBackend_Or3(JsBackend* self, BCValue *result, const BCValue* lhs, const BCValue* rhs)
{
    const char* e1 = GetValue(self, lhs);
    const char* e2 = GetValue(self, rhs);
    const char* targetVar = result ? GetValue(self, result) : "_Flag";

    if (AreSmallIntegers(lhs->type, rhs->type))
    {
        PrintF(self, "%s = (%s | %s);\n", targetVar, e1, e2);
    }
    else
    {
        PrintF(self, "%s = _Runtime.or(%s, %s));\n", targetVar, e1, e2);
    }

}

static inline void JsBackend_Xor3(JsBackend* self, BCValue *result, const BCValue* lhs, const BCValue* rhs)
{
    const char* e1 = GetValue(self, lhs);
    const char* e2 = GetValue(self, rhs);
    const char* targetVar = result ? GetValue(self, result) : "_Flag";

    if (AreSmallIntegers(lhs->type, rhs->type))
    {
        PrintF(self, "%s = (%s ^ %s);\n", targetVar, e1, e2);
    }
    else
    {
        PrintF(self, "%s = _Runtime.xor(%s, %s));\n", targetVar, e1, e2);
    }

}

static inline void JsBackend_Lsh3(JsBackend* self, BCValue *result, const BCValue* lhs, const BCValue* rhs)
{
    const char* e1 = GetValue(self, lhs);
    const char* e2 = GetValue(self, rhs);
    const char* targetVar = result ? GetValue(self, result) : "_Flag";

    if (AreSmallIntegers(lhs->type, rhs->type))
    {
        PrintF(self, "%s = (%s << %s);\n", targetVar, e1, e2);
    }
    else
    {
        PrintF(self, "%s = _Runtime.lsh(%s, %s));\n", targetVar, e1, e2);
    }

}

static inline void JsBackend_Rsh3(JsBackend* self, BCValue *result, const BCValue* lhs, const BCValue* rhs)
{
    const char* e1 = GetValue(self, lhs);
    const char* e2 = GetValue(self, rhs);
    const char* targetVar = result ? GetValue(self, result) : "_Flag";

    if (AreSmallIntegers(lhs->type, rhs->type))
    {
        PrintF(self, "%s = (%s >> %s);\n", targetVar, e1, e2);
    }
    else
    {
        PrintF(self, "%s = _Runtime.rsh(%s, %s));\n", targetVar, e1, e2);
    }
}

static inline void JsBackend_Mod3(JsBackend* self, BCValue *result, const BCValue* lhs, const BCValue* rhs)
{
    const char* e1 = GetValue(self, lhs);
    const char* e2 = GetValue(self, rhs);
    const char* targetVar = result ? GetValue(self, result) : "_Flag";

    if (DirectEmit(lhs->type, rhs->type))
    {
        PrintF(self, "%s = (%s % %s);\n", targetVar, e1, e2);
    }
    else
    {
        PrintF(self, "%s = _Runtime.mod(%s, %s));\n", targetVar, e1, e2);
    }
}

static inline void JsBackend_Umod3(JsBackend* self, BCValue *result, const BCValue* lhs, const BCValue* rhs)
{
    const char* e1 = GetValue(self, lhs);
    const char* e2 = GetValue(self, rhs);
    const char* targetVar = result ? GetValue(self, result) : "_Flag";

    PrintF(self, "%s = _Runtime.umod(%s, %s));\n", targetVar, e1, e2);
}

static inline void JsBackend_Not(JsBackend* self, BCValue *result, const BCValue* val)
{
    const char* e1 = GetValue(self, val);
    const char* targetVar = result ? GetValue(self, result) : "_Flag";

    if (SMALLINT(val->type))
    {
        PrintF(self, "%s = ~(%s);\n", targetVar, e1);
    }
    else
    {
        PrintF(self, "%s = _Runtime.not(%s);\n", targetVar, e1);
    }
}

static inline void JsBackend_LoadFramePointer(JsBackend* self, BCValue *result, const int32_t offset)
{
}

static inline void JsBackend_Call(JsBackend* self, BCValue *result, const BCValue* fn, const BCValue* args, uint32_t n_args)
{
    const char* func = 0;

    if (fn->vType == BCValueType_Immediate)
    {
        func = GetFunctionName(self, fn);
    }

    PrintF(self, "(");
    if (n_args)
    {
        uint32_t i;
        for(i = 0; i < n_args - 1; i++)
        {
            PrintF(self, "%s, ", GetValue(self, &args[i]));
        }
        PrintF(self, "%s", GetValue(self, &args[n_args - 1]));
    }
    PrintF(self, ");\n");
}

static inline BCLabel JsBackend_GenLabel(JsBackend* self)
{
    BCLabel result =  {{self->LabelCount++}};
    PrintF(self, "L%04d: ", result.addr.addr);
}

static inline void JsBackend_Jmp(JsBackend* self, BCLabel target)
{
}

static inline BCAddr JsBackend_BeginJmp(JsBackend* self)
{
}

static inline void JsBackend_EndJmp(JsBackend* self, BCAddr atIp, BCLabel target)
{
}

static inline CndJmpBegin JsBackend_BeginCndJmp(JsBackend* self, const BCValue* cond, bool ifTrue)
{
}

static inline void JsBackend_EndCndJmp(JsBackend* self, const CndJmpBegin *jmp, BCLabel target)
{
}

static inline void JsBackend_Load8(JsBackend* self, BCValue *dest, const BCValue* from)
{
    const char* destV = GetValue(self, dest);
    const char* fromV = GetValue(self, from);

    PrintF(self, "%s = _Runtime.HEAP8[%s];\n", destV, fromV);
}

static inline void JsBackend_Store8(JsBackend* self, BCValue *dest, const BCValue* value)
{
    const char* destV = GetValue(self, dest);
    const char* valueV = GetValue(self, valueV);

    PrintF(self, "_Runtime.HEAP8[%s] = %s;\n", destV, valueV);
}

static inline void JsBackend_Load16(JsBackend* self, BCValue *dest, const BCValue* from)
{
    const char* destV = GetValue(self, dest);
    const char* fromV = GetValue(self, from);

    PrintF(self, "%s = _Runtime.HEAP16[%s];\n", destV, fromV);
}

static inline void JsBackend_Store16(JsBackend* self, BCValue *dest, const BCValue* value)
{
    const char* destV = GetValue(self, dest);
    const char* valueV = GetValue(self, valueV);

    PrintF(self, "_Runtime.HEAP16[%s] = %s;\n" , destV, valueV);
}

static inline void JsBackend_Load32(JsBackend* self, BCValue *dest, const BCValue* from)
{
    const char* destV = GetValue(self, dest);
    const char* fromV = GetValue(self, from);

    PrintF(self, "%s = _Runtime.HEAP32[%s];\n", destV, fromV);
}

static inline void JsBackend_Store32(JsBackend* self, BCValue *dest, const BCValue* value)
{
    const char* destV = GetValue(self, dest);
    const char* valueV = GetValue(self, valueV);

    PrintF(self, "_Runtime.HEAP32[%s] = %s;\n", destV, valueV);
}

static inline void JsBackend_Load64(JsBackend* self, BCValue *dest, const BCValue* from)
{
}

static inline void JsBackend_Store64(JsBackend* self, BCValue *dest, const BCValue* value)
{
}

static inline void JsBackend_Throw(JsBackend* self, const BCValue* e)
{
}

static inline void JsBackend_PushCatch(JsBackend* self)
{
}

static inline void JsBackend_PopCatch(JsBackend* self)
{
}

static inline void JsBackend_Ret(JsBackend* self, const BCValue* val)
{
    const char* v = GetValue(self, val);
    if (val->vType != BCValueType_Unknown)
    {
        PrintF(self, "return %s;\n", v);
    }
    else
    {
        PrintF(self, "return ;\n");
    }
}

static inline void JsBackend_IToF32(JsBackend* self, BCValue *result, const BCValue* rhs)
{
    // noop
}

static inline void JsBackend_IToF64(JsBackend* self, BCValue *result, const BCValue* rhs)
{
    assert(0);
}

static inline void JsBackend_F32ToI(JsBackend* self, BCValue *result, const BCValue* rhs)
{
    // noop
}

static inline void JsBackend_F64ToI(JsBackend* self, BCValue *result, const BCValue* rhs)
{
    assert(0);
}

static inline void JsBackend_F32ToF64(JsBackend* self, BCValue *result, const BCValue* rhs)
{
    // noop
}

static inline void JsBackend_F64ToF32(JsBackend* self, BCValue *result, const BCValue* rhs)
{
    // noop
}

static inline void JsBackend_Memcmp(JsBackend* self, BCValue *result, const BCValue* lhs, const BCValue* rhs)
{
}

static inline void JsBackend_Realloc(JsBackend* self, BCValue *result, const BCValue* lhs, const BCValue* rhs, const uint32_t size)
{
}

static inline BCValue JsBackend_Run(JsBackend* self, uint32_t fnIdx, const BCValue* args, uint32_t n_args, BCHeap* heap)
{
}

static inline uint32_t JsBackend_sizeof_instance(JsBackend* self)
{
    return sizeof(JsBackend);
}

static inline void JsBackend_clear_instance(JsBackend* instance)
{
    free(instance->Code);
}

static inline void JsBackend_init_instance(JsBackend* instance)
{
    instance->CodeCapacity = 4096;
    instance->Code = malloc(instance->CodeCapacity);
}

static inline void JsBackend_fini_instance(JsBackend* selftance)
{
}

static inline void JsBackend_ReadI32(JsBackend* self, const BCValue* val, const ReadI32_cb_t readCb, void* userCtx)
{
}

static inline void JsBackend_ReadI32_cb(JsBackend* selfvalue, void* userCtx)
{
}

static inline void JsBackend_set_alloc_memory(JsBackend* self, alloc_fn_t alloc_fn, void* userCtx)
{
    self->allocFn = alloc_fn;
    self->allocCtx = userCtx;
}

static inline void JsBackend_set_get_typeinfo(JsBackend* self, get_typeinfo_fn_t get_typeinfo_fn, void* userCtx)
{
    self->getTypeInfoFn = get_typeinfo_fn;
    self->getTypeInfoCtx = userCtx;
}

const BackendInterface JsBackend_interface = {
    /*.name = */ "JsBackend",

    /*.Initialize = */(Initialize_t) JsBackend_Initialize,
    /*.InitializeV = */(InitializeV_t) JsBackend_InitializeV,
    /*.Finalize = */(Finalize_t) JsBackend_Finalize,

    /*.BeginFunction = */(BeginFunction_t) JsBackend_BeginFunction,
    /*.EndFunction = */(EndFunction_t) JsBackend_EndFunction,

    /*.GenTemporary = */(GenTemporary_t) JsBackend_GenTemporary,
    /*.DestroyTemporary = */(DestroyTemporary_t) JsBackend_DestroyTemporary,
    /*.GenLocal = */(GenLocal_t) JsBackend_GenLocal,
    /*.DestroyLocal = */(DestroyLocal_t) JsBackend_DestroyLocal,
    /*.GenParameter = */(GenParameter_t) JsBackend_GenParameter,
    /*.GenExternal = */(GenExternal_t) JsBackend_GenExternal,
    /*.MapExternal = */(MapExternal_t) JsBackend_MapExternal,
    /*.GenExternalFunc = */(GenExternalFunc_t) JsBackend_GenExternalFunc,
    /*.MapExternalFunc = */(MapExternalFunc_t) JsBackend_MapExternalFunc,
    /*.EmitFlag = */(EmitFlag_t) JsBackend_EmitFlag,
    /*.Alloc = */(Alloc_t) JsBackend_Alloc,
    /*.Assert = */(Assert_t) JsBackend_Assert,
    /*.MemCpy = */(MemCpy_t) JsBackend_MemCpy,
    /*.File = */(File_t) JsBackend_File,
    /*.Line = */(Line_t) JsBackend_Line,
    /*.Comment = */(Comment_t) JsBackend_Comment,
    /*.Prt = */(Prt_t) JsBackend_Prt,
    /*.Set = */(Set_t) JsBackend_Set,
    /*.Ult3 = */(Ult3_t) JsBackend_Ult3,
    /*.Ule3 = */(Ule3_t) JsBackend_Ule3,
    /*.Lt3 = */(Lt3_t) JsBackend_Lt3,
    /*.Le3 = */(Le3_t) JsBackend_Le3,
    /*.Ugt3 = */(Ugt3_t) JsBackend_Ugt3,
    /*.Uge3 = */(Uge3_t) JsBackend_Uge3,
    /*.Gt3 = */(Gt3_t) JsBackend_Gt3,
    /*.Ge3 = */(Ge3_t) JsBackend_Ge3,
    /*.Eq3 = */(Eq3_t) JsBackend_Eq3,
    /*.Neq3 = */(Neq3_t) JsBackend_Neq3,
    /*.Add3 = */(Add3_t) JsBackend_Add3,
    /*.Sub3 = */(Sub3_t) JsBackend_Sub3,
    /*.Mul3 = */(Mul3_t) JsBackend_Mul3,
    /*.Div3 = */(Div3_t) JsBackend_Div3,
    /*.Udiv3 = */(Udiv3_t) JsBackend_Udiv3,
    /*.And3 = */(And3_t) JsBackend_And3,
    /*.Or3 = */(Or3_t) JsBackend_Or3,
    /*.Xor3 = */(Xor3_t) JsBackend_Xor3,
    /*.Lsh3 = */(Lsh3_t) JsBackend_Lsh3,
    /*.Rsh3 = */(Rsh3_t) JsBackend_Rsh3,
    /*.Mod3 = */(Mod3_t) JsBackend_Mod3,
    /*.Umod3 = */(Umod3_t) JsBackend_Umod3,
    /*.Not = */(Not_t) JsBackend_Not,
    /*.LoadFramePointer = */(LoadFramePointer_t) JsBackend_LoadFramePointer,
    /*.Call = */(Call_t) JsBackend_Call,
    /*.GenLabel = */(GenLabel_t) JsBackend_GenLabel,
    /*.Jmp = */(Jmp_t) JsBackend_Jmp,
    /*.BeginJmp = */(BeginJmp_t) JsBackend_BeginJmp,
    /*.EndJmp = */(EndJmp_t) JsBackend_EndJmp,
    /*.BeginCndJmp = */(BeginCndJmp_t) JsBackend_BeginCndJmp,
    /*.EndCndJmp = */(EndCndJmp_t) JsBackend_EndCndJmp,
    /*.Load8 = */(Load8_t) JsBackend_Load8,
    /*.Store8 = */(Store8_t) JsBackend_Store8,
    /*.Load16 = */(Load16_t) JsBackend_Load16,
    /*.Store16 = */(Store16_t) JsBackend_Store16,
    /*.Load32 = */(Load32_t) JsBackend_Load32,
    /*.Store32 = */(Store32_t) JsBackend_Store32,
    /*.Load64 = */(Load64_t) JsBackend_Load64,
    /*.Store64 = */(Store64_t) JsBackend_Store64,
    /*.Throw = */(Throw_t) JsBackend_Throw,
    /*.PushCatch = */(PushCatch_t) JsBackend_PushCatch,
    /*.PopCatch = */(PopCatch_t) JsBackend_PopCatch,
    /*.Ret = */(Ret_t) JsBackend_Ret,
    /*.IToF32 = */(IToF32_t) JsBackend_IToF32,
    /*.IToF64 = */(IToF64_t) JsBackend_IToF64,
    /*.F32ToI = */(F32ToI_t) JsBackend_F32ToI,
    /*.F64ToI = */(F64ToI_t) JsBackend_F64ToI,
    /*.F32ToF64 = */(F32ToF64_t) JsBackend_F32ToF64,
    /*.F64ToF32 = */(F64ToF32_t) JsBackend_F64ToF32,
    /*.Memcmp = */(Memcmp_t) JsBackend_Memcmp,
    /*.Realloc = */(Realloc_t) JsBackend_Realloc,
    /*.Run = */(run_t) JsBackend_Run,
    /*.ReadI32 = */(ReadI32_t) JsBackend_ReadI32,

    /*.sizeof_instance = */(sizeof_instance_t) JsBackend_sizeof_instance,
    /*.clear_instance = */(clear_instance_t) JsBackend_clear_instance,
    /*.init_instance = */(init_instance_t) JsBackend_init_instance,
    /*.fini_instance = */(fini_instance_t) JsBackend_fini_instance,

    /*.set_alloc_memory = */(set_alloc_memory_t) JsBackend_set_alloc_memory,
    /*.set_get_typeinfo = */(set_get_typeinfo_t) JsBackend_set_get_typeinfo,
};
