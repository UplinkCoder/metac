#include "backend_interface_funcs.h"
#include "bc_common.h"
#include "fpconv/fpconv.c"
#include <assert.h>

#define cast(T) (T)

typedef struct ErrorInfo
{
    const char* msg;
    BCValue values[4];
    uint32_t valueCount;
} ErrorInfo;

typedef struct Printer
{
    char* Buffer;
    const char* BufferStart;
    uint32_t BufferCapacity;

    uint32_t vIp;
    uint32_t CurrentIndent;
    _Bool LineIndented;

    uint32_t NumberOfLocals;
    uint32_t NumberOfTemporaries;

    ErrorInfo* ErrorInfos;
    uint32_t ErrorInfoCount;
    uint32_t ErrorInfoCapacity;

    uint32_t NumberOfFunctions;
    uint32_t NumberOfParameters;
    uint32_t LastLabel;
    char* functionName;
} Printer;

void static inline Printer_EnsureCapacity(Printer* self, uint32_t capacity)
{
    if (self->BufferCapacity < capacity)
    {
        assert(0); // we don't support growing yet :->
    }
}

// #include "int_to_str.c"

static inline void Printer_PutNewline(Printer* self)
{
    *self->Buffer++ = '\n';
    self->LineIndented = false;
}

static inline void Printer_PutIndent(Printer* self)
{
    assert(!self->LineIndented);
    assert((self->CurrentIndent * 4) <= 128);

    Printer_EnsureCapacity(self, 128);
    for(int i = 0; i < self->CurrentIndent; i++)
    {
        *self->Buffer++ = ' ';
        *self->Buffer++ = ' ';
        *self->Buffer++ = ' ';
        *self->Buffer++ = ' ';
    }
    self->LineIndented = true;
}

static inline void Printer_PutNewlineIndent(Printer* self)
{
    Printer_PutNewline(self);
    Printer_PutIndent(self);
}

static inline void Printer_PutStr(Printer* self, const char* str)
{
    uint32_t length = 0;
    Printer_EnsureCapacity(self, 128);

    if (!self->LineIndented)
    {
        Printer_PutIndent(self);
        Printer_EnsureCapacity(self, 128);
    }
    char c;
    if (str)
    {
        while((c = *str++))
        {
            assert (c != '\n');
            length++;
            *self->Buffer++ = c;

            if (length & 128)
            {
                self->BufferCapacity -= 128;
                length -= 128;
                Printer_EnsureCapacity(self, 128);
            }
        }
    }

    self->BufferCapacity -= length;
}

static inline void Printer_PutChar(Printer *self, char c)
{
    if ((!self->LineIndented) && (c != '\n'))
    {
        Printer_PutIndent(self);
    }

    assert(self->BufferCapacity >= 1);

    *self->Buffer++ = c;
    self->BufferCapacity--;
}

static inline void Printer_PutQuotedStr(Printer* self, const char* str)
{
    if (str)
    {
        Printer_PutChar(self, '"');
        if (*str) Printer_PutStr(self, str);
        Printer_PutChar(self, '"');
    }
    else
    {
        Printer_PutChar(self, '0');
    }
}

static inline void Printer_PutHex(Printer* self, const uint64_t v)
{
    char buffer[17];

    char* begin_number = u64tohexstr(v, buffer);

    Printer_PutStr(self, begin_number);
}

static inline void Printer_PutU64(Printer* self, const uint64_t v)
{
    char buffer[21];

    char* begin_number = u64tostr(v, buffer);

    Printer_PutStr(self, begin_number);
}


static inline void Printer_PutI64(Printer* self, const int64_t v)
{
    char buffer[21];

    char* begin_number = i64tostr(v, buffer);

    Printer_PutStr(self, begin_number);
}

#define Printer_PutU32 Printer_PutU64
#define Printer_PutI32 Printer_PutI64

static inline void Printer_IncreaseIndent(Printer* self)
{
    self->CurrentIndent++;
    self->LineIndented = false;
}

static inline void Printer_DecreaseIndent(Printer* self)
{
    self->CurrentIndent--;
    self->LineIndented = false;
}

static inline void Printer_PutDouble(Printer* self, double d)
{
    Printer_EnsureCapacity(self, 24);

    uint32_t sz = fpconv_dtoa(d, self->Buffer);
    self->Buffer += sz;
    self->BufferCapacity -= sz;
}

static inline void Printer_PutFloat(Printer* self, float f)
{
    Printer_PutDouble(self, f);
    Printer_PutChar(self, 'f');
}

static inline void Printer_PrintType(Printer* self, const BCType* type)
{
    Printer_PutStr(self, "(BCType){");
    Printer_PutStr(self, BCTypeEnum_toChars(&type->type));

    Printer_PutStr(self, ", ");
    Printer_PutU32(self, type->typeIndex);
    Printer_PutStr(self, "}");
}

static inline void Printer_PrintLocal(Printer* self, const BCValue* local)
{
    assert(local->vType == BCValueType_Local);
    if (local->name)
    {
        Printer_PutStr(self, local->name);
    }
    else
    {
        Printer_PutStr(self, "local");
        Printer_PutU32(self, local->localIndex);
    }
}

static inline void Printer_PrintParameter(Printer* self, const BCValue* param)
{
    assert(param->vType == BCValueType_Parameter);
    if (param->name)
    {
        Printer_PutStr(self, param->name);
    }
    else
    {
        Printer_PutStr(self, "p");
        Printer_PutU32(self, param->parameterIndex);
    }
}

static inline void Printer_PrintTemporary(Printer* self, const BCValue* tmp)
{
    assert(tmp->vType == BCValueType_Temporary);

    Printer_PutStr(self, "tmp");
    Printer_PutU32(self, tmp->temporaryIndex);
}

static inline void Printer_PrintBCValue(Printer* self, const BCValue* val)
{
    BCValueType vType = val->vType;
    const BCType type = val->type;

    uint32_t val_imm32 = (vType  == BCValueType_Immediate ? val->imm32.imm32 : 0);

    switch (vType)
    {
    case BCValueType_Immediate:
        {
            switch(type.type)
            {
            case BCTypeEnum_c8:
                {
                    Printer_PutStr(self, "(Imm32){'");
                    Printer_PutChar(self, val_imm32 & 0xFF);
                    Printer_PutStr(self, "'}");
                } break;
            case BCTypeEnum_i8:
                {
                    Printer_PutStr(self, "(Imm32){");
                    Printer_PutU32(self, val_imm32);
                    Printer_PutStr(self, "} // i8");
                    Printer_PutNewline(self);
                } break;
            case BCTypeEnum_u32:
                {
                    Printer_PutStr(self, "(Imm32){");
                    Printer_PutU32(self, val_imm32);
                    Printer_PutStr(self, ", false}");
                } break;
            case BCTypeEnum_i32:
                {
                    Printer_PutStr(self, "(Imm32){");
                    Printer_PutI32(self, val_imm32);
                    Printer_PutStr(self, ", true}");
                } break;
            case BCTypeEnum_i64:
                {
                    Printer_PutStr(self, "(Imm64){");
                    Printer_PutI64(self, val->imm64.imm64);
                    Printer_PutStr(self, "LL, true}");
                } break;
            case BCTypeEnum_u64:
                {
                    Printer_PutStr(self, "(Imm64){");
                    Printer_PutU64(self, val->imm64.imm64);
                    Printer_PutStr(self, "ULL , false}");
                } break;
            case BCTypeEnum_f23:
                {
                    Printer_PutStr(self, "(Imm23f){");
                    Printer_PutFloat(self, *cast(float*)&val_imm32);
                    Printer_PutStr(self, "}");
                } break;
            case BCTypeEnum_f52:
                {
                    Printer_PutStr(self, "(Imm52f){");
                    Printer_PutDouble(self, *cast(double*)&val->imm64.imm64);
                    Printer_PutStr(self, "}");
                } break;
            case BCTypeEnum_Null:
                {
                    Printer_PutStr(self, "(Imm32){0/*null*/)");
                } break;
            case BCTypeEnum_Array:
                {
                    Printer_PutStr(self, "(Imm32){");
                    Printer_PutU32(self, val_imm32);
                    Printer_PutStr(self, " /*Array*/)");
                } break;
            default :
                {
#define CatStr(VAR, STR) \
    for(char c, *p = STR; (c = *p); p++) \
        *(VAR)++ = c;

#define LogError(STR)

                    //assert(0, "Unexpected Immediate of Type");
                    char errorBuffer[256];
                    char* msg = errorBuffer;

                    CatStr(msg, (char*)"Unexpected Immediate of Type");
                    CatStr(msg, cast(char*)BCTypeEnum_toChars(&type.type));
                    *msg = '\0';
                    LogError(errorBuffer);
                }
            }
        } break;

    case BCValueType_StackValue:
        {
            if (val->temporaryIndex)
            {
                Printer_PutStr(self,"tmp");
                Printer_PutU32(self, val->temporaryIndex);
            }
            else
            {
                Printer_PutStr(self, "StackAddr(");
                Printer_PutU32(self, val->stackAddr.addr);
                Printer_PutStr(self, "), ");
                Printer_PrintType(self, &type);
            }
        } break;

    case BCValueType_Local :
        {
            Printer_PrintLocal(self, val);
        } break;
    case BCValueType_Temporary:
        {
            Printer_PrintTemporary(self, val);
        } break;
    case BCValueType_Parameter:
        {
            Printer_PrintParameter(self, val);
        } break;
    case BCValueType_Error:
    case BCValueType_ErrorWithMessage:
        {
            Printer_PutStr(self, "(Imm32){");
            Printer_PutU32(self, val_imm32);
            Printer_PutStr(self, ")");

            if (val_imm32)
            {
                int closeMultilineCommment = 0;
                assert(self->ErrorInfoCount <= val_imm32);
                ErrorInfo* eInfo = self->ErrorInfos + (val_imm32 - 1);


                if (eInfo->msg && eInfo->msg[0] != '\0')
                {
                    closeMultilineCommment = 1;
                    Printer_PutStr(self, "/* ");
                    Printer_PutQuotedStr(self, eInfo->msg);
                    Printer_PutChar(self, ' ');
                }
                else
                {
                    Printer_PutStr(self, "// ");
                }


                for(int i = 0; i < eInfo->valueCount; i++)
                {
                    Printer_PrintBCValue(self, eInfo->values + i);
                    if (i != (eInfo->valueCount - 1))
                        Printer_PutStr(self, ", ");
                }
                if (closeMultilineCommment)
                    Printer_PutStr(self, "*/");
            }
        } break;
    case BCValueType_Unknown:
        {
            Printer_PutStr(self, "BCValue.init");
        } break;
    default:
        {
            char errorBuffer[256];
            char* msg = errorBuffer;

            CatStr(msg, cast(char*)"Printing for ");
            CatStr(msg, cast(char*)BCValueType_toChars(&val->vType));
            CatStr(msg, cast(char*)" unimplemented ");
            *msg = '\0';

            LogError(errorBuffer);
        }
    }
}

extern void Printer_StreamToFile(Printer* self, FILE* fd)
{
    uint32_t sz = self->Buffer - self->BufferStart;
    uint32_t bytes_written = fwrite(self->BufferStart, 1, sz, fd);
    assert(sz == bytes_written);

    self->Buffer = cast(char*)self->BufferStart;
    self->BufferCapacity += sz;
}

static inline void Printer_Op3(Printer* self,
    const BCValue *result, const BCValue *lhs, const BCValue *rhs
  , const char* inst)
{
    self->vIp += 2;
    Printer_PutStr(self, inst);
    Printer_PutChar(self, '(');

    if (result)
    {
        Printer_PrintBCValue(self, result);
    }
    else
    {
        Printer_PutChar(self, '0');
    }
    Printer_PutStr(self, ", ");

    Printer_PrintBCValue(self, lhs);
    Printer_PutStr(self, ", ");

    Printer_PrintBCValue(self, rhs);
    Printer_PutChar(self, ')');
    Printer_PutChar(self, ';');

    Printer_PutNewline(self);
}

static inline void Printer_Op2(Printer* self,
    const BCValue *lhs, const BCValue *rhs
  , const char* inst)
{
    self->vIp += 2;

    Printer_PutStr(self, inst);
    Printer_PutChar(self, '(');

    Printer_PrintBCValue(self, lhs);
    Printer_PutStr(self, ", ");

    Printer_PrintBCValue(self, rhs);
    Printer_PutChar(self, ')');
    Printer_PutChar(self, ';');

    Printer_PutNewline(self);
}

static inline void Printer_Op1(Printer* self,
    const BCValue *lhs
  , const char* inst)
{
    self->vIp += 2;

    Printer_PutStr(self, inst);

    Printer_PutChar(self, '(');
    Printer_PrintBCValue(self, lhs);
    Printer_PutChar(self, ')');
    Printer_PutChar(self, ';');

    Printer_PutNewline(self);
}

#define PR_OP3(OP) \
    static inline void Printer_##OP(Printer* self, BCValue *result, const BCValue* lhs, const BCValue* rhs) \
    { Printer_Op3(self, result, lhs, rhs, #OP); }


PR_OP3(Gt3)
PR_OP3(Ugt3)
PR_OP3(Ge3)
PR_OP3(Uge3)
PR_OP3(Lt3)
PR_OP3(Ult3)
PR_OP3(Le3)
PR_OP3(Ule3)
PR_OP3(Eq3)
PR_OP3(Neq3)

PR_OP3(Add3)
PR_OP3(Sub3)
PR_OP3(Mul3)
PR_OP3(Div3)
PR_OP3(Udiv3)
PR_OP3(Mod3)
PR_OP3(Umod3)
PR_OP3(Or3)
PR_OP3(Xor3)
PR_OP3(Rsh3)
PR_OP3(Lsh3)
PR_OP3(And3)

#define PR_OP2(OP) \
    static inline void Printer_##OP(Printer* self, \
        const BCValue* lhs, const BCValue* rhs) \
    { Printer_Op2(self, lhs, rhs, #OP); }

#define PR_OP1(OP) \
    static inline void Printer_##OP(Printer* self, \
        const BCValue* lhs) \
    { Printer_Op1(self, lhs, #OP); }

PR_OP2(Store8)
PR_OP2(Store16)
PR_OP2(Store32)
PR_OP2(Store64)

PR_OP2(Load8)
PR_OP2(Load16)
PR_OP2(Load32)
PR_OP2(Load64)

static inline void Printer_Initialize(Printer* self, uint32_t n_args, ...)
{
    Printer_PutStr(self, "Initialize(");
    Printer_PutU32(self, n_args);
    Printer_PutStr(self, ", ...);");
    Printer_PutNewline(self);
}

static inline void Printer_InitializeV(Printer* self, uint32_t n_args, va_list args)
{
}

static inline void Printer_Finalize(Printer* self)
{
    Printer_PutStr(self, "Finalize()");
    Printer_PutNewline(self);
}

static inline uint32_t Printer_beginFunction(Printer* self, uint32_t fnIdx, const char* name)
{
    if (!fnIdx)
        fnIdx = ++self->NumberOfFunctions;

    self->functionName = (char*)name;
    Printer_PutStr(self, "uint32_t ");
    if (name)
    {
        Printer_PutStr(self, name);
    }
    else
    {
        Printer_PutStr(self, "f");
        Printer_PutU32(self, fnIdx);
    }
    Printer_PutStr(self, "_idx = ");
    Printer_PutStr(self, "beginFunction(");
    Printer_PutU32(self, fnIdx);

    Printer_PutStr(self, ", ");
    if (name)
        Printer_PutQuotedStr(self, name);
    else
        Printer_PutChar(self, '0');
    Printer_PutChar(self, ')');

    Printer_PutNewline(self);
    Printer_PutChar(self, '{');

    Printer_IncreaseIndent(self);
    Printer_PutNewline(self);

    return fnIdx;
}

static inline void Printer_endFunction(Printer* self, uint32_t fnIdx)
{
    Printer_DecreaseIndent(self);
    Printer_PutChar(self, '}');
    Printer_PutNewline(self);

    self->NumberOfLocals = 0;
    self->NumberOfTemporaries = 0;
    self->NumberOfParameters = 0;

    Printer_PutStr(self, "endFunction(");
    if (self->functionName)
    {
        Printer_PutStr(self, self->functionName);
        Printer_PutStr(self, "_idx");
    }
    else
    {
        Printer_PutStr(self, "f");
        Printer_PutU32(self, fnIdx);
    }

    Printer_PutStr(self, ");");

    Printer_PutNewline(self);
}

static inline BCValue Printer_genTemporary(Printer* self, BCType bct)
{
    BCValue result = {
        .vType = BCValueType_Temporary,
        .type = bct,
    };
    result.temporaryIndex = ++self->NumberOfTemporaries;

    Printer_PutStr(self, "BCValue ");

    Printer_PrintTemporary(self, &result);

    Printer_PutStr(self, " = genTemporary(");
    Printer_PrintType(self, &bct);
    Printer_PutStr(self, ");");
    Printer_PutNewline(self);

    return result;
}

PR_OP1(destroyTemporary)

static inline BCValue Printer_genLocal(Printer* self, BCType bct, const char* name)
{
    BCValue result = {
        .vType = BCValueType_Local,
        .type = bct,
        .name = name
    };
    result.localIndex = ++self->NumberOfLocals;

    Printer_PutStr(self, "BCValue ");
    Printer_PrintLocal(self, &result);

    Printer_PutStr(self, " = ");
    Printer_PutStr(self, "genLocal(");

    Printer_PrintType(self, &bct);
    Printer_PutStr(self, ", ");

    Printer_PutQuotedStr(self, name);

    Printer_PutStr(self, ");");
    Printer_PutNewline(self);

    return result;
}

static inline BCValue Printer_genParameter(Printer* self, BCType bct, const char* name)
{
    BCValue result = {
        .vType = BCValueType_Parameter,
        .type = bct,
        .name = name
    };

    result.parameterIndex = ++self->NumberOfParameters;
    Printer_PutStr(self, "BCValue ");

    Printer_PrintParameter(self, &result);

    Printer_PutStr(self, " = genParameter(");

    Printer_PrintType(self, &bct);
    Printer_PutStr(self, ", ");

    if (name)
        Printer_PutQuotedStr(self, name);
    else
        Printer_PutChar(self, '0');

    Printer_PutStr(self, ");");
    Printer_PutNewline(self);

    return result;
}

PR_OP1(emitFlag)

PR_OP2(Alloc)
PR_OP2(Assert)
PR_OP3(MemCpy)

static inline void Printer_File(Printer* self, const char* filename)
{
    Printer_PutStr(self, "File(");
    Printer_PutQuotedStr(self, filename);
    Printer_PutStr(self, ");");
    Printer_PutNewline(self);
}

static inline void Printer_Line(Printer* self, uint32_t line)
{
    Printer_PutStr(self, "Line(");
    Printer_PutU32(self, line);
    Printer_PutStr(self, ");");
    Printer_PutNewline(self);
}

static inline void Printer_Comment(Printer* self, const char* comment)
{
    Printer_PutStr(self, "Comment(");
    Printer_PutQuotedStr(self, comment);
    Printer_PutStr(self, ");");
    Printer_PutNewline(self);
}

static inline void Printer_PutBool(Printer* self, bool v)
{
    Printer_PutStr(self, (v ? "true" : "false"));
}

static inline void Printer_Prt(Printer* self, const BCValue* value, bool isString)
{
    Printer_PutStr(self, "Prt(");
    Printer_PrintBCValue(self, value);
    Printer_PutStr(self, ", ");
    Printer_PutBool(self, isString);
    Printer_PutStr(self, ");");
    Printer_PutNewline(self);
}

PR_OP2(Set)
PR_OP2(Not)

static inline void Printer_LoadFramePointer(Printer* self, BCValue *result, const int32_t offset)
{
    Printer_PutStr(self, "LoadFramePointer(");
    Printer_PrintBCValue(self, result);
    Printer_PutStr(self, ", ");
    Printer_PutI32(self, offset);
    Printer_PutStr(self, ");");

    Printer_PutNewline(self);
}

static inline void Printer_Call(Printer* self, BCValue *result, const BCValue* fn, const BCValue* args, uint32_t n_args)
{
    Printer_PutStr(self, "Call(");
    Printer_PrintBCValue(self, result);
    Printer_PutStr(self, ", ");

    Printer_PrintBCValue(self, fn);
    Printer_PutStr(self, ", ");

    Printer_PutChar(self, '{');
    {
        if (!n_args) Printer_PutChar(self, '0');
        else for(int i = 0; i < n_args; i++)
        {
            const BCValue* arg = args + i;
            Printer_PrintBCValue(self, arg);
            if (i != n_args - 1)
                Printer_PutStr(self, ", ");
        }
    }
    Printer_PutChar(self, '}');
    Printer_PutStr(self, ", ");

    Printer_PutU32(self, n_args);
    Printer_PutStr(self, ");");
    Printer_PutNewline(self);
}

static inline void Printer_PrintLabel(Printer* self, const BCLabel* label)
{
    Printer_PutStr(self, "label");
    Printer_PutU32(self, label->addr.addr);
}

static inline BCLabel Printer_genLabel(Printer* self)
{
    BCLabel result = {{self->vIp}};
    if (self->LastLabel != self->vIp)
    {

        Printer_PrintLabel(self, &result);
        Printer_PutStr(self, " = genLabel();");
        Printer_PutNewline(self);
        self->LastLabel = self->vIp;
    }
    return result;
}

static inline BCAddr Printer_beginJmp(Printer* self)
{
    BCAddr result = {self->vIp};

    Printer_PutStr(self, "BCAddr jmp");
    Printer_PutU32(self, result.addr);
    self->vIp += 2;

    Printer_PutStr(self, " = beginJmp();");
    Printer_PutNewline(self);

    return result;
}

static inline void Printer_Jmp(Printer* self, BCLabel target)
{
    Printer_PutStr(self, "Jmp(");
    Printer_PrintLabel(self, &target);

    Printer_PutStr(self, ");");
    Printer_PutNewline(self);
}

static inline void Printer_endJmp(Printer* self, BCAddr atIp, BCLabel target)
{
    Printer_PutStr(self, "endJmp(jmp");
    Printer_PutU32(self, atIp.addr);

    Printer_PutStr(self, ",  ");
    Printer_PrintLabel(self, &target);

    Printer_PutStr(self, ");");
    Printer_PutNewline(self);
}

static inline void Printer_PrintCndJmp(Printer* self, const CndJmpBegin* jmp)
{
    Printer_PutStr(self, "cndJmp");
    Printer_PutU32(self, jmp->at.addr);
}

static inline CndJmpBegin Printer_beginCndJmp(Printer* self, const BCValue* cond, _Bool ifTrue)
{
    CndJmpBegin result =
        {.at = {self->vIp}, .cond = cast(BCValue*)cond, .ifTrue = ifTrue};

    Printer_PutStr(self, "CndJmpBegin ");
    Printer_PrintCndJmp(self, &result);
    self->vIp += 2;

    Printer_PutStr(self, " = beginCndJmp(");

    if (cond)
        Printer_PrintBCValue(self, cond);
    else
        Printer_PutChar(self, '0');
    Printer_PutStr(self, ", ");

    Printer_PutBool(self, ifTrue);

    Printer_PutStr(self, ");");
    Printer_PutNewline(self);

    return result;
}

static inline void Printer_endCndJmp(Printer* self, const CndJmpBegin *jmp, BCLabel target)
{
    Printer_PutStr(self, "endCndJmp(");
    Printer_PrintCndJmp(self, jmp);

    Printer_PutStr(self, ", ");
    Printer_PrintLabel(self, &target);

    Printer_PutStr(self, ");");
    Printer_PutNewline(self);
}

PR_OP1(Throw)
PR_OP1(PushCatch)
PR_OP1(PopCatch)
PR_OP1(Ret)

PR_OP2(IToF32)
PR_OP2(IToF64)
PR_OP2(F32ToI)
PR_OP2(F64ToI)
PR_OP2(F32ToF64)
PR_OP2(F64ToF32)

PR_OP3(Memcmp)
PR_OP3(Realloc)

static inline BCValue Printer_run(Printer* self, uint32_t fnIdx, const BCValue* args, uint32_t n_args)
{
    return (BCValue) {BCValueType_Unknown};
}

static inline void Printer_destroy_instance(Printer* self)
{
    free(self);
}

static inline uint32_t Printer_sizeof_instance(void)
{
    return sizeof(Printer);
}

static inline void Printer_init_instance(Printer* intstance)
{
    (*intstance) = (Printer){};
    const uint32_t initialSize = 8192 * 8;

    intstance->BufferStart = intstance->Buffer = (char*)malloc(initialSize);
    intstance->BufferCapacity = initialSize;
    intstance->ErrorInfoCount = 0;
    intstance->ErrorInfoCapacity = 1024;
    intstance->ErrorInfos = (ErrorInfo*) malloc(sizeof(ErrorInfo) * 1024);
}

static inline void Printer_new_instance(Printer** resultP)
{
    Printer* result =  cast(Printer*)malloc(sizeof(Printer));

    const uint32_t initialSize = 8192 * 8;

    result->BufferStart = result->Buffer = (char*)malloc(initialSize);
    result->BufferCapacity = initialSize;
    result->ErrorInfoCount = 0;
    result->ErrorInfoCapacity = 1024;
    result->ErrorInfos = (ErrorInfo*) malloc(sizeof(ErrorInfo) * 1024);
    result->NumberOfParameters = 0;
    result->NumberOfFunctions = 0;
    result->vIp = 0;

    *resultP = result;
}


#ifdef __cplusplus
extern "C"
#endif
const BackendInterface Printer_interface = {
    .name = "Printer",

    .Initialize = (Initialize_t) Printer_Initialize,
    .InitializeV = (InitializeV_t) Printer_InitializeV,
    .Finalize = (Finalize_t) Printer_Finalize,

    .beginFunction = (beginFunction_t) Printer_beginFunction,
    .endFunction = (endFunction_t) Printer_endFunction,

    .genTemporary = (genTemporary_t) Printer_genTemporary,
    .destroyTemporary = (destroyTemporary_t) Printer_destroyTemporary,
    .genLocal = (genLocal_t) Printer_genLocal,
    .genParameter = (genParameter_t) Printer_genParameter,
    .emitFlag = (emitFlag_t) Printer_emitFlag,
    .Alloc = (Alloc_t) Printer_Alloc,
    .Assert = (Assert_t) Printer_Assert,
    .MemCpy = (MemCpy_t) Printer_MemCpy,
    .File = (File_t) Printer_File,
    .Line = (Line_t) Printer_Line,
    .Comment = (Comment_t) Printer_Comment,
    .Prt = (Prt_t) Printer_Prt,
    .Set = (Set_t) Printer_Set,
    .Ult3 = (Ult3_t) Printer_Ult3,
    .Ule3 = (Ule3_t) Printer_Ule3,
    .Lt3 = (Lt3_t) Printer_Lt3,
    .Le3 = (Le3_t) Printer_Le3,
    .Ugt3 = (Ugt3_t) Printer_Ugt3,
    .Uge3 = (Uge3_t) Printer_Uge3,
    .Gt3 = (Gt3_t) Printer_Gt3,
    .Ge3 = (Ge3_t) Printer_Ge3,
    .Eq3 = (Eq3_t) Printer_Eq3,
    .Neq3 = (Neq3_t) Printer_Neq3,
    .Add3 = (Add3_t) Printer_Add3,
    .Sub3 = (Sub3_t) Printer_Sub3,
    .Mul3 = (Mul3_t) Printer_Mul3,
    .Div3 = (Div3_t) Printer_Div3,
    .Udiv3 = (Udiv3_t) Printer_Udiv3,
    .And3 = (And3_t) Printer_And3,
    .Or3 = (Or3_t) Printer_Or3,
    .Xor3 = (Xor3_t) Printer_Xor3,
    .Lsh3 = (Lsh3_t) Printer_Lsh3,
    .Rsh3 = (Rsh3_t) Printer_Rsh3,
    .Mod3 = (Mod3_t) Printer_Mod3,
    .Umod3 = (Umod3_t) Printer_Umod3,
    .Not = (Not_t) Printer_Not,
    .LoadFramePointer = (LoadFramePointer_t) Printer_LoadFramePointer,
    .Call = (Call_t) Printer_Call,
    .genLabel = (genLabel_t) Printer_genLabel,
    .Jmp = (Jmp_t) Printer_Jmp,
    .beginJmp = (beginJmp_t) Printer_beginJmp,
    .endJmp = (endJmp_t) Printer_endJmp,
    .beginCndJmp = (beginCndJmp_t) Printer_beginCndJmp,
    .endCndJmp = (endCndJmp_t) Printer_endCndJmp,
    .Load8 = (Load8_t) Printer_Load8,
    .Store8 = (Store8_t) Printer_Store8,
    .Load16 = (Load16_t) Printer_Load16,
    .Store16 = (Store16_t) Printer_Store16,
    .Load32 = (Load32_t) Printer_Load32,
    .Store32 = (Store32_t) Printer_Store32,
    .Load64 = (Load64_t) Printer_Load64,
    .Store64 = (Store64_t) Printer_Store64,
    .Throw = (Throw_t) Printer_Throw,
    .PushCatch = (PushCatch_t) Printer_PushCatch,
    .PopCatch = (PopCatch_t) Printer_PopCatch,
    .Ret = (Ret_t) Printer_Ret,
    .IToF32 = (IToF32_t) Printer_IToF32,
    .IToF64 = (IToF64_t) Printer_IToF64,
    .F32ToI = (F32ToI_t) Printer_F32ToI,
    .F64ToI = (F64ToI_t) Printer_F64ToI,
    .F32ToF64 = (F32ToF64_t) Printer_F32ToF64,
    .F64ToF32 = (F64ToF32_t) Printer_F64ToF32,
    .Memcmp = (Memcmp_t) Printer_Memcmp,
    .Realloc = (Realloc_t) Printer_Realloc,
    .run = (run_t) Printer_run,
    .destroy_instance = (destroy_instance_t) Printer_destroy_instance,
    .new_instance = (new_instance_t) Printer_new_instance,
    .sizeof_instance = (sizeof_instance_t) Printer_sizeof_instance,
    .init_instance = (init_instance_t) Printer_init_instance,
};
