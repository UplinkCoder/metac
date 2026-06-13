#include <dyncall.h>

#include "bc_common.h"
// ============================================================
// BCValue → libdyncall Argument
// ============================================================
static void PushArg(DCCallVM* vm, const BCValue* arg)
{
    switch (arg->type.type)
    {
        case BCTypeEnum_i8:
        case BCTypeEnum_u8:
            dcArgChar(vm, arg->imm32.imm32);
        break;
        case BCTypeEnum_i16:
        case BCTypeEnum_u16:
            dcArgShort(vm, arg->imm32.imm32);
        break;
        case BCTypeEnum_i32:
        case BCTypeEnum_u32:
            dcArgInt(vm, arg->imm32.imm32);
        break;
            
        case BCTypeEnum_i64:
        case BCTypeEnum_u64:
            dcArgLongLong(vm, arg->imm64.imm64);
        break;
            
        case BCTypeEnum_f23:
            dcArgFloat(vm, *(float*)&arg->imm32.imm32);
        break;
            
        case BCTypeEnum_f52:
            dcArgDouble(vm, *(double*)&arg->imm64.imm64);
        break;
            
        case BCTypeEnum_Ptr:
        case BCTypeEnum_Function:
            dcArgPointer(vm, (void*)(uintptr_t)arg->imm64.imm64);
        break;
            
        default:
            assert(!"Unsupported arg type for external call");
    }
}

void DynCallExternal(void* funcPtr, BCType returnType, BCValue* args, 
                     uint32_t argCount, BCValue* result)
{
    DCCallVM* vm = dcNewCallVM(4096);
    dcMode(vm, DC_CALL_C_DEFAULT);
    
    for (uint32_t i = 0; i < argCount; i++)
    {
        PushArg(vm, &args[i]);
    }

    switch (returnType.type)
    {
        case BCTypeEnum_Void:
            dcCallVoid(vm, funcPtr);
            memset(result, 0, sizeof(*result));
            break;
        case BCTypeEnum_i8:
            (*(int32_t*)&result->imm32.imm32) = dcCallChar(vm, funcPtr); break;
        case BCTypeEnum_i16:
            (*(int32_t*)&result->imm32.imm32) = dcCallShort(vm, funcPtr); break;
        case BCTypeEnum_i32:
            (*(int32_t*)&result->imm32.imm32) = dcCallInt(vm, funcPtr); break;

        case BCTypeEnum_u8:
            result->imm32.imm32 = dcCallChar(vm, funcPtr); break;
        case BCTypeEnum_u16:
            result->imm32.imm32 = dcCallShort(vm, funcPtr); break;
        case BCTypeEnum_u32:

            result->imm32.imm32 = dcCallInt(vm, funcPtr); break;
        case BCTypeEnum_i64:
            (*(int64_t*)&result->imm64.imm64) = dcCallLongLong(vm, funcPtr); break;
        case BCTypeEnum_u64:
            result->imm64.imm64 = ((uint64_t)dcCallLongLong(vm, funcPtr)); break;
        case BCTypeEnum_f23:
            result->imm32.imm32 = (*(uint32_t*)&(float){dcCallFloat(vm, funcPtr)}); break;
        case BCTypeEnum_f52:
            result->imm64.imm64 = (*(uint64_t*)&(double){dcCallDouble(vm, funcPtr)}); break;
        case BCTypeEnum_Ptr:
        case BCTypeEnum_Function:
            result->imm64.imm64 = ((uint64_t)(uintptr_t)dcCallPointer(vm, funcPtr)); break;
        default:
            assert(!"Unsupported return type");
    }
    
    dcFree(vm);
}
