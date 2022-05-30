#include <stdarg.h>
#include <stdint.h>
#include "bc_common.h"

typedef void (*Initialize_t) (void* ctx, uint32_t n_args, ...);
typedef void (*InitializeV_t) (void* ctx, uint32_t n_args, va_list args);
typedef void (*Finalize_t) (void* ctx);

typedef uint32_t (*beginFunction_t) (void* ctx, uint32_t fnId, const void* fd);
typedef void* (*endFunction_t) (void* ctx, uint32_t fnIdx);

typedef BCValue (*genTemporary_t) (void* ctx, BCType bct);
typedef void (*destroyTemporary_t) (void* ctx, BCValue* tmp);

typedef BCValue (*genLocal_t) (void* ctx, BCType bct, const char* name);
typedef BCValue (*genParameter_t) (void* ctx, BCType bct, const char* name);
typedef void (*emitFlag_t) (void* ctx, BCValue* lhs);

typedef void (*Alloc_t) (void* ctx, BCValue *heapPtr, const BCValue* size);
typedef void (*Assert_t) (void* ctx, const BCValue* value, const BCValue* err);
typedef void (*MemCpy_t) (void* ctx, const BCValue* dst, const BCValue* src, const BCValue* size);

typedef void (*File_t) (void* ctx, const char* filename);
typedef void (*Line_t) (void* ctx, uint32_t line);
typedef void (*Comment_t) (void* ctx, const char* comment);
typedef void (*Prt_t) (void* ctx, const BCValue* value, bool isString);

typedef void (*Set_t) (void* ctx, BCValue *lhs, const BCValue* rhs);
typedef void (*Ult3_t) (void* ctx, BCValue *result, const BCValue* lhs, const BCValue* rhs);
typedef void (*Ule3_t) (void* ctx, BCValue *result, const BCValue* lhs, const BCValue* rhs);
typedef void (*Lt3_t) (void* ctx, BCValue *result, const BCValue* lhs, const BCValue* rhs);
typedef void (*Le3_t) (void* ctx, BCValue *result, const BCValue* lhs, const BCValue* rhs);
typedef void (*Ugt3_t) (void* ctx, BCValue *result, const BCValue* lhs, const BCValue* rhs);
typedef void (*Uge3_t) (void* ctx, BCValue *result, const BCValue* lhs, const BCValue* rhs);
typedef void (*Gt3_t) (void* ctx, BCValue *result, const BCValue* lhs, const BCValue* rhs);
typedef void (*Ge3_t) (void* ctx, BCValue *result, const BCValue* lhs, const BCValue* rhs);
typedef void (*Eq3_t) (void* ctx, BCValue *result, const BCValue* lhs, const BCValue* rhs);
typedef void (*Neq3_t) (void* ctx, BCValue *result, const BCValue* lhs, const BCValue* rhs);
typedef void (*Add3_t) (void* ctx, BCValue *result, const BCValue* lhs, const BCValue* rhs);
typedef void (*Sub3_t) (void* ctx, BCValue *result, const BCValue* lhs, const BCValue* rhs);
typedef void (*Mul3_t) (void* ctx, BCValue *result, const BCValue* lhs, const BCValue* rhs);
typedef void (*Div3_t) (void* ctx, BCValue *result, const BCValue* lhs, const BCValue* rhs);
typedef void (*Udiv3_t) (void* ctx, BCValue *result, const BCValue* lhs, const BCValue* rhs);
typedef void (*And3_t) (void* ctx, BCValue *result, const BCValue* lhs, const BCValue* rhs);
typedef void (*Or3_t) (void* ctx, BCValue *result, const BCValue* lhs, const BCValue* rhs);
typedef void (*Xor3_t) (void* ctx, BCValue *result, const BCValue* lhs, const BCValue* rhs);
typedef void (*Lsh3_t) (void* ctx, BCValue *result, const BCValue* lhs, const BCValue* rhs);
typedef void (*Rsh3_t) (void* ctx, BCValue *result, const BCValue* lhs, const BCValue* rhs);
typedef void (*Mod3_t) (void* ctx, BCValue *result, const BCValue* lhs, const BCValue* rhs);
typedef void (*Umod3_t) (void* ctx, BCValue *result, const BCValue* lhs, const BCValue* rhs);
typedef void (*Not_t) (void* ctx, BCValue *result, const BCValue* val);

typedef void (*LoadFramePointer_t) (void* ctx, BCValue *result, const int32_t offset);

typedef void (*Call_t) (void* ctx, BCValue *result, const BCValue* fn, const BCValue* args, uint32_t n_args);
typedef BCLabel (*genLabel_t) (void* ctx);
typedef void (*Jmp_t) (void* ctx, BCLabel target);
typedef uint32_t (*beginJmp_t) (void* ctx);
typedef void (*endJmp_t) (void* ctx, BCAddr atIp, BCLabel target);
typedef CndJmpBegin (*beginCndJmp_t) (void* ctx, const BCValue* cond, bool ifTrue);
typedef void (*endCndJmp_t) (void* ctx, const CndJmpBegin *jmp, BCLabel target);

typedef void (*Load8_t) (void* ctx, BCValue *dest, const BCValue* from);
typedef void (*Store8_t) (void* ctx, BCValue *dest, const BCValue* value);
typedef void (*Load16_t) (void* ctx, BCValue *dest, const BCValue* from);
typedef void (*Store16_t) (void* ctx, BCValue *dest, const BCValue* value);
typedef void (*Load32_t) (void* ctx, BCValue *dest, const BCValue* from);
typedef void (*Store32_t) (void* ctx, BCValue *dest, const BCValue* value);
typedef void (*Load64_t) (void* ctx, BCValue *dest, const BCValue* from);
typedef void (*Store64_t) (void* ctx, BCValue *dest, const BCValue* value);

typedef void (*Throw_t) (void* ctx, const BCValue* e);
typedef void (*PushCatch_t) (void* ctx);
typedef void (*PopCatch_t) (void* ctx);
typedef void (*Ret_t) (void* ctx, const BCValue* val);

typedef void (*IToF32_t) (void* ctx, BCValue *result, const BCValue* rhs);
typedef void (*IToF64_t) (void* ctx, BCValue *result, const BCValue* rhs);
typedef void (*F32ToI_t) (void* ctx, BCValue *result, const BCValue* rhs);
typedef void (*F64ToI_t) (void* ctx, BCValue *result, const BCValue* rhs);
typedef void (*F32ToF64_t) (void* ctx, BCValue *result, const BCValue* rhs);
typedef void (*F64ToF32_t) (void* ctx, BCValue *result, const BCValue* rhs);

typedef void (*Memcmp_t) (void* ctx, BCValue *result, const BCValue* lhs, const BCValue* rhs);
typedef void (*Realloc_t) (void* ctx, BCValue *result, const BCValue* lhs, const BCValue* rhs, const uint32_t size);

typedef BCValue (*run_t) (void* ctx, uint32_t fnIdx, const BCValue* args, uint32_t n_args);
typedef void (*destroy_instance_t) (void* ctx);
typedef void (*new_instance_t) (void ** result_p);
typedef uint32_t (*sizeof_instance_t) (void);

typedef void (*ReadI32_t) (void* ctx, const BCValue* val, const ReadI32_cb_t readCb, void* userCtx);
typedef void (*ReadI32_cb_t)(uint32_t value, void* userCtx);

typedef struct BackendInterface
{
    const char* name;

    const void (*Initialize) (void* ctx, uint32_t n_args, ...);
    const void (*InitializeV) (void* ctx, uint32_t n_args, va_list args);
    const void (*Finalize) (void* ctx);

    const uint32_t (*beginFunction) (void* ctx, uint32_t fnId, const void* fd);
    const void* (*endFunction) (void* ctx, uint32_t fnIdx);

    const BCValue (*genTemporary) (void* ctx, BCType bct);
    const void (*destroyTemporary) (void* ctx, BCValue* tmp);

    const BCValue (*genLocal) (void* ctx, BCType bct, const char* name);
    const BCValue (*genParameter) (void* ctx, BCType bct, const char* name);
    const void (*emitFlag) (void* ctx, BCValue* lhs);

    const void (*Alloc) (void* ctx, BCValue *heapPtr, const BCValue* size);
    const void (*Assert) (void* ctx, const BCValue* value, const BCValue* err);
    const void (*MemCpy) (void* ctx, const BCValue* dst, const BCValue* src, const BCValue* size);

    const void (*File) (void* ctx, const char* filename);
    const void (*Line) (void* ctx, uint32_t line);
    const void (*Comment) (void* ctx, const char* comment);
    const void (*Prt) (void* ctx, const BCValue* value, bool isString);

    const void (*Set) (void* ctx, BCValue *lhs, const BCValue* rhs);
    const void (*Ult3) (void* ctx, BCValue *result, const BCValue* lhs, const BCValue* rhs);
    const void (*Ule3) (void* ctx, BCValue *result, const BCValue* lhs, const BCValue* rhs);
    const void (*Lt3) (void* ctx, BCValue *result, const BCValue* lhs, const BCValue* rhs);
    const void (*Le3) (void* ctx, BCValue *result, const BCValue* lhs, const BCValue* rhs);
    const void (*Ugt3) (void* ctx, BCValue *result, const BCValue* lhs, const BCValue* rhs);
    const void (*Uge3) (void* ctx, BCValue *result, const BCValue* lhs, const BCValue* rhs);
    const void (*Gt3) (void* ctx, BCValue *result, const BCValue* lhs, const BCValue* rhs);
    const void (*Ge3) (void* ctx, BCValue *result, const BCValue* lhs, const BCValue* rhs);
    const void (*Eq3) (void* ctx, BCValue *result, const BCValue* lhs, const BCValue* rhs);
    const void (*Neq3) (void* ctx, BCValue *result, const BCValue* lhs, const BCValue* rhs);
    const void (*Add3) (void* ctx, BCValue *result, const BCValue* lhs, const BCValue* rhs);
    const void (*Sub3) (void* ctx, BCValue *result, const BCValue* lhs, const BCValue* rhs);
    const void (*Mul3) (void* ctx, BCValue *result, const BCValue* lhs, const BCValue* rhs);
    const void (*Div3) (void* ctx, BCValue *result, const BCValue* lhs, const BCValue* rhs);
    const void (*Udiv3) (void* ctx, BCValue *result, const BCValue* lhs, const BCValue* rhs);
    const void (*And3) (void* ctx, BCValue *result, const BCValue* lhs, const BCValue* rhs);
    const void (*Or3) (void* ctx, BCValue *result, const BCValue* lhs, const BCValue* rhs);
    const void (*Xor3) (void* ctx, BCValue *result, const BCValue* lhs, const BCValue* rhs);
    const void (*Lsh3) (void* ctx, BCValue *result, const BCValue* lhs, const BCValue* rhs);
    const void (*Rsh3) (void* ctx, BCValue *result, const BCValue* lhs, const BCValue* rhs);
    const void (*Mod3) (void* ctx, BCValue *result, const BCValue* lhs, const BCValue* rhs);
    const void (*Umod3) (void* ctx, BCValue *result, const BCValue* lhs, const BCValue* rhs);
    const void (*Not) (void* ctx, BCValue *result, const BCValue* val);

    const void (*LoadFramePointer) (void* ctx, BCValue *result, const int32_t offset);

    const void (*Call) (void* ctx, BCValue *result, const BCValue* fn, const BCValue* args, uint32_t n_args);
    const BCLabel (*genLabel) (void* ctx);
    const void (*Jmp) (void* ctx, BCLabel target);
    const uint32_t (*beginJmp) (void* ctx);
    const void (*endJmp) (void* ctx, BCAddr atIp, BCLabel target);
    const CndJmpBegin (*beginCndJmp) (void* ctx, const BCValue* cond, bool ifTrue);
    const void (*endCndJmp) (void* ctx, const CndJmpBegin *jmp, BCLabel target);

    const void (*Load8) (void* ctx, BCValue *dest, const BCValue* from);
    const void (*Store8) (void* ctx, BCValue *dest, const BCValue* value);
    const void (*Load16) (void* ctx, BCValue *dest, const BCValue* from);
    const void (*Store16) (void* ctx, BCValue *dest, const BCValue* value);
    const void (*Load32) (void* ctx, BCValue *dest, const BCValue* from);
    const void (*Store32) (void* ctx, BCValue *dest, const BCValue* value);
    const void (*Load64) (void* ctx, BCValue *dest, const BCValue* from);
    const void (*Store64) (void* ctx, BCValue *dest, const BCValue* value);

    const void (*Throw) (void* ctx, const BCValue* e);
    const void (*PushCatch) (void* ctx);
    const void (*PopCatch) (void* ctx);
    const void (*Ret) (void* ctx, const BCValue* val);

    const void (*IToF32) (void* ctx, BCValue *result, const BCValue* rhs);
    const void (*IToF64) (void* ctx, BCValue *result, const BCValue* rhs);
    const void (*F32ToI) (void* ctx, BCValue *result, const BCValue* rhs);
    const void (*F64ToI) (void* ctx, BCValue *result, const BCValue* rhs);
    const void (*F32ToF64) (void* ctx, BCValue *result, const BCValue* rhs);
    const void (*F64ToF32) (void* ctx, BCValue *result, const BCValue* rhs);

    const void (*Memcmp) (void* ctx, BCValue *result, const BCValue* lhs, const BCValue* rhs);
    const void (*Realloc) (void* ctx, BCValue *result, const BCValue* lhs, const BCValue* rhs, const uint32_t size);

    const BCValue (*run) (void* ctx, uint32_t fnIdx, const BCValue* args, uint32_t n_args);
    const void (*destroy_instance) (void* ctx);
    const void (*new_instance) (void ** result_p);
    const uint32_t (*sizeof_instance) (void);

    const void (*ReadI32) (void* ctx, const BCValue* val, const ReadI32_cb_t readCb, void* userCtx);
} BackendInterface;
