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

    void (*const Initialize) (void* ctx, uint32_t n_args, ...);
    void (*const InitializeV) (void* ctx, uint32_t n_args, va_list args);
    void (*const Finalize) (void* ctx);

    uint32_t (*const beginFunction) (void* ctx, uint32_t fnId, const void* fd);
    void* (*const endFunction) (void* ctx, uint32_t fnIdx);

    BCValue (*const genTemporary) (void* ctx, BCType bct);
    void (*const destroyTemporary) (void* ctx, BCValue* tmp);

    BCValue (*const genLocal) (void* ctx, BCType bct, const char* name);
    BCValue (*const genParameter) (void* ctx, BCType bct, const char* name);
    void (*const emitFlag) (void* ctx, BCValue* lhs);

    void (*const Alloc) (void* ctx, BCValue *heapPtr, const BCValue* size);
    void (*const Assert) (void* ctx, const BCValue* value, const BCValue* err);
    void (*const MemCpy) (void* ctx, const BCValue* dst, const BCValue* src, const BCValue* size);

    void (*const File) (void* ctx, const char* filename);
    void (*const Line) (void* ctx, uint32_t line);
    void (*const Comment) (void* ctx, const char* comment);
    void (*const Prt) (void* ctx, const BCValue* value, bool isString);

    void (*const Set) (void* ctx, BCValue *lhs, const BCValue* rhs);
    void (*const Ult3) (void* ctx, BCValue *result, const BCValue* lhs, const BCValue* rhs);
    void (*const Ule3) (void* ctx, BCValue *result, const BCValue* lhs, const BCValue* rhs);
    void (*const Lt3) (void* ctx, BCValue *result, const BCValue* lhs, const BCValue* rhs);
    void (*const Le3) (void* ctx, BCValue *result, const BCValue* lhs, const BCValue* rhs);
    void (*const Ugt3) (void* ctx, BCValue *result, const BCValue* lhs, const BCValue* rhs);
    void (*const Uge3) (void* ctx, BCValue *result, const BCValue* lhs, const BCValue* rhs);
    void (*const Gt3) (void* ctx, BCValue *result, const BCValue* lhs, const BCValue* rhs);
    void (*const Ge3) (void* ctx, BCValue *result, const BCValue* lhs, const BCValue* rhs);
    void (*const Eq3) (void* ctx, BCValue *result, const BCValue* lhs, const BCValue* rhs);
    void (*const Neq3) (void* ctx, BCValue *result, const BCValue* lhs, const BCValue* rhs);
    void (*const Add3) (void* ctx, BCValue *result, const BCValue* lhs, const BCValue* rhs);
    void (*const Sub3) (void* ctx, BCValue *result, const BCValue* lhs, const BCValue* rhs);
    void (*const Mul3) (void* ctx, BCValue *result, const BCValue* lhs, const BCValue* rhs);
    void (*const Div3) (void* ctx, BCValue *result, const BCValue* lhs, const BCValue* rhs);
    void (*const Udiv3) (void* ctx, BCValue *result, const BCValue* lhs, const BCValue* rhs);
    void (*const And3) (void* ctx, BCValue *result, const BCValue* lhs, const BCValue* rhs);
    void (*const Or3) (void* ctx, BCValue *result, const BCValue* lhs, const BCValue* rhs);
    void (*const Xor3) (void* ctx, BCValue *result, const BCValue* lhs, const BCValue* rhs);
    void (*const Lsh3) (void* ctx, BCValue *result, const BCValue* lhs, const BCValue* rhs);
    void (*const Rsh3) (void* ctx, BCValue *result, const BCValue* lhs, const BCValue* rhs);
    void (*const Mod3) (void* ctx, BCValue *result, const BCValue* lhs, const BCValue* rhs);
    void (*const Umod3) (void* ctx, BCValue *result, const BCValue* lhs, const BCValue* rhs);
    void (*const Not) (void* ctx, BCValue *result, const BCValue* val);

    void (*const LoadFramePointer) (void* ctx, BCValue *result, const int32_t offset);

    void (*const Call) (void* ctx, BCValue *result, const BCValue* fn, const BCValue* args, uint32_t n_args);
    BCLabel (*const genLabel) (void* ctx);
    void (*const Jmp) (void* ctx, BCLabel target);
    uint32_t (*const beginJmp) (void* ctx);
    void (*const endJmp) (void* ctx, BCAddr atIp, BCLabel target);
    CndJmpBegin (*const beginCndJmp) (void* ctx, const BCValue* cond, bool ifTrue);
    void (*const endCndJmp) (void* ctx, const CndJmpBegin *jmp, BCLabel target);

    void (*const Load8) (void* ctx, BCValue *dest, const BCValue* from);
    void (*const Store8) (void* ctx, BCValue *dest, const BCValue* value);
    void (*const Load16) (void* ctx, BCValue *dest, const BCValue* from);
    void (*const Store16) (void* ctx, BCValue *dest, const BCValue* value);
    void (*const Load32) (void* ctx, BCValue *dest, const BCValue* from);
    void (*const Store32) (void* ctx, BCValue *dest, const BCValue* value);
    void (*const Load64) (void* ctx, BCValue *dest, const BCValue* from);
    void (*const Store64) (void* ctx, BCValue *dest, const BCValue* value);

    void (*const Throw) (void* ctx, const BCValue* e);
    void (*const PushCatch) (void* ctx);
    void (*const PopCatch) (void* ctx);
    void (*const Ret) (void* ctx, const BCValue* val);

    void (*const IToF32) (void* ctx, BCValue *result, const BCValue* rhs);
    void (*const IToF64) (void* ctx, BCValue *result, const BCValue* rhs);
    void (*const F32ToI) (void* ctx, BCValue *result, const BCValue* rhs);
    void (*const F64ToI) (void* ctx, BCValue *result, const BCValue* rhs);
    void (*const F32ToF64) (void* ctx, BCValue *result, const BCValue* rhs);
    void (*const F64ToF32) (void* ctx, BCValue *result, const BCValue* rhs);

    void (*const Memcmp) (void* ctx, BCValue *result, const BCValue* lhs, const BCValue* rhs);
    void (*const Realloc) (void* ctx, BCValue *result, const BCValue* lhs, const BCValue* rhs, const uint32_t size);

    BCValue (*const run) (void* ctx, uint32_t fnIdx, const BCValue* args, uint32_t n_args);
    void (*const destroy_instance) (void* ctx);
    void (*const new_instance) (void ** result_p);
    uint32_t (*const sizeof_instance) (void);

    void (*const ReadI32) (void* ctx, const BCValue* val, const ReadI32_cb_t readCb, void* userCtx);
} BackendInterface;
