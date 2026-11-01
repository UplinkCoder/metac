/**
 * Standalone test harness for Lightning backend
 * Tests basic operations without relying on codegen/semantic layers
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

#include "../libinterpret/bc_common.c"
#include "../libinterpret/bc_lightning_backend.c"

typedef struct {
    uint32_t test_count;
    uint32_t pass_count;
    uint32_t fail_count;
} test_stats_t;

void test_record(test_stats_t* stats, const char* name, int passed) {
    stats->test_count++;
    if (passed) {
        stats->pass_count++;
        printf("[PASS] %s\n", name);
    } else {
        stats->fail_count++;
        printf("[FAIL] %s\n", name);
    }
}

// Simple allocator for testing
static void* test_alloc(void* ctx, uint32_t size, void* fn) {
    return malloc(size);
}

static BCTypeInfo test_get_typeinfo(void* ctx, BCType* type) {
    return (BCTypeInfo){0};
}

/**
 * Create a backend context using the BackendInterface pattern
 */
typedef struct {
    const BackendInterface* backend;
    void* ctx;
    BCHeap heap;
} TestBackend;

TestBackend* TestBackend_Create(void) {
    TestBackend* tb = (TestBackend*)malloc(sizeof(TestBackend));
    
    // Use Lightning backend
    tb->backend = &Lightning_interface;
    
    // Allocate backend instance
    uint32_t ctx_size = tb->backend->sizeof_instance();
    tb->ctx = malloc(ctx_size);
    memset(tb->ctx, 0, ctx_size);
    
    // Create heap
    tb->heap.heapData = malloc(1024 * 1024);
    tb->heap.heapMax = 1024 * 1024;
    tb->heap.heapSize = 4;
    
    // Set up callbacks
    if (tb->backend->set_alloc_memory) {
        tb->backend->set_alloc_memory(tb->ctx, test_alloc, NULL);
    }
    
    if (tb->backend->set_get_typeinfo) {
        tb->backend->set_get_typeinfo(tb->ctx, test_get_typeinfo, NULL);
    }
    
    // Initialize instance
    tb->backend->init_instance(tb->ctx);
    
    // Initialize backend
    tb->backend->Initialize(tb->ctx, 0);
    
    return tb;
}

void TestBackend_Destroy(TestBackend* tb) {
    if (tb) {
        tb->backend->Finalize(tb->ctx);
        tb->backend->fini_instance(tb->ctx);
        free(tb->ctx);
        free(tb->heap.heapData);
        free(tb);
    }
}

/**
 * Test 1: Basic arithmetic (1 + 2 = 3)
 */
void test_simple_add(test_stats_t* stats) {
    TestBackend* tb = TestBackend_Create();
    
    uint32_t fnIdx = tb->backend->BeginFunction(tb->ctx, 0, NULL);
    
    BCValue lhs = imm32(1);
    BCValue rhs = imm32(2);
    BCValue result = tb->backend->GenTemporary(tb->ctx, (BCType){BCTypeEnum_i32, 0, BCTypeFlags_None});
    
    tb->backend->Add3(tb->ctx, &result, &lhs, &rhs);
    tb->backend->Ret(tb->ctx, &result);
    
    tb->backend->EndFunction(tb->ctx, fnIdx);
    tb->backend->Finalize(tb->ctx);
    
    BCValue args[] = {};
    BCValue output = tb->backend->Run(tb->ctx, fnIdx, args, 0, &tb->heap);
    
    int passed = (output.vType == BCValueType_Immediate && output.imm32.imm32 == 3);
    test_record(stats, "Simple addition (1+2=3)", passed);
    
    TestBackend_Destroy(tb);
}

/**
 * Test 2: Subtraction (10 - 3 = 7)
 */
void test_simple_sub(test_stats_t* stats) {
    TestBackend* tb = TestBackend_Create();
    
    uint32_t fnIdx = tb->backend->BeginFunction(tb->ctx, 0, NULL);
    
    BCValue lhs = imm32(10);
    BCValue rhs = imm32(3);
    BCValue result = tb->backend->GenTemporary(tb->ctx, (BCType){BCTypeEnum_i32, 0, BCTypeFlags_None});
    
    tb->backend->Sub3(tb->ctx, &result, &lhs, &rhs);
    tb->backend->Ret(tb->ctx, &result);
    
    tb->backend->EndFunction(tb->ctx, fnIdx);
    tb->backend->Finalize(tb->ctx);
    
    BCValue args[] = {};
    BCValue output = tb->backend->Run(tb->ctx, fnIdx, args, 0, &tb->heap);
    
    int passed = (output.vType == BCValueType_Immediate && output.imm32.imm32 == 7);
    test_record(stats, "Simple subtraction (10-3=7)", passed);
    
    TestBackend_Destroy(tb);
}

/**
 * Test 3: Multiplication (6 * 7 = 42)
 */
void test_simple_mul(test_stats_t* stats) {
    TestBackend* tb = TestBackend_Create();
    
    uint32_t fnIdx = tb->backend->BeginFunction(tb->ctx, 0, NULL);
    
    BCValue lhs = imm32(6);
    BCValue rhs = imm32(7);
    BCValue result = tb->backend->GenTemporary(tb->ctx, (BCType){BCTypeEnum_i32, 0, BCTypeFlags_None});
    
    tb->backend->Mul3(tb->ctx, &result, &lhs, &rhs);
    tb->backend->Ret(tb->ctx, &result);
    
    tb->backend->EndFunction(tb->ctx, fnIdx);
    tb->backend->Finalize(tb->ctx);
    
    BCValue args[] = {};
    BCValue output = tb->backend->Run(tb->ctx, fnIdx, args, 0, &tb->heap);
    
    int passed = (output.vType == BCValueType_Immediate && output.imm32.imm32 == 42);
    test_record(stats, "Simple multiplication (6*7=42)", passed);
    
    TestBackend_Destroy(tb);
}

/**
 * Test 4: Integer division (20 / 4 = 5)
 */
void test_simple_div(test_stats_t* stats) {
    TestBackend* tb = TestBackend_Create();
    
    uint32_t fnIdx = tb->backend->BeginFunction(tb->ctx, 0, NULL);
    
    BCValue lhs = imm32(20);
    BCValue rhs = imm32(4);
    BCValue result = tb->backend->GenTemporary(tb->ctx, (BCType){BCTypeEnum_i32, 0, BCTypeFlags_None});
    
    tb->backend->Div3(tb->ctx, &result, &lhs, &rhs);
    tb->backend->Ret(tb->ctx, &result);
    
    tb->backend->EndFunction(tb->ctx, fnIdx);
    tb->backend->Finalize(tb->ctx);
    
    BCValue args[] = {};
    BCValue output = tb->backend->Run(tb->ctx, fnIdx, args, 0, &tb->heap);
    
    int passed = (output.vType == BCValueType_Immediate && output.imm32.imm32 == 5);
    test_record(stats, "Simple division (20/4=5)", passed);
    
    TestBackend_Destroy(tb);
}

/**
 * Test 5: Bitwise AND (12 & 10 = 8)
 */
void test_simple_and(test_stats_t* stats) {
    TestBackend* tb = TestBackend_Create();
    
    uint32_t fnIdx = tb->backend->BeginFunction(tb->ctx, 0, NULL);
    
    BCValue lhs = imm32(12);  // 1100
    BCValue rhs = imm32(10);  // 1010
    BCValue result = tb->backend->GenTemporary(tb->ctx, (BCType){BCTypeEnum_i32, 0, BCTypeFlags_None});
    
    tb->backend->And3(tb->ctx, &result, &lhs, &rhs);
    tb->backend->Ret(tb->ctx, &result);
    
    tb->backend->EndFunction(tb->ctx, fnIdx);
    tb->backend->Finalize(tb->ctx);
    
    BCValue args[] = {};
    BCValue output = tb->backend->Run(tb->ctx, fnIdx, args, 0, &tb->heap);
    
    int passed = (output.vType == BCValueType_Immediate && output.imm32.imm32 == 8);
    test_record(stats, "Bitwise AND (12&10=8)", passed);
    
    TestBackend_Destroy(tb);
}

/**
 * Test 6: Comparison (5 < 10 = true/1)
 */
void test_simple_lt(test_stats_t* stats) {
    TestBackend* tb = TestBackend_Create();
    
    uint32_t fnIdx = tb->backend->BeginFunction(tb->ctx, 0, NULL);
    
    BCValue lhs = imm32(5);
    BCValue rhs = imm32(10);
    BCValue result = tb->backend->GenTemporary(tb->ctx, (BCType){BCTypeEnum_i32, 0, BCTypeFlags_None});
    
    tb->backend->Lt3(tb->ctx, &result, &lhs, &rhs);
    tb->backend->Ret(tb->ctx, &result);
    
    tb->backend->EndFunction(tb->ctx, fnIdx);
    tb->backend->Finalize(tb->ctx);
    
    BCValue args[] = {};
    BCValue output = tb->backend->Run(tb->ctx, fnIdx, args, 0, &tb->heap);
    
    int passed = (output.vType == BCValueType_Immediate && output.imm32.imm32 == 1);
    test_record(stats, "Comparison (5<10=true)", passed);
    
    TestBackend_Destroy(tb);
}

/**
 * Test 7: Unconditional forward jump skips an assignment
 */
void test_jmp_forward(test_stats_t* stats) {
    TestBackend* tb = TestBackend_Create();

    uint32_t fnIdx = tb->backend->BeginFunction(tb->ctx, 0, NULL);

    BCValue result = tb->backend->GenTemporary(tb->ctx, (BCType){BCTypeEnum_i32, 0, BCTypeFlags_None});
    BCValue one = imm32(1);
    BCValue two = imm32(2);
    tb->backend->Set(tb->ctx, &result, &one);

    BCAddr j = tb->backend->BeginJmp(tb->ctx);
    tb->backend->Set(tb->ctx, &result, &two);
    tb->backend->EndJmp(tb->ctx, j, tb->backend->GenLabel(tb->ctx));

    tb->backend->Ret(tb->ctx, &result);
    tb->backend->EndFunction(tb->ctx, fnIdx);
    tb->backend->Finalize(tb->ctx);

    BCValue args[] = {};
    BCValue output = tb->backend->Run(tb->ctx, fnIdx, args, 0, &tb->heap);

    int passed = (output.vType == BCValueType_Immediate && output.imm32.imm32 == 1);
    test_record(stats, "Unconditional forward jump (skips assignment)", passed);

    TestBackend_Destroy(tb);
}

/**
 * Test 8: Conditional forward jump, taken and not taken
 */
void test_cndjmp_forward(test_stats_t* stats) {
    int passed = 1;

    for (int i = 0; i < 2; i++) {
        TestBackend* tb = TestBackend_Create();

        uint32_t fnIdx = tb->backend->BeginFunction(tb->ctx, 0, NULL);

        BCValue result = tb->backend->GenTemporary(tb->ctx, (BCType){BCTypeEnum_i32, 0, BCTypeFlags_None});
        BCValue zero = imm32(1);
        BCValue cond = imm32(i);   // 0: jump not taken for ifTrue; 1: taken
        BCValue five = imm32(5);
        tb->backend->Set(tb->ctx, &result, &zero);

        CndJmpBegin j = tb->backend->BeginCndJmp(tb->ctx, &cond, true);
        tb->backend->Set(tb->ctx, &result, &five);
        tb->backend->EndCndJmp(tb->ctx, &j, tb->backend->GenLabel(tb->ctx));

        tb->backend->Ret(tb->ctx, &result);
        tb->backend->EndFunction(tb->ctx, fnIdx);
        tb->backend->Finalize(tb->ctx);

        BCValue args[] = {};
        BCValue output = tb->backend->Run(tb->ctx, fnIdx, args, 0, &tb->heap);

        // cond != 0 jumps over the assignment, so result stays 1
        int expected = (i == 0) ? 5 : 1;
        if (!(output.vType == BCValueType_Immediate && output.imm32.imm32 == expected))
            passed = 0;

        TestBackend_Destroy(tb);
    }

    test_record(stats, "Conditional forward jump (taken and not taken)", passed);
}

/**
 * Test 9: Local variable round-trip
 */
void test_local(test_stats_t* stats) {
    TestBackend* tb = TestBackend_Create();

    uint32_t fnIdx = tb->backend->BeginFunction(tb->ctx, 0, NULL);

    BCValue local = tb->backend->GenLocal(tb->ctx, (BCType){BCTypeEnum_i32, 0, BCTypeFlags_None}, "x");
    BCValue value = imm32(77);
    tb->backend->Set(tb->ctx, &local, &value);
    tb->backend->Ret(tb->ctx, &local);
    tb->backend->EndFunction(tb->ctx, fnIdx);
    tb->backend->Finalize(tb->ctx);

    BCValue args[] = {};
    BCValue output = tb->backend->Run(tb->ctx, fnIdx, args, 0, &tb->heap);

    int passed = (output.vType == BCValueType_Immediate && output.imm32.imm32 == 77);
    test_record(stats, "Local variable round-trip (77)", passed);

    TestBackend_Destroy(tb);
}

static int32_t read_i32_seen = 0;
static int read_i32_calls = 0;

static void test_read_i32_cb(uint32_t value, void* userCtx) {
    read_i32_seen = (int32_t)value;
    read_i32_calls++;
}

/**
 * Test 10: ReadI32 invokes the registered callback at runtime
 */
void test_read_i32(test_stats_t* stats) {
    TestBackend* tb = TestBackend_Create();

    read_i32_seen = 0;
    read_i32_calls = 0;

    uint32_t fnIdx = tb->backend->BeginFunction(tb->ctx, 0, NULL);

    BCValue value = imm32(42);
    tb->backend->ReadI32(tb->ctx, &value, test_read_i32_cb, NULL);
    BCValue zero = imm32(0);
    tb->backend->Ret(tb->ctx, &zero);
    tb->backend->EndFunction(tb->ctx, fnIdx);
    tb->backend->Finalize(tb->ctx);

    BCValue args[] = {};
    tb->backend->Run(tb->ctx, fnIdx, args, 0, &tb->heap);

    int passed = (read_i32_calls == 1 && read_i32_seen == 42);
    test_record(stats, "ReadI32 callback receives value (42)", passed);

    TestBackend_Destroy(tb);
}

/**
 * Test 11: 64-bit store then load through the heap
 */
void test_store_load64(test_stats_t* stats) {
    TestBackend* tb = TestBackend_Create();

    uint32_t fnIdx = tb->backend->BeginFunction(tb->ctx, 0, NULL);

    BCType i64t = {BCTypeEnum_i64, 0, BCTypeFlags_None};
    BCValue addr = imm32(AddressFrom(AddressKind_Heap, 16));
    BCValue value = {BCValueType_Immediate};
    value.type = i64t;
    value.imm64.imm64 = 0x123456789LL;
    tb->backend->Store64(tb->ctx, &addr, &value);

    BCValue loaded = tb->backend->GenTemporary(tb->ctx, i64t);
    tb->backend->Load64(tb->ctx, &loaded, &addr);
    tb->backend->Ret(tb->ctx, &loaded);
    tb->backend->EndFunction(tb->ctx, fnIdx);
    tb->backend->Finalize(tb->ctx);

    BCValue args[] = {};
    BCValue output = tb->backend->Run(tb->ctx, fnIdx, args, 0, &tb->heap);

    int passed = (output.vType == BCValueType_Immediate && output.imm32.imm32 == (int32_t)0x123456789LL);
    test_record(stats, "Store64/Load64 round-trip (low 32 bits)", passed);

    TestBackend_Destroy(tb);
}

/**
 * Test 12: int -> f32 -> int conversion round-trip through the frame
 */
void test_fp_roundtrip(test_stats_t* stats) {
    TestBackend* tb = TestBackend_Create();

    uint32_t fnIdx = tb->backend->BeginFunction(tb->ctx, 0, NULL);

    BCType i32t = {BCTypeEnum_i32, 0, BCTypeFlags_None};
    BCType f32t = {BCTypeEnum_f23, 0, BCTypeFlags_None};

    BCValue seven = imm32(7);
    BCValue asInt = tb->backend->GenTemporary(tb->ctx, i32t);
    tb->backend->Set(tb->ctx, &asInt, &seven);

    BCValue asFloat = tb->backend->GenTemporary(tb->ctx, f32t);
    tb->backend->IToF32(tb->ctx, &asFloat, &asInt);

    BCValue back = tb->backend->GenTemporary(tb->ctx, i32t);
    tb->backend->F32ToI(tb->ctx, &back, &asFloat);
    tb->backend->Ret(tb->ctx, &back);
    tb->backend->EndFunction(tb->ctx, fnIdx);
    tb->backend->Finalize(tb->ctx);

    BCValue args[] = {};
    BCValue output = tb->backend->Run(tb->ctx, fnIdx, args, 0, &tb->heap);

    int passed = (output.vType == BCValueType_Immediate && output.imm32.imm32 == 7);
    test_record(stats, "IToF32/F32ToI round-trip (7)", passed);

    TestBackend_Destroy(tb);
}

/**
 * Test 13a (debug): call a zero-argument generated function
 */
void test_call_no_args(test_stats_t* stats) {
    TestBackend* tb = TestBackend_Create();

    BCType i32t = {BCTypeEnum_i32, 0, BCTypeFlags_None};
    BCType fnt = {BCTypeEnum_Function, 0, BCTypeFlags_None};

    uint32_t calleeIdx = tb->backend->BeginFunction(tb->ctx, 0, NULL);
    BCValue c7 = imm32(7);
    tb->backend->Ret(tb->ctx, &c7);
    tb->backend->EndFunction(tb->ctx, calleeIdx);

    uint32_t callerIdx = tb->backend->BeginFunction(tb->ctx, 0, NULL);
    BCValue res = tb->backend->GenTemporary(tb->ctx, i32t);
    BCValue callee = imm32(calleeIdx);
    callee.type = fnt;
    tb->backend->Call(tb->ctx, &res, &callee, NULL, 0);
    tb->backend->Ret(tb->ctx, &res);
    tb->backend->EndFunction(tb->ctx, callerIdx);

    tb->backend->Finalize(tb->ctx);
    BCValue args[] = {};
    BCValue output = tb->backend->Run(tb->ctx, callerIdx, args, 0, &tb->heap);

    int passed = (output.vType == BCValueType_Immediate && output.imm32.imm32 == 7);
    test_record(stats, "Call with no arguments (7)", passed);

    TestBackend_Destroy(tb);
}

/**
 * Test 13: call a generated function with a frame-local argument (41 + 1 = 42)
 */
void test_call_with_args(test_stats_t* stats) {
    TestBackend* tb = TestBackend_Create();

    BCType i32t = {BCTypeEnum_i32, 0, BCTypeFlags_None};
    BCType fnt = {BCTypeEnum_Function, 0, BCTypeFlags_None};

    uint32_t calleeIdx = tb->backend->BeginFunction(tb->ctx, 0, NULL);
    BCValue param = tb->backend->GenParameter(tb->ctx, i32t, "a");
    BCValue one = imm32(1);
    BCValue sum = tb->backend->GenTemporary(tb->ctx, i32t);
    tb->backend->Add3(tb->ctx, &sum, &param, &one);
    tb->backend->Ret(tb->ctx, &sum);
    tb->backend->EndFunction(tb->ctx, calleeIdx);

    uint32_t callerIdx = tb->backend->BeginFunction(tb->ctx, 0, NULL);
    BCValue x = tb->backend->GenLocal(tb->ctx, i32t, "x");
    BCValue forty1 = imm32(41);
    tb->backend->Set(tb->ctx, &x, &forty1);

    BCValue res = tb->backend->GenTemporary(tb->ctx, i32t);
    BCValue callee = imm32(calleeIdx);
    callee.type = fnt;
    BCValue callArgs[] = { x };
    tb->backend->Call(tb->ctx, &res, &callee, callArgs, 1);
    tb->backend->Ret(tb->ctx, &res);
    tb->backend->EndFunction(tb->ctx, callerIdx);

    tb->backend->Finalize(tb->ctx);
    BCValue args[] = {};
    BCValue output = tb->backend->Run(tb->ctx, callerIdx, args, 0, &tb->heap);

    int passed = (output.vType == BCValueType_Immediate && output.imm32.imm32 == 42);
    test_record(stats, "Call with local argument (41+1=42)", passed);

    TestBackend_Destroy(tb);
}

/**
 * Test: a function with a parameter and a local, with File/Line marks and a debug ELF
 * written after Finalize. test_debug_hook is a gdb stop point: add-symbol-file the ELF there.
 */
__attribute__((noinline)) void test_debug_hook(const void* jitCode) {
    printf("JIT code base: %p\n", jitCode);
}

void test_debug_elf(test_stats_t* stats) {
    TestBackend* tb = TestBackend_Create();

    BCType i32t = {BCTypeEnum_i32, 0, BCTypeFlags_None};

    tb->backend->File(tb->ctx, "dbg_source.bc");
    tb->backend->Line(tb->ctx, 1);
    uint32_t fnIdx = tb->backend->BeginFunction(tb->ctx, 0, NULL);
    BCValue param = tb->backend->GenParameter(tb->ctx, i32t, "a");
    tb->backend->Line(tb->ctx, 2);
    BCValue local = tb->backend->GenLocal(tb->ctx, i32t, "x");
    BCValue one = imm32(1);
    tb->backend->Add3(tb->ctx, &local, &param, &one);
    tb->backend->Line(tb->ctx, 3);
    tb->backend->Ret(tb->ctx, &local);
    tb->backend->EndFunction(tb->ctx, fnIdx);

    tb->backend->Finalize(tb->ctx);
    int written = Lightning_WriteDebugElf((Lightning*) tb->ctx, "lightning_debug.elf");
    test_record(stats, "Debug ELF written", written == 0);
    test_debug_hook((const void*) ((Lightning*) tb->ctx)->FunctionPtrs[fnIdx]);

    BCValue args[] = { imm32(41) };
    BCValue output = tb->backend->Run(tb->ctx, fnIdx, args, 1, &tb->heap);

    int passed = (output.vType == BCValueType_Immediate && output.imm32.imm32 == 42);
    test_record(stats, "Parameter and local with debug info (41+1=42)", passed);

    TestBackend_Destroy(tb);
}

/**
 * Test 14: EmitFlag materializes the most recent comparison result
 */
void test_emit_flag(test_stats_t* stats) {
    TestBackend* tb = TestBackend_Create();

    BCType i32t = {BCTypeEnum_i32, 0, BCTypeFlags_None};

    uint32_t lastIdx = tb->backend->BeginFunction(tb->ctx, 0, NULL);
    BCValue lt = tb->backend->GenTemporary(tb->ctx, i32t);
    BCValue five = imm32(5);
    BCValue ten = imm32(10);
    tb->backend->Lt3(tb->ctx, &lt, &five, &ten);
    BCValue eq = tb->backend->GenTemporary(tb->ctx, i32t);
    BCValue three = imm32(3);
    BCValue four = imm32(4);
    tb->backend->Eq3(tb->ctx, &eq, &three, &four);
    BCValue flag = tb->backend->GenTemporary(tb->ctx, i32t);
    tb->backend->EmitFlag(tb->ctx, &flag);
    tb->backend->Ret(tb->ctx, &flag);
    tb->backend->EndFunction(tb->ctx, lastIdx);

    uint32_t trueIdx = tb->backend->BeginFunction(tb->ctx, 0, NULL);
    BCValue lt2 = tb->backend->GenTemporary(tb->ctx, i32t);
    BCValue five2 = imm32(5);
    BCValue ten2 = imm32(10);
    tb->backend->Lt3(tb->ctx, &lt2, &five2, &ten2);
    BCValue flag2 = tb->backend->GenTemporary(tb->ctx, i32t);
    tb->backend->EmitFlag(tb->ctx, &flag2);
    tb->backend->Ret(tb->ctx, &flag2);
    tb->backend->EndFunction(tb->ctx, trueIdx);

    tb->backend->Finalize(tb->ctx);
    BCValue args[] = {};
    BCValue out0 = tb->backend->Run(tb->ctx, lastIdx, args, 0, &tb->heap);
    test_record(stats, "EmitFlag after Eq3 reads last comparison (0)",
                out0.vType == BCValueType_Immediate && out0.imm32.imm32 == 0);

    BCValue out1 = tb->backend->Run(tb->ctx, trueIdx, args, 0, &tb->heap);
    test_record(stats, "EmitFlag after Lt3 (1)",
                out1.vType == BCValueType_Immediate && out1.imm32.imm32 == 1);

    TestBackend_Destroy(tb);
}

int main(int argc, const char** argv) {
    printf("=== Lightning Backend Standalone Tests ===\n\n");

    test_stats_t stats = {0};

    test_simple_add(&stats);
    test_simple_sub(&stats);
    test_simple_mul(&stats);
    test_simple_div(&stats);
    test_simple_and(&stats);
    test_simple_lt(&stats);
    test_jmp_forward(&stats);
    test_cndjmp_forward(&stats);
    test_local(&stats);
    test_read_i32(&stats);
    test_store_load64(&stats);
    test_fp_roundtrip(&stats);
    test_call_no_args(&stats);
    test_call_with_args(&stats);
    test_emit_flag(&stats);
    test_debug_elf(&stats);
    
    printf("\n=== Test Summary ===\n");
    printf("Total:  %u\n", stats.test_count);
    printf("Passed: %u\n", stats.pass_count);
    printf("Failed: %u\n", stats.fail_count);
    
    return (stats.fail_count == 0) ? 0 : 1;
}
