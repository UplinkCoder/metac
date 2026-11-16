/* ======================================================================
 *  coro_test.c
 *
 *  The tests of the coroutine layer of MetaC.
 *
 *  The coroutine layer has two implementations - libaco and
 *  boost.context - and both of them are supposed to behave the same.
 *  The test is written against the common API, so it can be run against
 *  both of them:
 *
 *      # the default (libaco)
 *      cc -o coro_test tests/coro_test.c os/metac_coro.c ... -I.
 *
 *      # boost.context
 *      cc -DMETAC_CORO_FCONTEXT -o coro_test tests/coro_test.c ...
 *
 *  The tests are:
 *
 *      1. a coroutine that yields a few times
 *      2. the argument of a coroutine
 *      3. the end of a coroutine (aco_exit)
 *      4. two coroutines that are interleaved
 *      5. a coroutine that hands the control over to another one
 *      6. every coroutine has its own stack
 *
 *  How to build it:
 *
 *      # the default (libaco)
 *      cc -o coro_test tests/coro_test.c
 *
 *      # boost.context
 *      cc -DMETAC_CORO_FCONTEXT -o coro_test tests/coro_test.c
 * ==================================================================== */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

/* the libaco implementation needs STATIC_ASSERT (and a few other things)
 * from the compatibility header */
#include "../os/compat.h"

/* ----------------------------------------------------------------------
 *  A minimal stub of the task system
 *
 *  The libaco backend updates the active task of the worker when the
 *  control is handed over to another coroutine, so the test has to
 *  provide the task system for it.
 * -------------------------------------------------------------------- */

typedef struct task_t
{
    int dummy;
} task_t;

typedef struct worker_context_t
{
    task_t* ActiveTask;
} worker_context_t;

static worker_context_t gTestWorker = {0};

static worker_context_t* CurrentWorker(void)
{
    return &gTestWorker;
}

#if defined(METAC_CORO_FCONTEXT)
/* the boost.context backend does the same through this macro */
#  define METAC_CORO_UPDATE_ACTIVE_TASK(CO) \
    (CurrentWorker()->ActiveTask = ((task_t*)(CO)->arg))
#endif

/* the coroutine backend */
#if defined(METAC_CORO_FCONTEXT)
#  include "../os/metac_coro_fcontext.h"
/* the implementation of the backend - it is included like
 * os/metac_coro.c does it */
#  include "../os/metac_coro_fcontext.c"
#else
#  include "../3rd_party/libaco/aco.h"
/* aco_global_init is not declared in the header of libaco */
extern void aco_global_init(void);

/* the implementation of libaco - it is included like os/metac_coro.c
 * does it (it needs the assert header of the includer) */
#  include "../3rd_party/libaco/aco.c"
#  ifndef ACOSW_EXTERNAL_ASM
#    include "../3rd_party/libaco/acosw.c"
#  endif
#endif

/* the "main co" of the thread - every coroutine is created with it */
static aco_t* gMainCo = 0;

/* Every coroutine gets a stack of its own.  libaco calls it a "share
 * stack" (it can be shared between coroutines), boost.context does not
 * support that - so every coroutine gets its own. */
static aco_t* CreateCoroutine(aco_cofuncp_t fp, void* arg,
                              aco_share_stack_t** stackOut)
{
    aco_share_stack_t* stack = aco_share_stack_new(64 * 1024);
    if (!stack)
    {
        printf("could not create a stack for the coroutine\n");
        exit(1);
    }
    *stackOut = stack;
    return aco_create(gMainCo, stack, 0, fp, arg);
}

static void DestroyCoroutine(aco_t* co, aco_share_stack_t* stack)
{
    aco_destroy(co);
    aco_share_stack_destroy(stack);
}

static uint32_t gTestsRun = 0;
static uint32_t gTestsFailed = 0;

#define TEST_ASSERT(EXPR) do { \
    if (!(EXPR)) { \
        printf("  FAILED: %s (%s:%u)\n", #EXPR, __FILE__, __LINE__); \
        gTestsFailed++; \
    } \
} while(0)

#define TEST_BEGIN(NAME) do { \
    printf("[test] %s\n", NAME); \
    gTestsRun++; \
} while(0)

/* ----------------------------------------------------------------------
 *  1. a coroutine that yields a few times
 * ---------------------------------------------------------------------- */

static int gYieldCounter = 0;
static int gObservedYields = 0;

static void YieldingCoroutine(void)
{
    for (int i = 0; i < 3; i++)
    {
        gYieldCounter++;
        aco_yield();
        /* after the coroutine has been resumed we continue here */
        gObservedYields++;
    }
    aco_exit();
}

static void TestYield(void)
{
    TEST_BEGIN("a coroutine can yield and be resumed");

    gYieldCounter = 0;
    gObservedYields = 0;

    aco_share_stack_t* stack = 0;
    aco_t* co = CreateCoroutine(YieldingCoroutine, 0, &stack);
    TEST_ASSERT(co != 0);

    /* the coroutine runs until it yields for the first time */
    aco_resume(co);
    TEST_ASSERT(gYieldCounter == 1);
    TEST_ASSERT(gObservedYields == 0);
    TEST_ASSERT(!co->is_end);

    aco_resume(co);
    TEST_ASSERT(gYieldCounter == 2);
    TEST_ASSERT(gObservedYields == 1);

    aco_resume(co);
    TEST_ASSERT(gYieldCounter == 3);
    TEST_ASSERT(gObservedYields == 2);

    /* the coroutine exits now */
    aco_resume(co);
    TEST_ASSERT(gYieldCounter == 3);
    TEST_ASSERT(gObservedYields == 3);
    TEST_ASSERT(co->is_end);

    DestroyCoroutine(co, stack);
}

/* ----------------------------------------------------------------------
 *  2. the argument of a coroutine
 * ---------------------------------------------------------------------- */

typedef struct coro_arg_t
{
    int value;
    int seen;
} coro_arg_t;

static void ArgCoroutine(void)
{
    coro_arg_t* arg = (coro_arg_t*)aco_get_arg();
    arg->seen = arg->value * 2;
    aco_yield();
}

static void TestArgument(void)
{
    TEST_BEGIN("the argument of a coroutine can be read");

    coro_arg_t arg = {21, 0};

    aco_share_stack_t* stack = 0;
    aco_t* co = CreateCoroutine(ArgCoroutine, &arg, &stack);
    TEST_ASSERT(co != 0);

    aco_resume(co);
    TEST_ASSERT(arg.seen == 42);

    DestroyCoroutine(co, stack);
}

/* ----------------------------------------------------------------------
 *  3. the end of a coroutine
 * ---------------------------------------------------------------------- */

static int gExitCounter = 0;

static void ExitingCoroutine(void)
{
    gExitCounter++;
    aco_exit();
    /* this must never be reached */
    gExitCounter += 1000;
}

static void TestExit(void)
{
    TEST_BEGIN("a coroutine can exit");

    gExitCounter = 0;

    aco_share_stack_t* stack = 0;
    aco_t* co = CreateCoroutine(ExitingCoroutine, 0, &stack);
    TEST_ASSERT(co != 0);
    TEST_ASSERT(!co->is_end);

    aco_resume(co);
    TEST_ASSERT(gExitCounter == 1);
    TEST_ASSERT(co->is_end);

    DestroyCoroutine(co, stack);
}

/* ----------------------------------------------------------------------
 *  4. two coroutines that are interleaved
 * ---------------------------------------------------------------------- */

static char gInterleaved[64];
static uint32_t gInterleavedLen = 0;

static void AppendChar(char c)
{
    if (gInterleavedLen + 1 < sizeof(gInterleaved))
    {
        gInterleaved[gInterleavedLen++] = c;
    }
}

static void InterleavedCoroutineA(void)
{
    for (int i = 0; i < 3; i++)
    {
        AppendChar('a');
        aco_yield();
    }
    aco_exit();
}

static void InterleavedCoroutineB(void)
{
    for (int i = 0; i < 3; i++)
    {
        AppendChar('b');
        aco_yield();
    }
    aco_exit();
}

static void TestInterleaved(void)
{
    TEST_BEGIN("two coroutines can be interleaved");

    gInterleavedLen = 0;
    gInterleaved[0] = 0;

    aco_share_stack_t* stackA = 0;
    aco_share_stack_t* stackB = 0;
    aco_t* a = CreateCoroutine(InterleavedCoroutineA, 0, &stackA);
    aco_t* b = CreateCoroutine(InterleavedCoroutineB, 0, &stackB);
    TEST_ASSERT(a != 0);
    TEST_ASSERT(b != 0);

    for (int i = 0; i < 4; i++)
    {
        aco_resume(a);
        aco_resume(b);
    }

    gInterleaved[gInterleavedLen] = 0;
    /* every coroutine appends three characters and then exits */
    TEST_ASSERT(strcmp(gInterleaved, "ababab") == 0);
    TEST_ASSERT(a->is_end);
    TEST_ASSERT(b->is_end);

    DestroyCoroutine(a, stackA);
    DestroyCoroutine(b, stackB);
}

/* ----------------------------------------------------------------------
 *  5. a coroutine that hands the control over to another one
 * ---------------------------------------------------------------------- */

static int gHandoffCounter = 0;
static int gHandoffTarget = 0;

static void HandoffTargetCoroutine(void)
{
    for (int i = 0; i < 2; i++)
    {
        gHandoffCounter++;
        aco_yield();
    }
    aco_exit();
}

static void HandoffCoroutine(void)
{
    aco_yield2((aco_t*)aco_get_arg());
    gHandoffCounter += 100;
}

static void TestHandoff(void)
{
    TEST_BEGIN("a coroutine can hand the control over to another one");

    gHandoffCounter = 0;

    aco_share_stack_t* targetStack = 0;
    aco_share_stack_t* stack = 0;
    aco_t* target = CreateCoroutine(HandoffTargetCoroutine, 0, &targetStack);
    aco_t* co = CreateCoroutine(HandoffCoroutine, target, &stack);
    TEST_ASSERT(target != 0);
    TEST_ASSERT(co != 0);

    gHandoffTarget = 0;

    /* the coroutine hands the control over to the target, which runs
     * until it yields */
    aco_resume(co);
    TEST_ASSERT(gHandoffCounter == 1);

    /* the target yields back to the coroutine that handed over the
     * control, which gives it back to us */
    aco_resume(target);
    TEST_ASSERT(gHandoffCounter == 2);

    DestroyCoroutine(target, targetStack);
    DestroyCoroutine(co, stack);
}

/* ----------------------------------------------------------------------
 *  6. every coroutine has its own stack
 * ---------------------------------------------------------------------- */

static int gStackTestValue = 0;

static int Recurse(int depth)
{
    /* a few hundred recursion levels need some stack */
    char buffer[256];
    memset(buffer, depth & 0xff, sizeof(buffer));
    if (depth <= 0)
    {
        return buffer[0];
    }
    return Recurse(depth - 1) + buffer[0];
}

static void StackCoroutine(void)
{
    /* the stack of a coroutine is its own - this recursion must not
     * touch the stack of the thread */
    gStackTestValue = Recurse(100);
    aco_yield();
}

static void TestOwnStack(void)
{
    TEST_BEGIN("every coroutine has its own stack");

    gStackTestValue = 0;

    aco_share_stack_t* stack = 0;
    aco_t* co = CreateCoroutine(StackCoroutine, 0, &stack);
    TEST_ASSERT(co != 0);

    aco_resume(co);
    TEST_ASSERT(gStackTestValue != 0);

    DestroyCoroutine(co, stack);
}

/* ----------------------------------------------------------------------
 *  the test run
 * ---------------------------------------------------------------------- */

int main(int argc, char* argv[])
{
#if defined(METAC_CORO_FCONTEXT)
    printf("coroutine backend: boost.context (deboost.context)\n");
#else
    printf("coroutine backend: libaco\n");
#endif

    aco_global_init();
    aco_thread_init(0);

    /* the "main co" of this thread */
    gMainCo = aco_create(0, 0, 0, 0, 0);
    if (!gMainCo)
    {
        printf("could not create the main coroutine\n");
        return 1;
    }

    TestYield();
    TestArgument();
    TestExit();
    TestInterleaved();
    TestHandoff();
    TestOwnStack();

    printf("\n%u test(s) run, %u assertion(s) failed\n",
           gTestsRun, gTestsFailed);

    return (gTestsFailed != 0);
}
