/* ======================================================================
 *  metac_coro_fcontext.c
 *
 *  The boost.context ("deboost.context") implementation of the coroutine
 *  layer of MetaC.  See os/metac_coro_fcontext.h for the API and for
 *  the differences to the libaco based implementation.
 *
 *  The context switching itself is the assembly of boost.context; it is
 *  embedded here as a top level asm block (the same trick libaco uses
 *  for its acosw) so that no extra object files have to be built.
 * ==================================================================== */

#include "metac_coro_fcontext.h"

#include <stdlib.h>
#include <string.h>
#include <assert.h>

#if defined(__unix__) || defined(__linux__) || defined(__unix) \
 || (defined(__APPLE__) && defined(__MACH__))
#  include <unistd.h>
#  include <sys/mman.h>
#  define ACO_HAVE_POSIX 1
#endif

/* ----------------------------------------------------------------------
 *  the context switching
 *
 *  This is the assembly of boost.context (3rd_party/deboost.context),
 *  converted into a top level asm block.  The original files are:
 *
 *      asm/jump_<arch>_<abi>_elf_gas.S
 *      asm/make_<arch>_<abi>_elf_gas.S
 *
 *  The layout of a context is the same in all of them:
 *
 *      0x00  the mxcsr register
 *      0x04  the x87 control word
 *      0x08  r12 / d8
 *      ...
 *      0x38  the return address
 *      0x40  the stack pointer
 * -------------------------------------------------------------------- */

#if defined(__APPLE__)
#  define ACO_ASM_TYPE(NAME) ".type " NAME ",_function\n"
#  define ACO_ASM_EXIT "_exit\n"
/* on macOS the symbols have a leading underscore */
#  define ACO_ASM_SYM(NAME) "_" NAME
#else
#  define ACO_ASM_TYPE(NAME) ".type " NAME ",@function\n"
#  define ACO_ASM_EXIT "_exit@PLT\n"
#  define ACO_ASM_SYM(NAME) NAME
#endif

#if defined(__x86_64__) && !defined(_MSC_VER)

__asm__(
".text\n"
".globl " ACO_ASM_SYM("jump_fcontext") "\n"
ACO_ASM_TYPE(ACO_ASM_SYM("jump_fcontext"))
".align 16\n"
ACO_ASM_SYM("jump_fcontext") ":\n"
"    leaq  -0x38(%rsp), %rsp\n"   /* prepare the stack */
"    stmxcsr (%rsp)\n"             /* save the MMX control- and status-word */
"    fnstcw  0x4(%rsp)\n"          /* save the x87 control-word */
"    movq  %r12, 0x8(%rsp)\n"
"    movq  %r13, 0x10(%rsp)\n"
"    movq  %r14, 0x18(%rsp)\n"
"    movq  %r15, 0x20(%rsp)\n"
"    movq  %rbx, 0x28(%rsp)\n"
"    movq  %rbp, 0x30(%rsp)\n"
"    movq  %rsp, %rax\n"            /* the context data of the caller */
"    movq  %rdi, %rsp\n"            /* the context data of the target */
"    movq  0x38(%rsp), %r8\n"       /* the return address of the target */
"    ldmxcsr (%rsp)\n"             /* restore the MMX control- and status-word */
"    fldcw  0x4(%rsp)\n"           /* restore the x87 control-word */
"    movq  0x8(%rsp), %r12\n"
"    movq  0x10(%rsp), %r13\n"
"    movq  0x18(%rsp), %r14\n"
"    movq  0x20(%rsp), %r15\n"
"    movq  0x28(%rsp), %rbx\n"
"    movq  0x30(%rsp), %rbp\n"
"    leaq  0x40(%rsp), %rsp\n"      /* prepare the stack */
"    movq  %rsi, %rdx\n"            /* the data */
"    movq  %rax, %rdi\n"            /* the context of the caller */
"    jmp  *%r8\n"
".size jump_fcontext,.-jump_fcontext\n"
);

__asm__(
".text\n"
".globl " ACO_ASM_SYM("make_fcontext") "\n"
ACO_ASM_TYPE(ACO_ASM_SYM("make_fcontext"))
".align 16\n"
ACO_ASM_SYM("make_fcontext") ":\n"
"    movq  %rdi, %rax\n"            /* the top of the stack */
"    andq  $-16, %rax\n"            /* align it to 16 bytes */
"    leaq  -0x40(%rax), %rax\n"     /* reserve the space for the context data */
"    movq  %rdx, 0x28(%rax)\n"      /* the context function (in RBX) */
"    stmxcsr (%rax)\n"
"    fnstcw  0x4(%rax)\n"
"    leaq  1f(%rip), %rcx\n"
"    movq  %rcx, 0x38(%rax)\n"      /* the return address of the context function */
"    leaq  2f(%rip), %rcx\n"
"    movq  %rcx, 0x30(%rax)\n"      /* the address to jump to if it returns */
"    ret\n"
"1:\n"                              /* the trampoline */
"    push %rbp\n"
"    jmp  *%rbx\n"
"2:\n"                              /* the context function returned - this must not happen */
"    xorq  %rdi, %rdi\n"
"    call  " ACO_ASM_EXIT "\n"
"    hlt\n"
".size make_fcontext,.-make_fcontext\n"
);

#elif defined(__aarch64__) && !defined(_MSC_VER)

__asm__(
".text\n"
".globl jump_fcontext\n"
".type   jump_fcontext, %function\n"
".align  2\n"
"jump_fcontext:\n"
"    sub  sp, sp, #0xb0\n"
"    stp  d8,  d9,  [sp, #0x00]\n"
"    stp  d10, d11, [sp, #0x10]\n"
"    stp  d12, d13, [sp, #0x20]\n"
"    stp  d14, d15, [sp, #0x30]\n"
"    stp  x19, x20, [sp, #0x40]\n"
"    stp  x21, x22, [sp, #0x50]\n"
"    stp  x23, x24, [sp, #0x60]\n"
"    stp  x25, x26, [sp, #0x70]\n"
"    stp  x27, x28, [sp, #0x80]\n"
"    stp  x29, x30, [sp, #0x90]\n"
"    str  x30, [sp, #0xa0]\n"
"    mov  x4, sp\n"                 /* the context data of the caller */
"    mov  sp, x0\n"                 /* the context data of the target */
"    ldp  d8,  d9,  [sp, #0x00]\n"
"    ldp  d10, d11, [sp, #0x10]\n"
"    ldp  d12, d13, [sp, #0x20]\n"
"    ldp  d14, d15, [sp, #0x30]\n"
"    ldp  x19, x20, [sp, #0x40]\n"
"    ldp  x21, x22, [sp, #0x50]\n"
"    ldp  x23, x24, [sp, #0x60]\n"
"    ldp  x25, x26, [sp, #0x70]\n"
"    ldp  x27, x28, [sp, #0x80]\n"
"    ldp  x29, x30, [sp, #0x90]\n"
"    mov x0, x4\n"                 /* X0 == FCTX, X1 == DATA */
"    ldr  x4, [sp, #0xa0]\n"        /* the pc */
"    add  sp, sp, #0xb0\n"
"    ret x4\n"
".size   jump_fcontext,.-jump_fcontext\n"
);

__asm__(
".text\n"
".globl make_fcontext\n"
".type   make_fcontext, %function\n"
".align  2\n"
"make_fcontext:\n"
"    and x0, x0, ~0xF\n"          /* align the stack to 16 bytes */
"    sub  x0, x0, #0xb0\n"        /* reserve the space for the context data */
"    str  x2, [x0, #0xa0]\n"      /* the context function (as the pc) */
"    adr  x1, 1f\n"
"    str  x1, [x0, #0x98]\n"      /* the address to jump to if it returns */
"    ret  x30\n"
"1:\n"
"    mov  x0, #0\n"
"    bl   _exit\n"
".size   make_fcontext,.-make_fcontext\n"
);

#else
#  error "no fcontext assembly for this platform (x86_64 and aarch64 are supported, the GNU assembler is needed)"
#endif

/* ----------------------------------------------------------------------
 *  the stacks
 *
 *  (adapted from 3rd_party/deboost.context/source/stack.c)
 * -------------------------------------------------------------------- */

#define ACO_STACK_DEFAULT_SIZE (128 * 1024)
#define ACO_STACK_MIN_SIZE     (16 * 1024)

static size_t aco_getPageSize(void)
{
#if defined(ACO_HAVE_POSIX)
    long pageSize = sysconf(_SC_PAGESIZE);
    return (pageSize > 0) ? (size_t)pageSize : 4096;
#else
    return 4096;
#endif
}

fcontext_stack_t create_fcontext_stack(size_t size)
{
    fcontext_stack_t result = {0};
    const size_t pageSize = aco_getPageSize();

    if (size == 0)
    {
        size = ACO_STACK_DEFAULT_SIZE;
    }
    if (size < ACO_STACK_MIN_SIZE)
    {
        size = ACO_STACK_MIN_SIZE;
    }

    size_t sizeRounded = (size / pageSize) * pageSize;
    if (sizeRounded < 4 * pageSize)
    {
        sizeRounded = 4 * pageSize;
    }

#if defined(ACO_HAVE_POSIX)
    void* vp = mmap(0, sizeRounded,
                    PROT_READ | PROT_WRITE,
                    MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
    if (vp == MAP_FAILED)
    {
        return result;
    }
    /* the lowest page is a guard page */
    mprotect(vp, pageSize, PROT_NONE);
#else
    void* vp = malloc(sizeRounded);
    if (!vp)
    {
        return result;
    }
#endif

    result.sptr = (char*)vp + sizeRounded;
    result.ssize = sizeRounded;
    return result;
}

void destroy_fcontext_stack(fcontext_stack_t* s)
{
    if (!s || !s->sptr)
    {
        return;
    }
    void* vp = (char*)s->sptr - s->ssize;
#if defined(ACO_HAVE_POSIX)
    munmap(vp, s->ssize);
#else
    free(vp);
#endif
    s->sptr = 0;
    s->ssize = 0;
}

/* ----------------------------------------------------------------------
 *  the coroutines
 * -------------------------------------------------------------------- */

ACO_TLS aco_t* aco_gtls_co_ = 0;

void aco_global_init(void)
{
}

void aco_thread_init(aco_cofuncp_t last_word_co_fp)
{
    (void)last_word_co_fp;
}

aco_share_stack_t* aco_share_stack_new(size_t sz)
{
    aco_share_stack_t* result =
        (aco_share_stack_t*)calloc(1, sizeof(aco_share_stack_t));
    if (!result)
    {
        return 0;
    }
    result->Stack = create_fcontext_stack(sz);
    if (!result->Stack.sptr)
    {
        free(result);
        return 0;
    }
    return result;
}

void aco_share_stack_destroy(aco_share_stack_t* sstk)
{
    if (!sstk)
    {
        return;
    }
    destroy_fcontext_stack(&sstk->Stack);
    free(sstk);
}

/* the entry point of every coroutine: it calls the user function and
 * switches back to the worker thread when it returns */
static void aco_entry(fcontext_transfer_t t)
{
    aco_t* co = (aco_t*)t.data;
    assert(co != 0);

    SET_CO(co);
    co->sched_ctx = t.ctx;
    co->started = 1;

    if (co->fp)
    {
        co->fp();
    }

    /* the entry function returned - the coroutine is done */
    co->is_end = 1;

    /* switch back to the worker thread - this must never return */
    jump_fcontext(co->sched_ctx, (void*)co);

    /* the coroutine must not be resumed after it has ended */
    for(;;)
    {
    }
}

aco_t* aco_create(aco_t* main_co,
                  aco_share_stack_t* share_stack,
                  size_t save_stack_sz,
                  aco_cofuncp_t fp,
                  void* arg)
{
    (void)save_stack_sz;

    aco_t* co = (aco_t*)calloc(1, sizeof(aco_t));
    if (!co)
    {
        return 0;
    }

    co->main_co = main_co;
    co->arg = arg;
    co->fp = fp;

    if (fp)
    {
        if (share_stack)
        {
            /* every coroutine gets its own stack - boost.context cannot
             * share a stack between coroutines */
            co->stack = share_stack->Stack;
            co->owns_stack = 0;
        }
        else
        {
            co->stack = create_fcontext_stack(0);
            co->owns_stack = 1;
        }

        if (!co->stack.sptr)
        {
            free(co);
            return 0;
        }

        co->ctx = make_fcontext(co->stack.sptr, co->stack.ssize, aco_entry);
    }

    return co;
}

void aco_destroy(aco_t* co)
{
    if (!co)
    {
        return;
    }
    if (co->owns_stack)
    {
        destroy_fcontext_stack(&co->stack);
    }
    free(co);
}

void aco_resume(aco_t* resume_co)
{
    assert(resume_co != 0);
    if (!resume_co->fp)
    {
        /* the "main co" of a thread has no entry function */
        return;
    }
    assert(!resume_co->is_end);

    aco_t* prev = GET_CO();
    SET_CO(resume_co);

    /* the data of the transfer is the coroutine that is resumed - it
     * needs it to know who it is */
    fcontext_transfer_t t = jump_fcontext(resume_co->ctx, (void*)resume_co);

    /* whoever yielded back to us owns the context that was returned */
    aco_t* owner = (aco_t*)t.data;
    if (owner)
    {
        owner->ctx = t.ctx;
    }
    else
    {
        resume_co->ctx = t.ctx;
    }

    SET_CO(prev);
}
