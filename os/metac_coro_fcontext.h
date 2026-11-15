#ifndef _METAC_CORO_FCONTEXT_H_
#define _METAC_CORO_FCONTEXT_H_

/* ======================================================================
 *  A libaco compatible coroutine API on top of boost.context.
 *
 *  This is the second implementation of the coroutine layer of MetaC.
 *  The first one (and the default) is libaco (3rd_party/libaco); this
 *  one is based on the "deboost.context" copy of boost.context in
 *  3rd_party/deboost.context.
 *
 *  The API is intentionally the same as the libaco one, so that
 *  os/metac_task.c and everything above it does not have to care which
 *  backend is used.  The backend is selected with
 *
 *      -DMETAC_CORO_FCONTEXT
 *
 *  and the default is still libaco.
 *
 *  Differences to libaco
 *  ----------------------
 *  * libaco can share one physical stack between several coroutines
 *    (it copies the stack contents around when switching).  boost.context
 *    cannot do that, so every coroutine gets its own stack here.
 *  * libaco always sends the "yield" of a coroutine to the "main co" of
 *    the thread.  boost.context has no such thing - a context is tied to
 *    a stack frame - so a coroutine returns to whoever resumed it.
 *    For the task system of MetaC this is the same thing (the worker
 *    thread is always the one that resumes a task), but the semantics
 *    of aco_yield2() differ a little.  See the comment at aco_yield2.
 * ==================================================================== */

#include <stdint.h>
#include <stddef.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

/* The task system of MetaC updates the active task of the worker when
 * the control is handed over to another coroutine.  The backend does
 * not know about the task system, so the update is done through this
 * macro (see os/metac_coro.h). */
#ifndef METAC_CORO_UPDATE_ACTIVE_TASK
#  define METAC_CORO_UPDATE_ACTIVE_TASK(CO) do { } while(0)
#endif

#ifdef __cplusplus
extern "C" {
#endif

/* ----------------------------------------------------------------------
 *  the boost.context interface
 * -------------------------------------------------------------------- */

typedef void* fcontext_t;

typedef struct
{
    fcontext_t ctx;
    void* data;
} fcontext_transfer_t;

typedef struct
{
    void* sptr;
    size_t ssize;
} fcontext_stack_t;

/**
 * Callback definition for context (coroutine)
 */
typedef void (*pfn_fcontext)(fcontext_transfer_t);

/**
 * Switches to another context
 */
fcontext_transfer_t jump_fcontext(fcontext_t const to, void* vp);

/**
 * Makes a new context
 */
fcontext_t make_fcontext(void* sp, size_t size, pfn_fcontext corofn);

/* the stack allocation (see os/metac_coro_fcontext.c) */
fcontext_stack_t create_fcontext_stack(size_t size);
void destroy_fcontext_stack(fcontext_stack_t* s);

/* ----------------------------------------------------------------------
 *  the libaco compatible part
 * -------------------------------------------------------------------- */

typedef void (*aco_cofuncp_t)(void);

typedef struct aco_share_stack_t
{
    fcontext_stack_t Stack;
    char InUse;
} aco_share_stack_t;

struct aco_s;
typedef struct aco_s aco_t;

struct aco_s
{
    /* where this coroutine continues when it is resumed */
    fcontext_t ctx;

    /* where this coroutine returns to when it yields */
    fcontext_t sched_ctx;

    /* the user argument (aco_get_arg) */
    void* arg;

    /* the "main" coroutine of the thread */
    aco_t* main_co;

    /* the coroutine has finished */
    char is_end;

    /* the coroutine has been entered at least once */
    char started;

    /* the entry function of the coroutine */
    aco_cofuncp_t fp;

    /* the stack of this coroutine */
    fcontext_stack_t stack;

    /* the stack belongs to this coroutine (and has to be freed with it) */
    char owns_stack;
};

aco_t* aco_create(aco_t* main_co,
                  aco_share_stack_t* share_stack,
                  size_t save_stack_sz,
                  aco_cofuncp_t fp,
                  void* arg);

void aco_destroy(aco_t* co);

aco_share_stack_t* aco_share_stack_new(size_t sz);
void aco_share_stack_destroy(aco_share_stack_t* sstk);

void aco_resume(aco_t* resume_co);

void aco_thread_init(aco_cofuncp_t last_word_co_fp);
void aco_global_init(void);

#if defined(__GNUC__) || defined(__clang__)
#  define ACO_TLS __thread
#else
#  define ACO_TLS
#endif

/* The task system of MetaC uses this to decide how it stores the thread
 * local state (see os/metac_task.c).  libaco defines it as well. */
#ifndef HAS_TLS
#  ifndef __TINYC__
#    define HAS_TLS
#  endif
#endif

extern ACO_TLS aco_t* aco_gtls_co_;

#define GET_CO() (aco_gtls_co_)
#define SET_CO(CO) (aco_gtls_co_ = (CO))

#define aco_get_co() (GET_CO())
#define aco_get_arg() (GET_CO()->arg)

#define aco_is_main_co(co) (((co)->main_co) == NULL)

/* the yield of a coroutine: it returns to whoever resumed it.
 *
 * The data of the transfer is the coroutine that yields - the resumer
 * uses it to find out whose context it got back. */
#define aco_yield1(yield_co) do { \
    aco_t* _co = (yield_co); \
    fcontext_transfer_t _t = jump_fcontext(_co->sched_ctx, (void*)_co); \
    _co->sched_ctx = _t.ctx; \
    /* we have been resumed - the resumer told us who we are */ \
    SET_CO((aco_t*)_t.data); \
} while(0)

#define aco_yield() do { \
    aco_yield1(GET_CO()); \
} while(0)

/* Hand the control over to another coroutine.
 *
 * libaco switches the registers directly, so the other coroutine keeps
 * running until it yields - and its yield goes to the "main co" of the
 * thread, not back to us.
 *
 * Here the other coroutine is resumed and *it* yields back to us (the
 * context of a coroutine is the stack frame it was suspended in, so
 * there is no way to "take over" the stack of another coroutine).  We
 * then give the control back to the worker thread, which is what
 * libaco does as well - the difference is that we are woken up by the
 * first yield of the other coroutine instead of staying suspended
 * until the worker thread resumes us again.
 */
#define aco_yield2(target_co) do { \
    aco_t* _me = GET_CO(); \
    aco_t* _target = (target_co); \
    METAC_CORO_UPDATE_ACTIVE_TASK(_target); \
    fcontext_transfer_t _t = jump_fcontext(_target->ctx, (void*)_target); \
    /* the other coroutine yielded back to us */ \
    SET_CO(_me); \
    /* it continues where it left off the next time it is resumed */ \
    _target->ctx = _t.ctx; \
    /* from now on it yields to the worker thread again */ \
    _target->sched_ctx = _me->sched_ctx; \
    /* give the control back to the worker thread */ \
    fcontext_transfer_t _t2 = jump_fcontext(_me->sched_ctx, (void*)_me); \
    SET_CO((aco_t*)_t2.data); \
} while(0)

#define aco_exit1(co) do { \
    (co)->is_end = 1; \
    aco_yield1(co); \
} while(0)

#define aco_exit() do { \
    aco_exit1(GET_CO()); \
} while(0)

#ifdef __cplusplus
}
#endif

#endif // _METAC_CORO_FCONTEXT_H_
