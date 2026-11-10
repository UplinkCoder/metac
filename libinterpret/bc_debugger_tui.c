/**
 * bc_debugger_tui.c
 *
 * A gdb-tui like ncurses debugging interface for the bytecode interpreter
 * implemented in bc_interpreter_backend.c.
 *
 * This file is #included from bc_interpreter_backend.c (the same "big
 * translation unit" pattern the rest of the project uses) so that it has
 * direct access to the (file local) BCInterpreter state.
 *
 * Everything in here is compiled away unless BC_DEBUG_TUI is defined.
 *
 * Build with:  -DBC_DEBUG_TUI   and link against:  -lncurses
 *
 * Runtime configuration (environment variables):
 *   BC_DEBUG_UI = paused | run | off     (default: paused)
 *   BC_DEBUG_BREAK = 0x10,40,0x100       breakpoints set before the first step
 *
 * Commands (once paused):
 *   s | step        step one instruction
 *   n | next        step to the next source line
 *   c | continue    resume execution
 *   b <ip>          set a breakpoint
 *   d <ip>          delete a breakpoint
 *   bl              list breakpoints
 *   r | regs        reset the register view to the current frame
 *   bt              show the backtrace
 *   x[/n] <addr>    examine n bytes of memory at a hex address
 *   disas <ip>      move the disassembly view
 *   q | quit        terminate execution (bailout)
 *   ? | help        show help
 * Keys: PgUp/PgDn/Up/Down scroll the disassembly, +/- scroll the registers
 */

#ifndef _BC_DEBUGGER_TUI_C_
#define _BC_DEBUGGER_TUI_C_ 1

#ifdef BC_DEBUG_TUI

/* metac_preproc.h defines `define_key` as an object-like macro while
 * ncurses.h declares a function by that name; hide the macro while the
 * ncurses declarations are processed.  The metac key macros live above
 * metac_preproc.h's include guard, so re-including that header only
 * re-runs the #define block and restores them. */
#ifdef define_key
#  undef define_key
#  define BCDBG_RESTORE_METAC_KEYS 1
#endif

#include <ncurses.h>

#ifdef BCDBG_RESTORE_METAC_KEYS
#  undef BCDBG_RESTORE_METAC_KEYS
#  include "../parser/metac_preproc.h"
#endif

#include <setjmp.h>
#include <signal.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#define BCDBG_MAX_BREAKPOINTS 64
#define BCDBG_MAX_INDEX       16384
#define BCDBG_STATUS_LEN      256
#define BCDBG_INPUT_LEN       256
#define BCDBG_OUT_LINES       24
#define BCDBG_OUT_WIDTH       160
#define BCDBG_POLL_INTERVAL   4096

#define BCDBG_PAIR_TITLE   1
#define BCDBG_PAIR_STOP    2
#define BCDBG_PAIR_RUN     3
#define BCDBG_PAIR_PC      4
#define BCDBG_PAIR_STATUS  5

#define BCDBG_ACT_NONE    0
#define BCDBG_ACT_RESUME  1
#define BCDBG_ACT_QUIT    2

typedef enum BCDebuggerStepMode
{
    BCDBG_STEP_NONE = 0,
    BCDBG_STEP_ONE,
    BCDBG_STEP_LINE,
} BCDebuggerStepMode;

typedef struct BCDebugger
{
    bool configured;
    bool active;
    bool running;
    bool quit;
    bool uiUp;
    bool followPc;
    bool atExit;
    bool autoRun;
    bool colors;
    BCDebuggerStepMode stepMode;

    uint32_t stepLine;
    uint32_t pollCounter;
    const char* exitReason;

    uint32_t breakpoints[BCDBG_MAX_BREAKPOINTS];
    uint32_t nBreakpoints;

    uint32_t codeLen;      ///< length of the code in words
    uint32_t idxCount;     ///< number of instructions in the index

    int viewIdx;           ///< first instruction shown in the disassembly pane
    int pcIdx;             ///< index of the current instruction
    int regView;           ///< first register shown in the register pane

    BCInterpreter* state;
    BCGen* gen;
    uint32_t* code;
    BCHeap* heap;
    int cond;

    char status[BCDBG_STATUS_LEN];
    char lastCmd[BCDBG_INPUT_LEN];
    char input[BCDBG_INPUT_LEN];
    int inputLen;

    char out[BCDBG_OUT_LINES][BCDBG_OUT_WIDTH];
    int outCount;
} BCDebugger;

static uint32_t bcDbgIndex[BCDBG_MAX_INDEX];
static BCDebugger bcDebugger;
static sigjmp_buf bcDbgSegvJmp;
static bool bcDbgCursesAlive = false;

/* ---------------------------------------------------------------------- */
/* small helpers                                                          */
/* ---------------------------------------------------------------------- */

static void BCDebugger_SetStatus(BCDebugger* dbg, const char* fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    vsnprintf(dbg->status, sizeof(dbg->status), fmt, ap);
    va_end(ap);
}

static void BCDebugger_ClearOutput(BCDebugger* dbg)
{
    dbg->outCount = 0;
    memset(dbg->out, 0, sizeof(dbg->out));
}

static void BCDebugger_OutLine(BCDebugger* dbg, const char* fmt, ...)
{
    va_list ap;
    char* dst;
    if (dbg->outCount >= BCDBG_OUT_LINES)
        return;
    dst = dbg->out[dbg->outCount++];
    va_start(ap, fmt);
    vsnprintf(dst, BCDBG_OUT_WIDTH, fmt, ap);
    va_end(ap);
}

/* ---------------------------------------------------------------------- */
/* configuration                                                          */
/* ---------------------------------------------------------------------- */

static void BCDebugger_AddBreakpoint(BCDebugger* dbg, uint32_t ip)
{
    uint32_t i;
    for (i = 0; i < dbg->nBreakpoints; i++)
    {
        if (dbg->breakpoints[i] == ip)
            return;
    }
    if (dbg->nBreakpoints >= BCDBG_MAX_BREAKPOINTS)
    {
        BCDebugger_SetStatus(dbg, "breakpoint table full");
        return;
    }
    dbg->breakpoints[dbg->nBreakpoints++] = ip;
}

static int BCDebugger_HasBreakpoint(BCDebugger* dbg, uint32_t ip)
{
    uint32_t i;
    for (i = 0; i < dbg->nBreakpoints; i++)
    {
        if (dbg->breakpoints[i] == ip)
            return 1;
    }
    return 0;
}

static void BCDebugger_Configure(BCDebugger* dbg)
{
    const char* v;
    const char* term;

    dbg->configured = true;
    dbg->active = false;
    dbg->running = false;
    dbg->stepMode = BCDBG_STEP_NONE;
    dbg->followPc = true;
    dbg->idxCount = 0;
    dbg->viewIdx = 0;
    dbg->regView = 0;
    BCDebugger_SetStatus(dbg, "type `help` for a list of commands");

    /* without a real terminal there is nothing to draw on */
    if (!isatty(0) || !isatty(1))
        return;
    term = getenv("TERM");
    if (!term || !term[0] || strcmp(term, "dumb") == 0)
        return;

    dbg->active = true;

    v = getenv("BC_DEBUG_UI");
    if (v && v[0])
    {
        if (strcmp(v, "off") == 0 || strcmp(v, "0") == 0)
        {
            dbg->active = false;
            return;
        }
        if (strcmp(v, "run") == 0 || strcmp(v, "continue") == 0)
        {
            dbg->running = true;
            dbg->autoRun = true;
        }
    }

    v = getenv("BC_DEBUG_BREAK");
    if (v && v[0])
    {
        while (*v)
        {
            char* endp = 0;
            unsigned long ip = strtoul(v, &endp, 0);
            if (!endp || endp == v)
                break;
            BCDebugger_AddBreakpoint(dbg, (uint32_t)ip);
            v = endp;
            while (*v == ' ' || *v == ',')
                v++;
        }
    }
}

/* ---------------------------------------------------------------------- */
/* instruction index / disassembly                                        */
/* ---------------------------------------------------------------------- */

/// number of words the instruction at `ip` occupies (including payloads)
static uint32_t BCDebugger_InstWords(uint32_t lw, uint32_t hi)
{
    if (lw == 0)
        return 2;
    switch (lw & INSTMASK)
    {
        case LongInst_Comment:
        case LongInst_File:
            return 2 + (((hi + 7u) & ~7u) / 4u);
        case LongInst_MapExternal:
            return 4;
        default:
            return 2;
    }
}

/// (re)build the index of instruction offsets; does nothing if it is valid
static void BCDebugger_BuildIndex(BCDebugger* dbg)
{
    uint32_t ip = 0;
    uint32_t n = 0;

    if (dbg->idxCount != 0)
        return;
    if (!dbg->code || dbg->codeLen == 0)
        return;

    while (ip + 1 < dbg->codeLen && n < BCDBG_MAX_INDEX)
    {
        uint32_t words;
        /* a fully empty slot is header padding, not an instruction */
        if (dbg->code[ip] == 0 && dbg->code[ip + 1] == 0)
        {
            ip += 2;
            continue;
        }
        words = BCDebugger_InstWords(dbg->code[ip], dbg->code[ip + 1]);
        if (words < 2)
            words = 2;
        if (ip + words > dbg->codeLen)
            words = dbg->codeLen - ip;
        bcDbgIndex[n++] = ip;
        ip += words;
    }

    if (n == 0)
        n = 1; /* keep the "already built" marker, the pane shows <no code> */
    dbg->idxCount = n;
}

/// index of the instruction starting at `ip` or -1
static int BCDebugger_IndexOfIp(BCDebugger* dbg, uint32_t ip)
{
    uint32_t lo = 0, hi = dbg->idxCount;
    while (lo < hi)
    {
        uint32_t mid = lo + (hi - lo) / 2;
        if (bcDbgIndex[mid] == ip)
            return (int)mid;
        if (bcDbgIndex[mid] < ip)
            lo = mid + 1;
        else
            hi = mid;
    }
    return -1;
}

static const char* const bcDbgRegOps[] = {
    "Add", "Sub", "Div", "Mul", "Mod",
    "Eq", "Neq", "Lt", "Le", "Gt", "Ge",
    "Ult", "Ule", "Ugt", "Uge",
    "Udiv", "Umod", "And", "And32", "Or", "Xor", "Xor32", "Lsh", "Rsh", "Set"
};

static const char* const bcDbgImmOps[] = {
    "ImmAdd", "ImmSub", "ImmDiv", "ImmMul", "ImmMod",
    "ImmEq", "ImmNeq", "ImmLt", "ImmLe", "ImmGt", "ImmGe",
    "ImmUlt", "ImmUle", "ImmUgt", "ImmUge",
    "ImmUdiv", "ImmUmod", "ImmAnd", "ImmAnd32", "ImmOr",
    "ImmXor", "ImmXor32", "ImmLsh", "ImmRsh"
};

/* 1 => print the immediate as a signed value, 0 => print it unsigned */
static const uint8_t bcDbgImmSigned[] = {
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
};

static const char* BCDebugger_FloatName(uint32_t inst)
{
    switch (inst)
    {
        case LongInst_FAdd32: return "FAdd32";
        case LongInst_FSub32: return "FSub32";
        case LongInst_FDiv32: return "FDiv32";
        case LongInst_FMul32: return "FMul32";
#ifdef WITH_FMOD
        case LongInst_FMod32: return "FMod32";
#endif
        case LongInst_FEq32:  return "FEq32";
        case LongInst_FNeq32: return "FNeq32";
        case LongInst_FLt32:  return "FLt32";
        case LongInst_FLe32:  return "FLe32";
        case LongInst_FGt32:  return "FGt32";
        case LongInst_FGe32:  return "FGe32";
        case LongInst_FAdd64: return "FAdd64";
        case LongInst_FSub64: return "FSub64";
        case LongInst_FDiv64: return "FDiv64";
        case LongInst_FMul64: return "FMul64";
#ifdef WITH_FMOD
        case LongInst_FMod64: return "FMod64";
#endif
        case LongInst_FEq64:  return "FEq64";
        case LongInst_FNeq64: return "FNeq64";
        case LongInst_FLt64:  return "FLt64";
        case LongInst_FLe64:  return "FLe64";
        case LongInst_FGt64:  return "FGt64";
        case LongInst_FGe64:  return "FGe64";
        default:              return "FFop";
    }
}

static void BCDebugger_FormatInst(uint32_t* code, uint32_t codeLen, uint32_t ip,
                                  char* out, size_t outSize)
{
    uint32_t lw, hi, inst, opRef, lhs, rhs;
    int32_t imm;
    uint8_t opSpecial;

    out[0] = 0;
    if (!code || (ip + 1) >= codeLen)
    {
        snprintf(out, outSize, "<out of range>");
        return;
    }

    lw = code[ip];
    hi = code[ip + 1];
    imm = (int32_t)hi;
    inst = lw & INSTMASK;
    opSpecial = (uint8_t)((lw >> 8) & 0xFF);
    opRef = ((lw >> 16) & 0xFFFFu) / 4;
    lhs = (hi & 0xFFFFu) / 4;
    rhs = (hi >> 16) / 4;

    if (!lw)
    {
        snprintf(out, outSize, "NOP");
        return;
    }

    if (inst >= LongInst_Add && inst <= LongInst_Set)
    {
        snprintf(out, outSize, "%-10s R[%u], R[%u]",
                 bcDbgRegOps[inst - LongInst_Add], lhs, rhs);
        return;
    }
    if (inst >= LongInst_ImmAdd && inst <= LongInst_ImmRsh)
    {
        unsigned idx = inst - LongInst_ImmAdd;
        if (bcDbgImmSigned[idx])
            snprintf(out, outSize, "%-10s R[%u], %d",
                     bcDbgImmOps[idx], opRef, imm);
        else
            snprintf(out, outSize, "%-10s R[%u], %u",
                     bcDbgImmOps[idx], opRef, hi);
        return;
    }

    switch (inst)
    {
        case LongInst_PrintValue:
            snprintf(out, outSize, "%-10s R[%u]%s",
                     "Print", opRef, opSpecial ? " (string)" : "");
            break;
        case LongInst_RelJmp:
            snprintf(out, outSize, "%-10s -> %u",
                     "RelJmp", (uint32_t)(ip + (int16_t)(lw >> 16)));
            break;
        case LongInst_Ret32:
        case LongInst_RetS32:
            if (hi)
                snprintf(out, outSize, "%-10s %d", "Ret",
                         (hi == (uint32_t)INT32_MIN ? 0 : imm));
            else
                snprintf(out, outSize, "%-10s R[%u]", "Ret", opRef);
            break;
        case LongInst_Ret64:
        case LongInst_RetS64:
            snprintf(out, outSize, "%-10s R[%u]", "Ret", opRef);
            break;
        case LongInst_Not:
            snprintf(out, outSize, "%-10s R[%u] = ~R[%u]", "Not", opRef, opRef);
            break;
        case LongInst_Flg:
            snprintf(out, outSize, "%-10s R[%u] = cond", "Flg", opRef);
            break;
        case LongInst_Jmp:
            snprintf(out, outSize, "%-10s -> %u", "Jmp", hi);
            break;
        case LongInst_JmpFalse:
            snprintf(out, outSize, "%-10s !cond -> %u", "JmpFalse", hi);
            break;
        case LongInst_JmpTrue:
            snprintf(out, outSize, "%-10s cond -> %u", "JmpTrue", hi);
            break;
        case LongInst_JmpZ:
            snprintf(out, outSize, "%-10s R[%u] == 0 -> %u", "JmpZ", opRef, hi);
            break;
        case LongInst_JmpNZ:
            snprintf(out, outSize, "%-10s R[%u] != 0 -> %u", "JmpNZ", opRef, hi);
            break;
        case LongInst_PushCatch:
            snprintf(out, outSize, "PushCatch");
            break;
        case LongInst_PopCatch:
            snprintf(out, outSize, "PopCatch");
            break;
        case LongInst_Throw:
            snprintf(out, outSize, "%-10s R[%u]", "Throw", opRef);
            break;
        case LongInst_Memcmp:
            snprintf(out, outSize, "%-10s cond = memcmp(R[%u], R[%u], R[%u])",
                     "Memcmp", lhs, rhs, opRef);
            break;
        case LongInst_Assert:
            snprintf(out, outSize, "%-10s R[%u] != 0 else error %d",
                     "Assert", opRef, imm);
            break;
        case LongInst_SetHighImm32:
            snprintf(out, outSize, "%-10s R[%u] |= 0x%xull << 32",
                     "SetHighImm32", opRef, hi);
            break;
        case LongInst_SetImm32:
            snprintf(out, outSize, "%-10s R[%u] = %d", "SetImm32", opRef, imm);
            break;
        case LongInst_SetImm8:
            snprintf(out, outSize, "%-10s R[%u] = %u", "SetImm8", opRef, hi);
            break;
        case LongInst_ContextManip:
            snprintf(out, outSize, "%-10s {%s, R[%u], %d}",
                     "CtxManip", CtxM_toChars((CtxM)opSpecial), lhs, imm);
            break;
        case LongInst_Call:
            snprintf(out, outSize, "%-10s R[%u] = call(R[%u])",
                     "Call", lhs, rhs);
            break;
        case LongInst_HeapLoad8:
        case LongInst_HeapLoad16:
        case LongInst_HeapLoad32:
        case LongInst_HeapLoad64:
            snprintf(out, outSize, "%-10s R[%u] = M%u[R[%u]]",
                     "HeapLoad", lhs,
                     8u << (inst - LongInst_HeapLoad8), rhs);
            break;
        case LongInst_HeapStore8:
        case LongInst_HeapStore16:
        case LongInst_HeapStore32:
        case LongInst_HeapStore64:
            snprintf(out, outSize, "%-10s M%u[R[%u]] = R[%u]",
                     "HeapStore",
                     8u << (inst - LongInst_HeapStore8), lhs, rhs);
            break;
        case LongInst_SetMode:
            snprintf(out, outSize, "%-10s mode %u, value %d",
                     "SetMode", (lw >> 16) & 0xFFFFu, imm);
            break;
        case LongInst_Alloc:
            snprintf(out, outSize, "%-10s R[%u] = alloc(R[%u])",
                     "Alloc", lhs, rhs);
            break;
        case LongInst_MapExternal:
            snprintf(out, outSize, "%-10s R[%u] = ptr{sz: %d}",
                     "MapExternal", opRef, imm);
            break;
        case LongInst_MapExternalFunc:
            snprintf(out, outSize, "%-10s R[%u] = mapfn(R[%u], R[%u])",
                     "MapExtFunc", opRef, lhs, rhs);
            break;
        case LongInst_MemCpy:
            snprintf(out, outSize, "%-10s memcpy(R[%u], R[%u], R[%u])",
                     "MemCpy", lhs, rhs, opRef);
            break;
        case LongInst_Realloc:
            snprintf(out, outSize, "%-10s R[%u] = realloc(R[%u], R[%u])",
                     "Realloc", opRef, lhs, rhs);
            break;
        case LongInst_BuiltinCall:
            snprintf(out, outSize, "BuiltinCall");
            break;
        case LongInst_Comment:
        case LongInst_File:
        {
            uint32_t maxLen = ((ip + 2) < codeLen) ? (codeLen - ip - 2) * 4 : 0;
            uint32_t len = hi;
            if (len > maxLen)
                len = maxLen;
            snprintf(out, outSize, "%-10s \"%.*s\"",
                     (inst == LongInst_File) ? "File" : "Comment",
                     (int)len, (const char*)(code + ip + 2));
            break;
        }
        case LongInst_Line:
            snprintf(out, outSize, "%-10s %u", "Line", hi);
            break;
        case LongInst_ReadI32:
            snprintf(out, outSize, "%-10s R[%u], ctx %u",
                     "ReadI32", opRef, hi);
            break;
        case LongInst_ReadString:
            snprintf(out, outSize, "%-10s R[%u], ctx %u",
                     "ReadString", opRef, hi);
            break;
        case LongInst_F32ToF64:
            snprintf(out, outSize, "%-10s R[%u] = (double)R[%u]",
                     "F32ToF64", lhs, rhs);
            break;
        case LongInst_F32ToI:
            snprintf(out, outSize, "%-10s R[%u] = (int)R[%u]",
                     "F32ToI", lhs, rhs);
            break;
        case LongInst_IToF32:
            snprintf(out, outSize, "%-10s R[%u] = (float)R[%u]",
                     "IToF32", lhs, rhs);
            break;
        case LongInst_F64ToF32:
            snprintf(out, outSize, "%-10s R[%u] = (float)R[%u]",
                     "F64ToF32", lhs, rhs);
            break;
        case LongInst_F64ToI:
            snprintf(out, outSize, "%-10s R[%u] = (int64_t)R[%u]",
                     "F64ToI", lhs, rhs);
            break;
        case LongInst_IToF64:
            snprintf(out, outSize, "%-10s R[%u] = (double)R[%u]",
                     "IToF64", lhs, rhs);
            break;
        case LongInst_FAdd32: case LongInst_FSub32:
        case LongInst_FDiv32: case LongInst_FMul32:
        case LongInst_FEq32:  case LongInst_FNeq32:
        case LongInst_FLt32:  case LongInst_FLe32:
        case LongInst_FGt32:  case LongInst_FGe32:
        case LongInst_FAdd64: case LongInst_FSub64:
        case LongInst_FDiv64: case LongInst_FMul64:
        case LongInst_FEq64:  case LongInst_FNeq64:
        case LongInst_FLt64:  case LongInst_FLe64:
        case LongInst_FGt64:  case LongInst_FGe64:
#ifdef WITH_FMOD
        case LongInst_FMod32:
        case LongInst_FMod64:
#endif
            snprintf(out, outSize, "%-10s R[%u], R[%u]",
                     BCDebugger_FloatName(inst), lhs, rhs);
            break;
        default:
            snprintf(out, outSize, "<inst %u> lw=0x%08x hi=0x%08x",
                     inst, lw, hi);
            break;
    }
}

/* ---------------------------------------------------------------------- */
/* memory examination (guarded against bad pointers)                      */
/* ---------------------------------------------------------------------- */

static void BCDebugger_SegvHandler(int sig)
{
    (void)sig;
    siglongjmp(bcDbgSegvJmp, 1);
}

static int BCDebugger_SafeRead(const void* src, void* dst, size_t n)
{
    struct sigaction sa, oldSegv, oldBus;
    int ok = 0;

    memset(&sa, 0, sizeof(sa));
    sa.sa_handler = BCDebugger_SegvHandler;
    sigemptyset(&sa.sa_mask);

    if (sigaction(SIGSEGV, &sa, &oldSegv) != 0)
        return 0;
    sigaction(SIGBUS, &sa, &oldBus);

    if (sigsetjmp(bcDbgSegvJmp, 1) == 0)
    {
        memcpy(dst, src, n);
        ok = 1;
    }

    sigaction(SIGSEGV, &oldSegv, NULL);
    sigaction(SIGBUS, &oldBus, NULL);
    return ok;
}

/* ---------------------------------------------------------------------- */
/* rendering                                                              */
/* ---------------------------------------------------------------------- */

static void BCDebugger_DrawBox(int y, int x, int h, int w, const char* title)
{
    int i;
    if (h < 2 || w < 2)
        return;
    mvaddch(y, x, ACS_ULCORNER);
    mvaddch(y, x + w - 1, ACS_URCORNER);
    mvaddch(y + h - 1, x, ACS_LLCORNER);
    mvaddch(y + h - 1, x + w - 1, ACS_LRCORNER);
    for (i = 1; i < w - 1; i++)
    {
        mvaddch(y, x + i, ACS_HLINE);
        mvaddch(y + h - 1, x + i, ACS_HLINE);
    }
    for (i = 1; i < h - 1; i++)
    {
        mvaddch(y + i, x, ACS_VLINE);
        mvaddch(y + i, x + w - 1, ACS_VLINE);
    }
    if (title && w > 4)
    {
        int tl = (int)strlen(title);
        int max = w - 4;
        if (tl > max)
            tl = max;
        if (tl > 0)
        {
            if (bcDebugger.colors)
                attron(COLOR_PAIR(BCDBG_PAIR_TITLE));
            mvaddnstr(y, x + 2, title, tl);
            if (bcDebugger.colors)
                attroff(COLOR_PAIR(BCDBG_PAIR_TITLE));
        }
    }
}

static void BCDebugger_PutLine(int y, int x, int maxw, const char* s)
{
    int avail;
    if (y < 0 || y >= LINES || x < 0 || x >= COLS || maxw <= 0)
        return;
    avail = COLS - x;
    if (avail > maxw)
        avail = maxw;
    if (avail <= 0)
        return;
    mvaddnstr(y, x, s, avail);
}

/// number of registers belonging to the current frame
static uint32_t BCDebugger_FrameRegs(BCDebugger* dbg)
{
    BCInterpreter* st = dbg->state;
    BCGen* gen = dbg->gen;
    uint32_t n = 0;

    if (gen && st && st->fnIdx < gen->functionCount)
        n = gen->functions[st->fnIdx].maxStackUsed / 4;
    if (n == 0)
        n = 32;
    if (n > MAX_REGS)
        n = MAX_REGS;
    return n;
}

static void BCDebugger_ScrollDisasm(BCDebugger* dbg, int delta)
{
    int maxIdx = (int)dbg->idxCount - 1;
    if (maxIdx < 0)
        maxIdx = 0;
    dbg->followPc = false;
    dbg->viewIdx += delta;
    if (dbg->viewIdx < 0)
        dbg->viewIdx = 0;
    if (dbg->viewIdx > maxIdx)
        dbg->viewIdx = maxIdx;
}

static void BCDebugger_ScrollRegs(BCDebugger* dbg, int delta)
{
    int base = (int)dbg->state->stackTop;
    int max = base + (int)BCDebugger_FrameRegs(dbg) - 1;
    dbg->regView += delta;
    if (dbg->regView < base)
        dbg->regView = base;
    if (dbg->regView > max)
        dbg->regView = max;
}

static void BCDebugger_Render(BCDebugger* dbg)
{
    BCInterpreter* st = dbg->state;
    BCGen* gen = dbg->gen;
    int L = LINES, W = COLS;
    int bodyTop, bodyBot, disasmH, regsTop, regsH, regsW;
    int rows, i, startIdx, maxStart, pcIdx;
    uint32_t frameBase, frameRegs;
    int pcAttr;
    char line[512];

    erase();

    if (L < 16 || W < 60)
    {
        mvprintw(0, 0, "bc-debugger: terminal too small (%dx%d), need 60x16",
                 W, L);
        refresh();
        return;
    }

    /* ---- header ---------------------------------------------------- */
    attron(A_REVERSE);
    mvhline(0, 0, ' ', W);
    snprintf(line, sizeof(line),
             " bc-debugger | ip=%04x fn=%u/%u depth=%u rets=%u sp=%u "
             "mode=0x%x cond=%d line=%u ",
             st->ip, st->fnIdx + 1, gen->functionCount, st->callDepth,
             st->n_return_addrs, st->stackTop, st->mode, dbg->cond,
             st->lastLine);
    BCDebugger_PutLine(0, 0, W, line);
    attroff(A_REVERSE);

    {
        const char* tag = dbg->running ? "[ RUNNING ]" : "[ PAUSED ]";
        int tagLen = (int)strlen(tag);
        int tagX = W - tagLen - 1;
        int pair = dbg->running ? BCDBG_PAIR_RUN : BCDBG_PAIR_STOP;
        if (tagX < 0)
            tagX = 0;
        if (dbg->colors)
            attron(COLOR_PAIR(pair) | A_BOLD);
        else
            attron(A_BOLD);
        BCDebugger_PutLine(0, tagX, tagLen + 1, tag);
        if (dbg->colors)
            attroff(COLOR_PAIR(pair) | A_BOLD);
        else
            attroff(A_BOLD);
    }

    /* ---- layout ----------------------------------------------------- */
    bodyTop = 1;
    bodyBot = L - 4;                       /* L-3 status, L-2 rule, L-1 input */
    disasmH = ((bodyBot - bodyTop + 1) * 55) / 100;
    if (disasmH < 6)
        disasmH = 6;
    if (disasmH > (bodyBot - bodyTop))
        disasmH = bodyBot - bodyTop;
    regsTop = bodyTop + disasmH;
    regsH = bodyBot - regsTop + 1;
    if (regsH < 5 && disasmH > 6)
    {
        disasmH -= (5 - regsH);
        regsTop = bodyTop + disasmH;
        regsH = bodyBot - regsTop + 1;
    }
    regsW = (W * 60) / 100;
    if (regsW < 24)
        regsW = 24;
    if (regsW > W - 24)
        regsW = W - 24;

    /* ---- disassembly ------------------------------------------------ */
    BCDebugger_BuildIndex(dbg);
    pcIdx = (dbg->idxCount > 1) ? BCDebugger_IndexOfIp(dbg, st->ip) : -1;
    dbg->pcIdx = pcIdx;
    rows = disasmH - 2;

    BCDebugger_DrawBox(bodyTop, 0, disasmH, W, " Disassembly ");

    if (dbg->followPc && pcIdx >= 0)
        startIdx = pcIdx - rows / 2;
    else
        startIdx = dbg->viewIdx;
    maxStart = (int)dbg->idxCount - rows;
    if (maxStart < 0)
        maxStart = 0;
    if (startIdx < 0)
        startIdx = 0;
    if (startIdx > maxStart)
        startIdx = maxStart;
    dbg->viewIdx = startIdx;

    if (dbg->idxCount <= 1)
    {
        BCDebugger_PutLine(bodyTop + 1, 2, W - 4, "<no instructions>");
    }
    else if (pcIdx < 0 && dbg->followPc)
    {
        char tmp[128];
        snprintf(tmp, sizeof(tmp), "<pc 0x%04x is not an instruction>", st->ip);
        BCDebugger_PutLine(bodyTop + 1, 2, W - 4, tmp);
    }

    pcAttr = dbg->colors ? COLOR_PAIR(BCDBG_PAIR_PC) : A_REVERSE;
    for (i = 0; i < rows; i++)
    {
        int idx = startIdx + i;
        uint32_t ip;
        char mark, pcMark, inst[320];

        if (idx < 0 || idx >= (int)dbg->idxCount)
            break;
        ip = bcDbgIndex[idx];
        mark = BCDebugger_HasBreakpoint(dbg, ip) ? '*' : ' ';
        pcMark = (idx == pcIdx) ? '>' : ' ';
        BCDebugger_FormatInst(dbg->code, dbg->codeLen, ip, inst, sizeof(inst));
        snprintf(line, sizeof(line), "%c%c%04x: %s", pcMark, mark, ip, inst);
        if (idx == pcIdx)
        {
            attron(pcAttr);
            BCDebugger_PutLine(bodyTop + 1 + i, 1, W - 2, line);
            attroff(pcAttr);
        }
        else
        {
            BCDebugger_PutLine(bodyTop + 1 + i, 1, W - 2, line);
        }
    }

    /* ---- registers --------------------------------------------------- */
    frameBase = st->stackTop;
    frameRegs = BCDebugger_FrameRegs(dbg);
    if (dbg->regView < (int)frameBase)
        dbg->regView = (int)frameBase;

    {
        char title[96];
        snprintf(title, sizeof(title), " Registers [%u..%u] ",
                 frameBase, frameBase + frameRegs - 1);
        BCDebugger_DrawBox(regsTop, 0, regsH, regsW, title);
    }

    snprintf(line, sizeof(line), "fn=%u [%04x..%04x] frame=%u..%u",
             st->fnIdx + 1,
             (st->fnIdx < gen->functionCount)
                 ? gen->functions[st->fnIdx].bytecode_start : 0,
             (st->fnIdx < gen->functionCount)
                 ? gen->functions[st->fnIdx].bytecode_end : 0,
             frameBase, frameBase + frameRegs - 1);
    BCDebugger_PutLine(regsTop + 1, 2, regsW - 4, line);

    for (i = 1; i < regsH - 2; i++)
    {
        int reg = dbg->regView + i - 1;
        int64_t v;
        if (reg < (int)frameBase ||
            reg >= (int)(frameBase + frameRegs) ||
            reg >= (int)MAX_REGS)
            break;
        v = st->regs[reg];
        if (regsW >= 52)
            snprintf(line, sizeof(line), "R[%03d] 0x%016llx %20lld",
                     reg, (unsigned long long)v, (long long)v);
        else
            snprintf(line, sizeof(line), "R[%03d] 0x%016llx",
                     reg, (unsigned long long)v);
        BCDebugger_PutLine(regsTop + 1 + i, 2, regsW - 4, line);
    }

    /* ---- backtrace / command output ---------------------------------- */
    {
        int btX = regsW;
        int btW = W - regsW;
        int inner = regsH - 2;
        const char* title = dbg->outCount ? " Output " : " Backtrace ";

        BCDebugger_DrawBox(regsTop, btX, regsH, btW, title);

        if (dbg->outCount)
        {
            for (i = 0; i < dbg->outCount && i < inner; i++)
                BCDebugger_PutLine(regsTop + 1 + i, btX + 2, btW - 4,
                                   dbg->out[i]);
        }
        else
        {
            int row = 0;
            snprintf(line, sizeof(line), "#0 fn=%u ip=%04x sp=%u",
                     st->fnIdx + 1, st->ip, st->stackTop);
            BCDebugger_PutLine(regsTop + 1 + row, btX + 2, btW - 4, line);
            for (i = (int)st->n_return_addrs; i > 0 && row + 1 < inner; i--)
            {
                ReturnAddr* ra = &st->returnAddrs[i - 1];
                row++;
                snprintf(line, sizeof(line), "#%d fn=%u ip=%04x sp=%u",
                         row, ra->fnId + 1, ra->ip, ra->stackTop);
                BCDebugger_PutLine(regsTop + 1 + row, btX + 2, btW - 4, line);
            }
        }
    }

    /* ---- status + input ---------------------------------------------- */
    attron(A_REVERSE);
    mvhline(L - 3, 0, ' ', W);
    BCDebugger_PutLine(L - 3, 0, W, dbg->status);
    attroff(A_REVERSE);

    mvhline(L - 2, 0, ACS_HLINE, W);

    if (dbg->colors)
        attron(COLOR_PAIR(BCDBG_PAIR_TITLE) | A_BOLD);
    BCDebugger_PutLine(L - 1, 0, 8, "(bctui) ");
    if (dbg->colors)
        attroff(COLOR_PAIR(BCDBG_PAIR_TITLE) | A_BOLD);
    BCDebugger_PutLine(L - 1, 8, W - 8, dbg->input);

    curs_set(1);
    move(L - 1, 8 + dbg->inputLen);
    refresh();
}

/* ---------------------------------------------------------------------- */
/* commands                                                               */
/* ---------------------------------------------------------------------- */

static int BCDebugger_Resume(BCDebugger* dbg, BCDebuggerStepMode mode)
{
    dbg->running = true;
    dbg->autoRun = false;
    dbg->stepMode = mode;
    if (mode == BCDBG_STEP_LINE)
        dbg->stepLine = dbg->state->lastLine;
    if (dbg->atExit)
    {
        dbg->atExit = false;
        dbg->exitReason = "program exited";
    }
    switch (mode)
    {
        case BCDBG_STEP_ONE:  BCDebugger_SetStatus(dbg, "stepping"); break;
        case BCDBG_STEP_LINE: BCDebugger_SetStatus(dbg, "stepping to next line"); break;
        default:              BCDebugger_SetStatus(dbg, "running"); break;
    }
    return BCDBG_ACT_RESUME;
}

static int BCDebugger_Quit(BCDebugger* dbg)
{
    dbg->quit = true;
    dbg->active = false;
    dbg->exitReason = "quit by user";
    BCDebugger_SetStatus(dbg, "quitting");
    return BCDBG_ACT_QUIT;
}

static void BCDebugger_Examine(BCDebugger* dbg, const char* arg)
{
    const char* p = arg;
    uint32_t len = 64;
    unsigned long addr;
    char* endp = 0;
    uint8_t buf[256];
    uint32_t i;

    while (*p == ' ')
        p++;
    if (*p == '/')
    {
        unsigned long n = strtoul(p + 1, &endp, 0);
        if (n > 0 && n <= sizeof(buf))
            len = (uint32_t)n;
        p = endp ? endp : p + 1;
    }
    while (*p == ' ')
        p++;
    if (!*p)
    {
        BCDebugger_SetStatus(dbg, "usage: x[/n] <hex-address>");
        return;
    }
    addr = strtoul(p, &endp, 16);
    if (endp == p)
    {
        BCDebugger_SetStatus(dbg, "cannot parse address '%s'", p);
        return;
    }

    if (!BCDebugger_SafeRead((const void*)(uintptr_t)addr, buf, len))
    {
        BCDebugger_SetStatus(dbg, "invalid address 0x%lx", addr);
        return;
    }

    BCDebugger_ClearOutput(dbg);
    for (i = 0; i < len; i += 16)
    {
        char hex[3 * 16 + 1];
        char asc[17];
        uint32_t j, n = len - i;
        if (n > 16)
            n = 16;
        for (j = 0; j < n; j++)
        {
            sprintf(hex + j * 3, "%02x ", (unsigned)buf[i + j]);
            asc[j] = (buf[i + j] >= 0x20 && buf[i + j] < 0x7f)
                        ? (char)buf[i + j] : '.';
        }
        hex[n * 3] = 0;
        asc[n] = 0;
        BCDebugger_OutLine(dbg, "%08lx: %s |%s|", addr + i, hex, asc);
    }
    BCDebugger_SetStatus(dbg, "examined %u bytes at 0x%lx", len, addr);
}

static void BCDebugger_ShowHelp(BCDebugger* dbg)
{
    BCDebugger_ClearOutput(dbg);
    BCDebugger_OutLine(dbg, "s | step       step one instruction");
    BCDebugger_OutLine(dbg, "n | next       step to the next source line");
    BCDebugger_OutLine(dbg, "c | continue   resume execution");
    BCDebugger_OutLine(dbg, "b <ip>         set a breakpoint");
    BCDebugger_OutLine(dbg, "d <ip>         delete a breakpoint");
    BCDebugger_OutLine(dbg, "bl             list breakpoints");
    BCDebugger_OutLine(dbg, "r | regs       reset register view to frame");
    BCDebugger_OutLine(dbg, "bt             show the backtrace");
    BCDebugger_OutLine(dbg, "x[/n] <addr>   examine n bytes at hex address");
    BCDebugger_OutLine(dbg, "disas <ip>     move the disassembly view");
    BCDebugger_OutLine(dbg, "q | quit       terminate execution");
    BCDebugger_OutLine(dbg, "? | help       this text");
    BCDebugger_OutLine(dbg, "keys: PgUp/PgDn scroll, +/- regs, ^D quit");
    BCDebugger_OutLine(dbg, "env: BC_DEBUG_UI=paused|run|off");
    BCDebugger_OutLine(dbg, "     BC_DEBUG_BREAK=0x10,40");
    BCDebugger_SetStatus(dbg, "help");
}

static int BCDebugger_ParseIp(BCDebugger* dbg, const char* arg, uint32_t* outIp)
{
    char* endp = 0;
    unsigned long ip;
    while (*arg == ' ')
        arg++;
    if (!*arg)
    {
        BCDebugger_SetStatus(dbg, "missing instruction address");
        return 0;
    }
    ip = strtoul(arg, &endp, 0);
    if (endp == arg)
    {
        BCDebugger_SetStatus(dbg, "cannot parse address '%s'", arg);
        return 0;
    }
    *outIp = (uint32_t)ip;
    return 1;
}

static int BCDebugger_ExecCommand(BCDebugger* dbg, const char* raw)
{
    char line[BCDBG_INPUT_LEN];
    char* tok;
    char* arg;
    size_t len;
    uint32_t ip;
    uint32_t i, n;
    int idx;

    /* trim */
    while (*raw == ' ')
        raw++;
    snprintf(line, sizeof(line), "%s", raw);
    len = strlen(line);
    while (len > 0 && (line[len - 1] == ' ' || line[len - 1] == '\t'))
        line[--len] = 0;

    if (!line[0])
    {
        if (!dbg->lastCmd[0])
        {
            BCDebugger_SetStatus(dbg, "nothing to repeat");
            return BCDBG_ACT_NONE;
        }
        snprintf(line, sizeof(line), "%s", dbg->lastCmd);
    }
    else
    {
        snprintf(dbg->lastCmd, sizeof(dbg->lastCmd), "%s", line);
    }

    tok = line;
    arg = line;
    while (*arg && *arg != ' ' && *arg != '\t')
        arg++;
    if (*arg)
    {
        *arg++ = 0;
        while (*arg == ' ' || *arg == '\t')
            arg++;
    }

    if (!strcmp(tok, "s") || !strcmp(tok, "step"))
        return BCDebugger_Resume(dbg, BCDBG_STEP_ONE);
    if (!strcmp(tok, "n") || !strcmp(tok, "next"))
        return BCDebugger_Resume(dbg, BCDBG_STEP_LINE);
    if (!strcmp(tok, "c") || !strcmp(tok, "continue") || !strcmp(tok, "cont"))
        return BCDebugger_Resume(dbg, BCDBG_STEP_NONE);
    if (!strcmp(tok, "q") || !strcmp(tok, "quit") || !strcmp(tok, "kill"))
        return BCDebugger_Quit(dbg);
    if (!strcmp(tok, "?") || !strcmp(tok, "h") || !strcmp(tok, "help"))
    {
        BCDebugger_ShowHelp(dbg);
        return BCDBG_ACT_NONE;
    }
    if (!strcmp(tok, "bt") || !strcmp(tok, "backtrace"))
    {
        BCDebugger_ClearOutput(dbg);
        BCDebugger_SetStatus(dbg, "backtrace");
        return BCDBG_ACT_NONE;
    }
    if (!strcmp(tok, "r") || !strcmp(tok, "regs"))
    {
        dbg->regView = (int)dbg->state->stackTop;
        BCDebugger_SetStatus(dbg, "register view reset to sp=%u",
                             dbg->state->stackTop);
        return BCDBG_ACT_NONE;
    }
    if (!strcmp(tok, "bl") || !strcmp(tok, "breakpoints"))
    {
        BCDebugger_ClearOutput(dbg);
        if (!dbg->nBreakpoints)
            BCDebugger_OutLine(dbg, "no breakpoints");
        for (i = 0; i < dbg->nBreakpoints; i++)
            BCDebugger_OutLine(dbg, "  %2u: ip=%04x", i + 1,
                               dbg->breakpoints[i]);
        BCDebugger_SetStatus(dbg, "%u breakpoint(s)", dbg->nBreakpoints);
        return BCDBG_ACT_NONE;
    }
    if (!strcmp(tok, "b") || !strcmp(tok, "break") || !strcmp(tok, "breakat"))
    {
        if (!BCDebugger_ParseIp(dbg, arg, &ip))
            return BCDBG_ACT_NONE;
        if (BCDebugger_IndexOfIp(dbg, ip) < 0)
        {
            BCDebugger_SetStatus(dbg,
                "warning: 0x%04x is not an instruction boundary", ip);
            return BCDBG_ACT_NONE;
        }
        BCDebugger_AddBreakpoint(dbg, ip);
        BCDebugger_SetStatus(dbg, "breakpoint set at ip=%04x", ip);
        return BCDBG_ACT_NONE;
    }
    if (!strcmp(tok, "d") || !strcmp(tok, "del") || !strcmp(tok, "delete"))
    {
        if (!BCDebugger_ParseIp(dbg, arg, &ip))
            return BCDBG_ACT_NONE;
        n = dbg->nBreakpoints;
        for (i = 0; i < n; i++)
        {
            if (dbg->breakpoints[i] == ip)
            {
                dbg->breakpoints[i] = dbg->breakpoints[n - 1];
                dbg->nBreakpoints--;
                BCDebugger_SetStatus(dbg, "breakpoint at ip=%04x deleted", ip);
                return BCDBG_ACT_NONE;
            }
        }
        BCDebugger_SetStatus(dbg, "no breakpoint at ip=%04x", ip);
        return BCDBG_ACT_NONE;
    }
    if (!strcmp(tok, "x") || !strcmp(tok, "examine"))
    {
        BCDebugger_Examine(dbg, arg);
        return BCDBG_ACT_NONE;
    }
    if (!strncmp(tok, "x/", 2))
    {
        char argbuf[BCDBG_INPUT_LEN];
        snprintf(argbuf, sizeof(argbuf), "/%s %s", tok + 2, arg);
        BCDebugger_Examine(dbg, argbuf);
        return BCDBG_ACT_NONE;
    }
    if (!strcmp(tok, "disas") || !strcmp(tok, "disassemble"))
    {
        if (!BCDebugger_ParseIp(dbg, arg, &ip))
            return BCDBG_ACT_NONE;
        idx = BCDebugger_IndexOfIp(dbg, ip);
        if (idx < 0)
        {
            BCDebugger_SetStatus(dbg, "0x%04x is not an instruction", ip);
            return BCDBG_ACT_NONE;
        }
        dbg->followPc = false;
        dbg->viewIdx = idx;
        BCDebugger_SetStatus(dbg, "view moved to ip=%04x", ip);
        return BCDBG_ACT_NONE;
    }

    BCDebugger_SetStatus(dbg, "unknown command '%s' (try help)", tok);
    return BCDBG_ACT_NONE;
}

static int BCDebugger_HandleCh(BCDebugger* dbg, int ch)
{
    if (ch == ERR)
        return BCDBG_ACT_NONE;

    switch (ch)
    {
        case KEY_RESIZE:
            return BCDBG_ACT_NONE;
        case KEY_UP:
            BCDebugger_ScrollDisasm(dbg, -1);
            return BCDBG_ACT_NONE;
        case KEY_DOWN:
            BCDebugger_ScrollDisasm(dbg, 1);
            return BCDBG_ACT_NONE;
        case KEY_PPAGE:
            BCDebugger_ScrollDisasm(dbg, -8);
            return BCDBG_ACT_NONE;
        case KEY_NPAGE:
            BCDebugger_ScrollDisasm(dbg, 8);
            return BCDBG_ACT_NONE;
        case KEY_HOME:
            dbg->followPc = true;
            return BCDBG_ACT_NONE;
        case '+':
        case '=':
            BCDebugger_ScrollRegs(dbg, 1);
            return BCDBG_ACT_NONE;
        case '-':
        case '_':
            BCDebugger_ScrollRegs(dbg, -1);
            return BCDBG_ACT_NONE;
        case '\n':
        case '\r':
        case KEY_ENTER:
        {
            char raw[BCDBG_INPUT_LEN];
            int act;
            snprintf(raw, sizeof(raw), "%s", dbg->input);
            dbg->inputLen = 0;
            dbg->input[0] = 0;
            act = BCDebugger_ExecCommand(dbg, raw);
            return act;
        }
        case KEY_BACKSPACE:
        case 127:
        case 8:
            if (dbg->inputLen > 0)
                dbg->input[--dbg->inputLen] = 0;
            return BCDBG_ACT_NONE;
        case 21: /* ^U */
            dbg->inputLen = 0;
            dbg->input[0] = 0;
            return BCDBG_ACT_NONE;
        case 3: /* ^C */
            BCDebugger_SetStatus(dbg, "already paused - c to continue, q to quit");
            return BCDBG_ACT_NONE;
        case 4: /* ^D on an empty line quits */
            if (dbg->inputLen == 0)
                return BCDebugger_Quit(dbg);
            return BCDBG_ACT_NONE;
        default:
            if (ch >= 32 && ch < 256 &&
                dbg->inputLen + 1 < (int)sizeof(dbg->input))
            {
                dbg->input[dbg->inputLen++] = (char)ch;
                dbg->input[dbg->inputLen] = 0;
            }
            return BCDBG_ACT_NONE;
    }
}

/* ---------------------------------------------------------------------- */
/* the screen                                                             */
/* ---------------------------------------------------------------------- */

static void BCDebugger_StartUi(BCDebugger* dbg)
{
    if (dbg->uiUp)
        return;

    if (!bcDbgCursesAlive)
    {
        initscr();
        cbreak();
        noecho();
        keypad(stdscr, TRUE);
        curs_set(1);
        if (has_colors())
        {
            start_color();
            use_default_colors();
            init_pair(BCDBG_PAIR_TITLE, COLOR_CYAN, -1);
            init_pair(BCDBG_PAIR_STOP, COLOR_YELLOW, -1);
            init_pair(BCDBG_PAIR_RUN, COLOR_GREEN, -1);
            init_pair(BCDBG_PAIR_PC, COLOR_BLACK, COLOR_CYAN);
            init_pair(BCDBG_PAIR_STATUS, COLOR_RED, -1);
            dbg->colors = true;
        }
        bcDbgCursesAlive = true;
    }
    else
    {
        /* resume curses mode after a previous endwin() */
        refresh();
    }
    dbg->uiUp = true;
}

static void BCDebugger_UILoop(BCDebugger* dbg)
{
    BCDebugger_StartUi(dbg);
    if (!dbg->uiUp)
    {
        /* cannot draw anything - just let the program run */
        dbg->running = true;
        dbg->stepMode = BCDBG_STEP_NONE;
        return;
    }

    /* we are stopped and waiting for the user */
    dbg->running = false;

    /* a fresh stop: recentre the view and drop stale output */
    dbg->idxCount = 0;
    BCDebugger_BuildIndex(dbg);
    dbg->followPc = true;
    dbg->regView = (int)dbg->state->stackTop;
    dbg->outCount = 0;

    for (;;)
    {
        int ch;
        int act;
        BCDebugger_Render(dbg);
        nodelay(stdscr, FALSE);
        ch = getch();
        act = BCDebugger_HandleCh(dbg, ch);
        if (act != BCDBG_ACT_NONE)
            break;
    }
    nodelay(stdscr, FALSE);
}

/// look for ^C while the interpreter is running
static void BCDebugger_PollInterrupt(BCDebugger* dbg)
{
    int ch;
    if (!dbg->uiUp)
        return;
    if (++dbg->pollCounter < BCDBG_POLL_INTERVAL)
        return;
    dbg->pollCounter = 0;

    nodelay(stdscr, TRUE);
    ch = getch();
    nodelay(stdscr, FALSE);

    if (ch == 3) /* ^C */
    {
        dbg->running = false;
        dbg->stepMode = BCDBG_STEP_NONE;
        BCDebugger_SetStatus(dbg, "interrupted (^C)");
    }
}

static int BCDebugger_IsRetAt(BCDebugger* dbg, uint32_t ip)
{
    if (!dbg->code || (ip + 1) >= dbg->codeLen)
        return 0;
    switch (dbg->code[ip] & INSTMASK)
    {
        case LongInst_Ret32:
        case LongInst_Ret64:
        case LongInst_RetS32:
        case LongInst_RetS64:
            return 1;
        default:
            return 0;
    }
}

/// returns 1 when execution has to stop at the current instruction
static int BCDebugger_ShouldStop(BCDebugger* dbg)
{
    BCInterpreter* st = dbg->state;
    uint32_t ip = st->ip;

    if (BCDebugger_HasBreakpoint(dbg, ip))
    {
        dbg->atExit = false;
        BCDebugger_SetStatus(dbg, "breakpoint at ip=%04x (line %u)",
                             ip, st->lastLine);
        return 1;
    }

    if (dbg->stepMode == BCDBG_STEP_ONE)
    {
        dbg->stepMode = BCDBG_STEP_NONE;
        dbg->atExit = false;
        BCDebugger_SetStatus(dbg, "paused at line %u", st->lastLine);
        return 1;
    }
    if (dbg->stepMode == BCDBG_STEP_LINE && st->lastLine != dbg->stepLine)
    {
        dbg->stepMode = BCDBG_STEP_NONE;
        dbg->atExit = false;
        BCDebugger_SetStatus(dbg, "paused at line %u", st->lastLine);
        return 1;
    }

    /* the outermost return - let the user take a last look at the state */
    if (!dbg->autoRun && st->n_return_addrs == 0 && BCDebugger_IsRetAt(dbg, ip))
    {
        dbg->atExit = true;
        BCDebugger_SetStatus(dbg,
            "program is about to return - c to finish, q to abort");
        return 1;
    }

    return 0;
}

/**
 * Called from the interpreter loop right before an instruction is fetched,
 * so state->ip points at the instruction that is about to execute.
 * Returns 1 when the interpreter should bail out.
 */
static int BCDebugger_OnInstruction(BCInterpreter* state, BCGen* gen,
                                    uint32_t* code, int cond, BCHeap* heap)
{
    BCDebugger* dbg = &bcDebugger;

    if (!dbg->configured)
        BCDebugger_Configure(dbg);
    if (!dbg->active)
        return 0;

    dbg->state = state;
    dbg->gen = gen;
    dbg->code = code;
    dbg->cond = cond;
    dbg->heap = heap;

    /* pick up freshly generated code */
    dbg->codeLen = gen->byteCodeCount ? gen->byteCodeCount : gen->ip;
    if (dbg->codeLen > BCDBG_MAX_INDEX * 2)
        dbg->codeLen = BCDBG_MAX_INDEX * 2;

    if (!dbg->running || BCDebugger_ShouldStop(dbg))
    {
        BCDebugger_UILoop(dbg);
        if (dbg->quit)
            return 1;
        /* the current instruction still has to be executed */
        return 0;
    }

    BCDebugger_PollInterrupt(dbg);
    if (!dbg->running)
    {
        BCDebugger_UILoop(dbg);
        if (dbg->quit)
            return 1;
    }
    return 0;
}

/// called by the interpreter wrapper after every run
static void BCDebugger_OnExit(void)
{
    BCDebugger* dbg = &bcDebugger;
    if (dbg->uiUp)
    {
        endwin();
        dbg->uiUp = false;
    }
    if (dbg->exitReason)
    {
        fprintf(stderr, "[bc-debugger] %s\n", dbg->exitReason);
        dbg->exitReason = 0;
    }
}

/* ncurses pollutes the macro namespace - undo the interesting ones so that
 * the remainder of bc_interpreter_backend.c keeps compiling untouched */
#undef move
#undef clear
#undef refresh
#undef erase
#undef box
#undef border
#undef lines
#undef cols
#undef timeout
#undef delay
#undef filter
#undef echo
#undef standout
#undef standend
#undef getch
#undef addch
#undef addstr
#undef printw
#undef mvprintw
#undef mvaddch
#undef mvaddnstr
#undef mvhline
#undef mvvline
#undef attron
#undef attroff
#undef curs_set
#undef nodelay
#undef keypad
#undef cbreak
#undef noecho
#undef initscr
#undef endwin
#undef has_colors
#undef start_color
#undef use_default_colors
#undef init_pair
#undef COLOR_PAIR

#endif /* BC_DEBUG_TUI */

#endif /* _BC_DEBUGGER_TUI_C_ */
