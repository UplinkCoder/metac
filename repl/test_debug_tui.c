/*
 * Call/response test for the BC_DEBUG_TUI debugger, written against the
 * pty_test C API (pty_test.h).  The same scenarios exist as a script test
 * in test_debug_tui.sh; this is the C version of them.
 *
 * usage: test_debug_tui /path/to/repl
 *
 * build: cc -O1 -Wall -Wextra test_debug_tui.c pty_test.c -o test_debug_tui
 */
#include <stdio.h>
#include <stdlib.h>

#include "pty_test.h"

static const char *replPath;
static const char *caseName;
static int caseFailed;

/* every step of a case goes through TRY; the first failure prints where it
   happened (pty_test has already dumped the screen to stderr) and jumps to
   the common teardown */
#define TRY(expr, what)                                                     \
    do {                                                                    \
        if ((expr) != 0) {                                                  \
            fprintf(stderr, "  %s:%d: %s\n", __FILE__, __LINE__, (what));   \
            caseFailed = 1;                                                 \
            goto out;                                                       \
        }                                                                   \
    } while (0)

static PTY *openRepl(void)
{
    char *argv[] = { (char *)replPath, NULL };
    return pty_spawn(argv, 40, 120);
}

static int finish(PTY *p)
{
    int rc = 0;
    int status = p ? pty_close(p) : 0;

    if (status != 0 && !caseFailed)
    {
        fprintf(stderr, "  child exited with status %d\n", status);
        caseFailed = 1;
    }
    if (caseFailed)
    {
        fprintf(stderr, "FAIL: %s\n", caseName);
        rc = 1;
    }
    else
        printf("PASS: %s\n", caseName);
    return rc;
}

/* starts paused, sets a breakpoint, disassembles, continues to the
   breakpoint, then finishes at the outermost return */
static int case_paused(void)
{
    PTY *p = NULL;
    caseName = "paused mode stops, breaks, disassembles and resumes";
    caseFailed = 0;
    setenv("BC_DEBUG_UI", "paused", 1);
    unsetenv("BC_DEBUG_BREAK");

    p = openRepl();
    if (!p) { perror("pty_spawn"); caseFailed = 1; goto out; }

    TRY(pty_expect(p, "EE>"), "prompt");
    TRY(pty_sendline(p, "1+1"), "send 1+1");
    TRY(pty_expect(p, "bc-debugger"), "debugger banner");
    TRY(pty_assert(p, "PAUSED"), "header says PAUSED");
    TRY(pty_assert_not(p, "NOP"), "index has no bogus NOP entries");

    TRY(pty_sendline(p, "help"), "help");
    TRY(pty_sleep(p, 0.4), "help draws");
    TRY(pty_assert(p, "BC_DEBUG_UI"), "help documents the env vars");

    TRY(pty_sendline(p, "b 0x0a"), "set breakpoint");
    TRY(pty_sleep(p, 0.3), "breakpoint draws");
    TRY(pty_assert(p, "ip=000a"), "breakpoint listed");

    TRY(pty_sendline(p, "bl"), "list breakpoints");
    TRY(pty_sleep(p, 0.3), "list draws");
    TRY(pty_assert(p, "1 breakpoint"), "one breakpoint listed");

    TRY(pty_sendline(p, "disas 0x0c"), "disassemble");
    TRY(pty_sleep(p, 0.3), "disassembly draws");
    TRY(pty_assert(p, "view moved to ip=000c"), "view moved");

    TRY(pty_sendline(p, "c"), "continue to the breakpoint");
    TRY(pty_expect(p, "breakpoint at ip=000a"), "breakpoint hit");
    TRY(pty_sendline(p, "c"), "continue to the end");
    TRY(pty_expect(p, "program is about to return"), "outermost return");
    TRY(pty_sendline(p, "c"), "finish the program");
    TRY(pty_expect(p, "1 + 1 = 2"), "result");
    TRY(pty_assert(p, "1 + 1 = 2"), "result on screen");

out:
    return finish(p);
}

/* same exchange with the debugger switched off / set to run */
static int case_plain(const char *name, const char *ui)
{
    PTY *p = NULL;
    caseName = name;
    caseFailed = 0;
    setenv("BC_DEBUG_UI", ui, 1);
    unsetenv("BC_DEBUG_BREAK");

    p = openRepl();
    if (!p) { perror("pty_spawn"); caseFailed = 1; goto out; }

    TRY(pty_expect(p, "EE>"), "prompt");
    TRY(pty_sendline(p, "1+1"), "send 1+1");
    TRY(pty_expect(p, "1 + 1 = 2"), "result");
    TRY(pty_assert_not(p, "bc-debugger"), "debugger never shows up");
    TRY(pty_assert(p, "1 + 1 = 2"), "result on screen");

out:
    return finish(p);
}

/* BC_DEBUG_UI=run only stops at breakpoints */
static int case_break(void)
{
    PTY *p = NULL;
    caseName = "BC_DEBUG_BREAK stops run mode at the requested instruction";
    caseFailed = 0;
    setenv("BC_DEBUG_UI", "run", 1);
    setenv("BC_DEBUG_BREAK", "0x000a", 1);

    p = openRepl();
    if (!p) { perror("pty_spawn"); caseFailed = 1; goto out; }

    TRY(pty_expect(p, "EE>"), "prompt");
    TRY(pty_sendline(p, "1+1"), "send 1+1");
    TRY(pty_expect(p, "breakpoint at ip=000a"), "breakpoint hit");
    TRY(pty_assert(p, "PAUSED"), "header says PAUSED");
    TRY(pty_sendline(p, "c"), "continue to the end");
    TRY(pty_expect(p, "program is about to return"), "outermost return");
    TRY(pty_sendline(p, "c"), "finish the program");
    TRY(pty_expect(p, "1 + 1 = 2"), "result");

out:
    return finish(p);
}

/* q bails the interpreter out and tears the screen down */
static int case_quit(void)
{
    PTY *p = NULL;
    caseName = "q aborts the run and tears the debugger screen down";
    caseFailed = 0;
    setenv("BC_DEBUG_UI", "paused", 1);
    unsetenv("BC_DEBUG_BREAK");

    p = openRepl();
    if (!p) { perror("pty_spawn"); caseFailed = 1; goto out; }

    TRY(pty_expect(p, "EE>"), "prompt");
    TRY(pty_sendline(p, "1+1"), "send 1+1");
    TRY(pty_expect(p, "bc-debugger"), "debugger banner");
    TRY(pty_assert(p, "PAUSED"), "header says PAUSED");
    TRY(pty_sendline(p, "q"), "quit");
    TRY(pty_sleep(p, 1.0), "teardown");
    TRY(pty_assert(p, "quit by user"), "exit reason printed");

out:
    return finish(p);
}

int main(int argc, char **argv)
{
    if (argc != 2)
    {
        fprintf(stderr, "usage: %s /path/to/repl\n", argv[0]);
        return 2;
    }
    replPath = argv[1];

    int rc = 0;
    rc |= case_paused();
    rc |= case_plain("BC_DEBUG_UI=off runs without the debugger", "off");
    rc |= case_plain("BC_DEBUG_UI=run runs to completion without pausing",
                     "run");
    rc |= case_break();
    rc |= case_quit();

    if (!rc) printf("PASS: bc_debugger_tui via the pty_test API\n");
    return rc;
}
