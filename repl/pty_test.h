#ifndef PTY_TEST_H
#define PTY_TEST_H

#include <stddef.h>
#include <stdio.h>

/*
 * pty_test - drive a child program on a pseudo terminal from a C test.
 *
 * Typical use:
 *
 *     setenv("BC_DEBUG_UI", "paused", 1);
 *     PTY* p = pty_spawn((char*[]){ "/path/to/repl", NULL }, 40, 120);
 *     if (!p) return 1;
 *     if (pty_expect(p, "EE>") != 0) { ... }
 *     if (pty_sendline(p, "1+1") != 0) { ... }
 *     if (pty_expect(p, "1 + 1 = 2") != 0) { ... }
 *     pty_close(p);
 *
 * The child sees a real terminal: linenoise runs in raw mode, Enter is "\r"
 * (never "\n"), and programs that check isatty() - like the bytecode
 * debugger - start up normally.
 *
 * expect() matches the screen the child drew, not the raw byte stream.
 * ncurses only transmits the cells it changed, so a line of text arrives in
 * fragments with holes in it; pty_test feeds the output through a small
 * terminal emulator so the screen always holds the whole line.
 *
 * Every function returns 0 on success and -1 on failure.  Failures print a
 * diagnostic (including the current screen) to stderr, so a test only has to
 * report which step went wrong.
 */

typedef struct PTY PTY;

/* spawn argv[0] with the remaining arguments on a rows x cols pty; the child
   inherits the environment.  returns NULL if the pty or the fork failed. */
PTY* pty_spawn(char* const argv[], int rows, int cols);

/* timeout in seconds used by expect/assert (default 10) */
void pty_set_timeout(PTY* p, double seconds);

/* mirror every byte from the child to `f`; pass NULL to stop */
void pty_capture(PTY* p, FILE* f);
void pty_echo(PTY* p, FILE* f);

/* write to the child; the *line variants append Enter (\r), not \n */
int pty_send(PTY* p, const char* text);
int pty_sendline(PTY* p, const char* text);
int pty_sendf(PTY* p, const char* fmt, ...);
int pty_sendlinef(PTY* p, const char* fmt, ...);

/* wait until text shows up on the screen */
int pty_expect(PTY* p, const char* text);
int pty_expectf(PTY* p, const char* fmt, ...);
int pty_expect_re(PTY* p, const char* pattern);

/* wait for the child to exit or its output to end */
int pty_expect_eof(PTY* p);

/* check the screen right now, without waiting */
int pty_assert(PTY* p, const char* text);
int pty_assert_not(PTY* p, const char* text);

/* let the child run for a while - only useful before an assert, never in
   place of an expect */
int pty_sleep(PTY* p, double seconds);

/* the screen as rows right-trimmed and joined with '\n' */
const char* pty_screen(PTY* p);
void pty_dump(PTY* p, FILE* to);

/* tear the session down: SIGTERM/SIGKILL a child that is still running, reap
   it and free p.  returns the child's exit status, or 0 when the child had
   to be killed (cleanup, not a failure). */
int pty_close(PTY* p);

#endif /* PTY_TEST_H */
