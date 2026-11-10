# Call/response tests for the REPL with `pty_test`

A call/response test drives the repl like a user would: send one line of
input, wait for the answer, check it. `repl/pty_test.c` runs the repl on a
pseudo terminal and lets you do that from C (`repl/pty_test.h`) or from a
shell script (`repl/pty_script.c`).

## Why the repl needs a pty

* The repl uses linenoise, which puts the tty in raw mode, echoes what you
  type and draws its own prompt. Piped stdin is not a terminal, so linenoise
  falls back and the session does not behave like the real thing.
* In raw mode `ICRNL` is off: **Enter is `\r` (13), not `\n`**. Input that
  sends `\n` will never submit a line.
* The bytecode debugger (`-DBC_DEBUG_TUI`) refuses to start unless
  `isatty(0)` and `isatty(1)` are true and `TERM` is set to something other
  than `dumb`.

`pty_test` gives you a real pty, a window size you choose, and a way to talk
to it: an API for C tests, a script language for shell tests.

## Building the pieces

```sh
cd repl
cc linenoise_repl.c -I.. -DACCEL=ACCEL_TABLE -DBC_DEBUG_TUI \
    -mtune=core2 -march=core2 -mstackrealign \
    -lm -lpthread -lncurses -o repl          # the program under test
cc -O1 -Wall -Wextra pty_script.c pty_test.c -o pty_script   # script driver
cc -O1 -Wall -Wextra test_debug_tui.c pty_test.c -o test_debug_tui  # C test
```

Drop `-DBC_DEBUG_TUI -lncurses` when the test does not involve the debugger.

`-mstackrealign` is not optional: without it the repl dies with SIGSEGV
before it prints anything on some toolchains (the same flags are used by
`test_run_eject.sh` and `build.sh`).

## Writing a test in C

Link `pty_test.c` next to your test and include `pty_test.h`. Every call
returns `0` on success and `-1` on failure; failures already print a
diagnostic *and the current screen* to stderr, so a test only has to say
which step went wrong.

| call | meaning |
| --- | --- |
| `pty_spawn(argv, rows, cols)` | start `argv[0]` on a rows×cols pty |
| `pty_set_timeout(p, sec)` | timeout for `expect`/`assert` (default 10) |
| `pty_send(p, text)` | write, no Enter |
| `pty_sendline(p, text)` | write followed by `\r` (the Enter key) |
| `pty_sendf` / `pty_sendlinef` | `printf`-style versions of the two above |
| `pty_expect(p, text)` | block until `text` is on the screen |
| `pty_expectf(p, fmt, ...)` | `printf`-style `expect` |
| `pty_expect_re(p, regex)` | wait for an extended POSIX regex |
| `pty_expect_eof(p)` | wait for the child to exit / output to end |
| `pty_assert(p, text)` | fail now unless `text` is on the screen |
| `pty_assert_not(p, text)` | fail now if `text` is on the screen |
| `pty_sleep(p, sec)` | let the child run for a while |
| `pty_screen(p)` | the screen as one string (rows joined with `\n`) |
| `pty_dump(p, FILE*)` | print the screen |
| `pty_capture(p, FILE*)` | also write the raw byte stream to a file |
| `pty_echo(p, FILE*)` | mirror the child's output (e.g. `stdout`) |
| `pty_close(p)` | kill, reap and free; returns the child's exit status |

The child inherits the environment, so set whatever the test needs before
spawning:

```c
#include <stdio.h>
#include <stdlib.h>
#include "pty_test.h"

#define TRY(e) do { if ((e) != 0) { rc = 1; goto out; } } while (0)

int main(void)
{
    int rc = 0;
    setenv("BC_DEBUG_UI", "paused", 1);
    unsetenv("BC_DEBUG_BREAK");

    PTY *p = pty_spawn((char *[]){ "./repl", NULL }, 40, 120);
    if (!p) return 1;

    TRY(pty_expect(p, "EE>"));             /* anchor on the prompt */
    TRY(pty_sendline(p, "1+1"));           /* the call */
    TRY(pty_expect(p, "bc-debugger"));     /* ... and its response */
    TRY(pty_sendline(p, "b 0x0a"));
    TRY(pty_sleep(p, 0.3));                /* time for the UI to draw */
    TRY(pty_assert(p, "ip=000a"));
    TRY(pty_sendline(p, "c"));
    TRY(pty_expect(p, "breakpoint at ip=000a"));
    TRY(pty_sendline(p, "c"));
    TRY(pty_sendline(p, "c"));             /* second c finishes the run */
    TRY(pty_expect(p, "1 + 1 = 2"));

out:
    if (pty_close(p) != 0 && !rc) rc = 1;  /* a crash fails the test */
    if (!rc) printf("PASS\n");
    return rc;
}
```

`repl/test_debug_tui.c` is the full worked example: five cases covering
paused/run/off modes, `BC_DEBUG_BREAK`, breakpoints, disassembly and `q`.
It is built and run by `repl/test_debug_tui.sh` alongside the script
version of the same scenarios.

## The script front-end

`pty_script` is a thin shell front end over the same API, for when a test is
easier to write as a script than as a program. One command per line, `#`
starts a comment, blank lines are ignored:

| command | meaning |
| --- | --- |
| `sleep <sec>` | wait, without matching anything |
| `send <text>` | write text, no Enter |
| `sendline <text>` | write text followed by `\r` (the Enter key) |
| `expect <text>` | block until `text` is on the screen |
| `expect_re <regex>` | same, with an extended POSIX regex |
| `expect_eof` | wait for the child to exit / output to end |
| `assert <text>` | fail now unless `text` is on the screen |
| `assert_not <text>` | fail now if `text` is on the screen |
| `timeout <sec>` | change the timeout for later `expect`s |

`text` may be quoted (`"..."`) and understands `\n`, `\r`, `\t`, `\\`, `\"`
and `\xNN`.

Invocation:

```sh
pty_script [-t sec] [-s rowsxcols] [-o raw.bin] [-q] [-f script] \
           -- ./repl [args...] < script.scr
```

* `-t` – default `expect` timeout in seconds (10).
* `-s` – terminal size, default `40x120`.
* `-o` – also write the raw byte stream to a file (for debugging).
* `-q` – do not echo the child's output to stdout.
* `-f` – read the script from a file instead of stdin.

Exit status: `0` success, `1` an `expect`/`assert` failed, `2` usage or exec
error, otherwise the child's own exit status — but a child that the script
itself killed counts as `0` (cleanup, not failure).

Minimal call/response script:

```
# repl/echo.scr
expect "EE>"
sendline "1+1"
expect "1 + 1 = 2"
assert_not "bc-debugger"
```

```sh
pty_script -t 10 -q -- ./repl < echo.scr && echo PASS
```

## How matching works

`expect` does **not** search the raw byte stream. It searches the screen
that `pty_test` reconstructs from the child's output.

That matters for curses programs. ncurses only transmits the cells that
changed, so a status line arrives in fragments with holes where the old text
already matched, for example:

```
breakpoint at[21Ga (line 0)      <- "ip=000" never sent
```

Searching the raw stream for `breakpoint at ip=000a` would fail about half
the time, depending on what the line said before. The reconstructed screen
always contains the full line.

`pty_test` understands cursor addressing (`CUP`, `VPA`, `CHP`), erasing
(`EL`, `ED`, `ECH`), line/character insert and delete, scrolling, the
alternate screen (`ESC[?1049h/l`) and replies to cursor-position queries
(`ESC[6n`). The screen is the rows right-trimmed and joined with `\n`.

On failure the current screen is printed to stderr, which is normally all you
need to see what went wrong.

## Writing a good call/response test

1. **Anchor on the prompt first.** Every session starts the same way, so
   start with `expect(p, "EE>")`.
2. **Send the call, then expect the answer.** `pty_sendline` submits it,
   `pty_expect` waits for the reply. Do not use `pty_sleep` to wait for a
   response — that is what makes tests flaky. Sleep is for giving the UI
   time to draw before an `assert`.
3. **Match the result, not the echo.** The repl echoes what you typed
   (`EE>1+1`) and prints generated code before the answer. Expect the
   distinctive form of the result: `1 + 1 = 2`.
4. **`expect` for things that arrive, `assert` for things already there.**
   Use `assert_not` to pin down absence (no debugger banner, no `NOP` in the
   disassembly).
5. **Pick unique strings.** `assert` scans the whole screen; text that stays
   in the scrollback (like `EE>`) can be matched again later. Prefer
   something that appears once: `ip=000a`, `breakpoint at ip=000a`,
   `view moved to ip=000c`.
6. **Keep timeouts generous, sleeps short.** `expect` blocks until the text
   shows up or the timeout expires, so a hung child fails the test instead of
   hanging the runner.
7. **Close the session.** `pty_close` reaps the child; its exit status is a
   cheap extra check that nothing crashed after the last expectation.

## A complete shell test

Follow the shape used by `repl/test_debug_tui.sh`: build into a temp
directory, run each scenario, and print the driver's stderr on failure.

```sh
#!/bin/bash
set -eu
cd "$(dirname "$0")"
WORK=$(mktemp -d)
trap 'rm -rf "$WORK"' EXIT

if ! cc linenoise_repl.c -I.. -DACCEL=ACCEL_TABLE \
    -mtune=core2 -march=core2 -mstackrealign \
    -lm -lpthread -o "$WORK/repl" > "$WORK/build.log" 2>&1; then
    cat "$WORK/build.log"
    exit 1
fi
if ! cc -O1 -Wall -Wextra pty_script.c pty_test.c -o "$WORK/pty_script" \
    > "$WORK/pty_build.log" 2>&1; then
    cat "$WORK/pty_build.log"
    exit 1
fi

run_case() {
    local name=$1 script=$2
    shift 2
    local status=0
    env "$@" timeout 45 "$WORK/pty_script" -t 10 -q \
        -- "$WORK/repl" < "$script" > /dev/null 2> "$WORK/err" || status=$?
    if [ "$status" -ne 0 ]; then
        echo "FAIL: $name (pty_script exit $status)"
        head -c 4000 "$WORK/err" 2>/dev/null || true
        exit 1
    fi
    echo "PASS: $name"
}

cat > "$WORK/case.scr" <<'EOF'
expect "EE>"
sendline "1+1"
expect "1 + 1 = 2"
assert_not "bc-debugger"
EOF

run_case "1+1 evaluates" "$WORK/case.scr"
```

Scenarios that need the debugger are the same shape, only the script and the
environment differ:

```
expect "EE>"
sendline "1+1"
expect "bc-debugger"
assert "PAUSED"
sendline "b 0x0a"
sleep 0.3
assert "ip=000a"
sendline "c"
expect "breakpoint at ip=000a"
sendline "c"
expect "1 + 1 = 2"
```

```sh
run_case "breakpoint" "$WORK/case.scr" BC_DEBUG_UI=paused
```

## Pitfalls

* **Enter is `\r`.** Always use `pty_sendline` / `sendline`; `send "x\n"`
  only moves the cursor down a line, it does not submit anything.
* **Build with `-mstackrealign`.** A repl built without it can segfault
  before it prints anything, which looks like a broken test.
* **The repl exits with status 1 on `:q`** (`main` returns 1 on normal
  termination). Do not expect `0` for a test that quits that way.
* **A debugged program stops twice**: once when it reaches the outermost
  `Ret` (`program is about to return`), and again after you press `c`.
  Tests that run to completion need two `c` commands.
* **ncurses installs a `SIGTERM` handler**, so a TUI killed by the driver
  exits with status 1 instead of 143. `pty_close` reports `0` for children
  it killed itself, so this does not show up as a failure.
* **Set the terminal size** (`-s` / the `pty_spawn` arguments) if a test is
  sensitive to layout; the debugger is drawn for 40 rows. The size is
  applied to both the master and the child, which is what keeps linenoise
  from issuing an `ESC[6n` cursor query at startup.
* **`TERM` must be set** and must not be `dumb`, or the debugger silently
  stays off and the test will fail looking for `bc-debugger`.

## Debugging a test

* `pty_capture(p, fopen("raw.bin", "wb"))` / `pty_script -o raw.bin` keeps
  every byte the child produced; look at it with `less -r raw.bin`.
* On any `expect`/`assert` failure the reconstructed screen goes to stderr —
  read that first (`pty_dump` prints it on demand too).
* Run the child under `script`/`strace` or shorten the test to the smallest
  failing exchange; almost all problems are either the missing `\r` or an
  `expect` string that does not match what is actually drawn.

## Existing tests

* `repl/test_debug_tui.sh` – builds the debugger repl, runs the five
  scenarios twice: once as scripts through `pty_script`, once as C through
  `pty_test` (`repl/test_debug_tui.c`). Covers paused/run/off modes,
  `BC_DEBUG_BREAK`, breakpoints, disassembly and `q`.
* `repl/test_run_eject.sh`, `repl/test_enum_type.sh`, `repl/test_dot_mode.sh`
  – repl features driven through a heredoc on plain stdin (no pty needed).
