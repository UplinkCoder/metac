#!/bin/bash
# Exercises the BC_DEBUG_TUI bytecode debugger over a pty.
#
# The debugger refuses to start without real terminals, so every scenario runs
# the repl as the child of pty_script.c (the little expect-style driver in
# this directory).  Build the same way by hand with:
#   ./build.sh -g -O0 -DBC_DEBUG_TUI -lncurses
set -eu
cd "$(dirname "$0")"
ulimit -c 0
WORK=$(mktemp -d)
trap 'rm -rf "$WORK"' EXIT

if ! cc linenoise_repl.c -I.. -DACCEL=ACCEL_TABLE -DBC_DEBUG_TUI \
    -mtune=core2 -march=core2 -mstackrealign \
    -lm -lpthread -lncurses \
    -o "$WORK/repl" > "$WORK/build.log" 2>&1; then
    cat "$WORK/build.log"
    exit 1
fi

if ! cc -O1 -Wall -Wextra pty_script.c -o "$WORK/pty_script" \
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

# ---------------------------------------------------------------- paused mode
# starts paused, sets a breakpoint, steps, disassembles, continues to the
# breakpoint, then finishes at the outermost return
cat > "$WORK/paused.scr" <<'EOF'
expect "EE>"
sendline "1+1"
expect "bc-debugger"
assert "PAUSED"
assert_not "NOP"
sendline "help"
sleep 0.4
assert "BC_DEBUG_UI"
sendline "b 0x0a"
sleep 0.3
assert "ip=000a"
sendline "bl"
sleep 0.3
assert "1 breakpoint"
sendline "disas 0x0c"
sleep 0.3
assert "view moved to ip=000c"
sendline "c"
expect "breakpoint at ip=000a"
sendline "c"
expect "program is about to return"
sendline "c"
expect "1 + 1 = 2"
assert "1 + 1 = 2"
EOF
run_case "paused mode stops, breaks, disassembles and resumes" \
    "$WORK/paused.scr" BC_DEBUG_UI=paused

# ------------------------------------------------------------------ off mode
cat > "$WORK/plain.scr" <<'EOF'
expect "EE>"
sendline "1+1"
expect "1 + 1 = 2"
assert_not "bc-debugger"
assert "1 + 1 = 2"
EOF
run_case "BC_DEBUG_UI=off runs without the debugger" \
    "$WORK/plain.scr" BC_DEBUG_UI=off

# ----------------------------------------------------------------- run mode
run_case "BC_DEBUG_UI=run runs to completion without pausing" \
    "$WORK/plain.scr" BC_DEBUG_UI=run

# -------------------------------------------------------- run + breakpoint
cat > "$WORK/break.scr" <<'EOF'
expect "EE>"
sendline "1+1"
expect "breakpoint at ip=000a"
assert "PAUSED"
sendline "c"
expect "program is about to return"
sendline "c"
expect "1 + 1 = 2"
assert "1 + 1 = 2"
EOF
run_case "BC_DEBUG_BREAK stops run mode at the requested instruction" \
    "$WORK/break.scr" BC_DEBUG_UI=run BC_DEBUG_BREAK=0x000a

# ----------------------------------------------------------------- q quits
cat > "$WORK/quit.scr" <<'EOF'
expect "EE>"
sendline "1+1"
expect "bc-debugger"
assert "PAUSED"
sendline "q"
sleep 1
assert "quit by user"
EOF
run_case "q aborts the run and tears the debugger screen down" \
    "$WORK/quit.scr" BC_DEBUG_UI=paused

echo "PASS: bc_debugger_tui"
