#!/bin/bash
set -eu
cd "$(dirname "$0")"
ulimit -c 0
WORK=$(mktemp -d)
trap 'rm -rf "$WORK"' EXIT

if ! cc linenoise_repl.c -I.. -DACCEL=ACCEL_TABLE -DOLD_PARSER=1 \
    -UMETAC_COMPILER_INTERFACE -mtune=core2 -march=core2 -mstackrealign \
    -lm -lpthread -o "$WORK/repl" > "$WORK/build.log" 2>&1; then
    cat "$WORK/build.log"
    exit 1
fi

status=0
timeout 30 stdbuf -o0 -e0 "$WORK/repl" > "$WORK/output" 2>&1 <<EOF || status=$?
:r $(pwd)/test_run_eject.mtc
:q
EOF

# The REPL currently returns 1 on normal :q termination.
if [ "$status" -ne 1 ] ||
   ! grep -qF 'semantic done: 3 decls' "$WORK/output" ||
   ! grep -qF 'Repl_Loop exited this should only happen on quit' "$WORK/output" ||
   grep -Eq 'SemanticError|Assertion|lookup failed|unsupported' "$WORK/output"; then
    cat "$WORK/output"
    echo "FAIL: @run/eject without the compiler interface (exit $status)"
    exit 1
fi
echo 'PASS: @run executes and reparses ejected declarations and statements without .Compiler'

status=0
timeout 30 stdbuf -o0 -e0 "$WORK/repl" > "$WORK/output" 2>&1 <<EOF || status=$?
:r $(pwd)/test_run_eject_strings.mtc
:q
EOF
if [ "$status" -ne 1 ] ||
   ! grep -qF 'semantic done: 8 decls' "$WORK/output" ||
   ! grep -qF 'Repl_Loop exited this should only happen on quit' "$WORK/output" ||
   grep -Eq 'SemanticError|Assertion|lookup failed|unsupported' "$WORK/output"; then
    cat "$WORK/output"
    echo "FAIL: @run string and identifier splices (exit $status)"
    exit 1
fi
echo 'PASS: VM strings produce identifiers and string literals without .Compiler'

sed 's/same_text(generated_name(), "hello world")/same_text(generated_name(), "hello worle")/' \
    test_run_eject_strings.mtc > "$WORK/wrong_string.mtc"
status=0
timeout 30 stdbuf -o0 -e0 "$WORK/repl" > "$WORK/output" 2>&1 <<EOF || status=$?
:r $WORK/wrong_string.mtc
:q
EOF
if [ "$status" -eq 124 ] ||
   ! grep -qF '@run execution failed' "$WORK/output" ||
   grep -qF 'semantic done:' "$WORK/output"; then
    cat "$WORK/output"
    echo 'FAIL: incorrect generated string content was not rejected'
    exit 1
fi
echo 'PASS: generated string assertions check exact content, not just the first character'

reject_string_splice() {
    local source=$1 diagnostic=$2 status=0
    printf '%s\n' "$source" > "$WORK/bad_splice.mtc"
    timeout 30 stdbuf -o0 -e0 "$WORK/repl" > "$WORK/output" 2>&1 <<EOF || status=$?
:r $WORK/bad_splice.mtc
:q
EOF
    if [ "$status" -eq 124 ] ||
       ! grep -qF "$diagnostic" "$WORK/output" ||
       grep -qF 'semantic done:' "$WORK/output"; then
        cat "$WORK/output"
        echo "FAIL: expected string splice rejection: $diagnostic"
        exit 1
    fi
}

for name in '' '1bad' 'two words' 'a-b' 'int'; do
    printf -v source '@run { const char* name = "%s"; eject { int $(name); } };' "$name"
    reject_string_splice "$source" 'eject identifier splice requires a non-keyword identifier'
done
reject_string_splice '@run { const char* text = 0; eject { #$(text) } };' \
    'VM string splice has a null or unmapped address'
reject_string_splice '@run { const char* text = "x"; text += 2; eject { #$(text) } };' \
    'VM string splice has a null or unmapped address'
reject_string_splice '@run { int* text = 0; eject { #$(text) } };' \
    'eject string splice requires a char pointer or array'
echo 'PASS: invalid identifiers, null/out-of-range strings and non-character pointers are rejected'

status=0
timeout 30 stdbuf -o0 -e0 "$WORK/repl" > "$WORK/output" 2>&1 <<EOF || status=$?
:r $(pwd)/test_run_eject_edges.mtc
:q
EOF
if [ "$status" -ne 1 ] ||
   ! grep -qF 'semantic done: 7 decls' "$WORK/output" ||
   ! grep -qF 'Repl_Loop exited this should only happen on quit' "$WORK/output" ||
   grep -Eq 'SemanticError|Assertion|lookup failed|unsupported' "$WORK/output"; then
    cat "$WORK/output"
    echo "FAIL: @run/eject edge cases (exit $status)"
    exit 1
fi
echo 'PASS: empty buffers, buffer growth, switch cases, negative splices, generated locals and nested reparsing'

{
    printf '@run { eject { int quoted_sum() { return 0'
    for ((i=0; i<30; i++)); do printf ' + 1'; done
    printf '; } } }; @run { assert(quoted_sum() == 30); };\n'
} > "$WORK/large_quote.mtc"
status=0
timeout 30 stdbuf -o0 -e0 "$WORK/repl" > "$WORK/output" 2>&1 <<EOF || status=$?
:r $WORK/large_quote.mtc
:q
EOF
if [ "$status" -ne 1 ] ||
   ! grep -qF 'semantic done: 2 decls' "$WORK/output"; then
    cat "$WORK/output"
    echo 'FAIL: large eject token sequence'
    exit 1
fi
echo 'PASS: eject sequences exceeding the initial VM callback capacity'

sed 's/6 \* 7/6 * 8/' test_run_eject.mtc > "$WORK/wrong_answer.mtc"
status=0
timeout 30 stdbuf -o0 -e0 "$WORK/repl" > "$WORK/output" 2>&1 <<EOF || status=$?
:r $WORK/wrong_answer.mtc
:q
EOF
if [ "$status" -eq 124 ] ||
   ! grep -qF '@run execution failed' "$WORK/output" ||
   grep -qF 'semantic done:' "$WORK/output"; then
    cat "$WORK/output"
    echo 'FAIL: an incorrect generated result was not rejected'
    exit 1
fi
echo 'PASS: generated code is executed and failed compile-time assertions are reported'

sed 's/int count = 4;/int count = input;/' test_run_eject.mtc > "$WORK/capture.mtc"
status=0
timeout 30 stdbuf -o0 -e0 "$WORK/repl" > "$WORK/output" 2>&1 <<EOF || status=$?
:r $WORK/capture.mtc
:q
EOF
if [ "$status" -eq 124 ] ||
   ! grep -qF "@run cannot read enclosing runtime variable 'input'" "$WORK/output"; then
    cat "$WORK/output"
    echo 'FAIL: enclosing runtime parameter was not rejected'
    exit 1
fi
echo 'PASS: enclosing types are visible but runtime parameter values cannot be captured'
