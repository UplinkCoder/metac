#!/bin/bash
# Asserts that a typedef of enum E declared in :ds mode makes typeof(E) evaluate to a type under :ee.
set -e
cd "$(dirname "$0")"
BIN=${REPL_BIN:-/tmp/replrun_enum_test}

cc linenoise_repl.c -I.. -DACCEL=ACCEL_TABLE -DOLD_PARSER=1 -DMETAC_COMPILER_INTERFACE=1 \
    -mtune=core2 -march=core2 -mstackrealign -lm -lpthread -o "$BIN" > /tmp/enum_test_build.log 2>&1

OUT=$(timeout 20 stdbuf -o0 -e0 "$BIN" <<'EOF' 2>&1
:ds
typedef enum E { one, two, three } E;
:ee
typeof(E)
:q
EOF
) || true

if echo "$OUT" | grep -q "lookup failed"; then
    echo "FAIL: identifier lookup failed"
    echo "$OUT"
    exit 1
fi
if ! echo "$OUT" | grep -q "^typeof *(E) = (typedef enum E E)"; then
    echo "FAIL: typeof(E) did not evaluate to the typedef of enum E"
    echo "$OUT"
    exit 1
fi
echo "PASS: typeof(E) evaluates to a type after :ds typedef enum E"
