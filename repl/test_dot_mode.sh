#!/bin/bash
# Asserts that :dot writes a graphviz parse tree next to the source file with the expected nodes.
set -e
cd "$(dirname "$0")"
BIN=${REPL_BIN:-/tmp/replrun_dot_test}

cc linenoise_repl.c -I.. -DACCEL=ACCEL_TABLE -DOLD_PARSER=1 -DMETAC_COMPILER_INTERFACE=1 \
    -mtune=core2 -march=core2 -mstackrealign -lm -lpthread -o "$BIN" > /tmp/dot_test_build.log 2>&1

WORK=$(mktemp -d)
trap 'rm -rf "$WORK"' EXIT
SRC="$WORK/sample.mtc"
DOT="$SRC.dot"

cat > "$SRC" <<'EOF'
int g(int a) { return a; }
EOF

OUT=$(timeout 20 stdbuf -o0 -e0 "$BIN" <<EOF 2>&1
:dot $SRC
:q
EOF
) || true

if [ ! -f "$DOT" ]; then
    echo "FAIL: dot file was not written"
    echo "$OUT"
    exit 1
fi

BARE=$(timeout 20 stdbuf -o0 -e0 "$BIN" <<EOF 2>&1
:dot
:q
EOF
) || true

if ! grep -qF "usage: :dot <file>" <<< "$BARE"; then
    echo "FAIL: bare :dot did not print usage"
    echo "$BARE"
    exit 1
fi

check() {
    if ! grep -qF -- "$1" "$DOT"; then
        echo "FAIL: missing '$1' in $DOT"
        cat "$DOT"
        exit 1
    fi
}

check "digraph parsetree {"
check 'n2 [label="decl_function\ng"];'
check 'n4 [label="decl_variable\na"];'
check 'label="return"'
check 'label="param"'
check 'label="body"'
check 'stmt_return'
check 'expr_identifier\na'
check '}'

echo "PASS: :dot writes a graphviz parse tree for the source file"
