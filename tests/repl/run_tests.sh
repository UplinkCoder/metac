#!/bin/sh
#
# Runs the REPL test cases.
#
# Every test case is a file in this directory.  The name encodes the mode
# the REPL has to run in:
#
#   <name>.ds.mtc   declaration semantic mode  (:ds)
#   <name>.ss.mtc   statement semantic mode   (:ss)
#   <name>.ee.mtc   expression evaluation     (:ee)
#
# The runner feeds the case to the REPL and checks that
#
#   * the REPL exits with the code it uses for a normal quit,
#   * the output does not contain an assertion failure,
#   * and every string of the companion file <name>.expect appears in the
#     output.
#
# A test case that is known to fail has a companion file <name>.xfail that
# explains why.  Such a case is reported as XFAIL when it fails and as
# XPASS (a failure of the test suite) when it unexpectedly succeeds - that
# way the marker has to be removed once the bug is fixed.
#
# Usage: run_tests.sh [repl-binary]

if [ -z $CC ]; then
    CC=cc
fi

# the directory of this script, not the current working directory
DIR=`cd \`dirname "$0"\` && pwd`
REPL=${1:-"$DIR/../../repl/repl"}

if [ ! -x "$REPL" ]; then
    echo "no repl binary at '$REPL' - skipping the repl tests"
    exit 0
fi

# The repl returns 1 when it is quit normally.
NORMAL_EXIT=1

failures=0
total=0
xfailed=0

for testcase in "$DIR"/*.mtc; do
    [ -e "$testcase" ] || continue

    name=`basename "$testcase" .mtc`
    mode=`echo "$name" | sed 's/.*\.\([a-z]*\)$/\1/'`

    case "$mode" in
        ds|ss|ee) ;;
        *)
            echo "SKIP $name (unknown mode '$mode')"
            continue
            ;;
    esac

    total=`expr $total + 1`

    output=`mktemp`
    {
        printf ':%s\n' "$mode"
        cat "$testcase"
        printf ':q\n'
    } | "$REPL" > "$output" 2>&1
    status=$?

    failed=0
    reason=""

    if [ $status -ne $NORMAL_EXIT ]; then
        reason="exit code $status, expected $NORMAL_EXIT"
        failed=1
    fi

    if grep -q "Assertion" "$output"; then
        reason="$reason; assertion failure"
        failed=1
    fi

    if [ -f "$DIR/$name.expect" ]; then
        while read -r expected; do
            [ -n "$expected" ] || continue
            if ! grep -qF "$expected" "$output"; then
                reason="$reason; missing '$expected'"
                failed=1
            fi
        done < "$DIR/$name.expect"
    fi

    if [ -f "$DIR/$name.xfail" ]; then
        if [ $failed -ne 0 ]; then
            echo "XFAIL $name ($reason)"
            xfailed=`expr $xfailed + 1`
        else
            echo "XPASS $name - the bug it was marked for seems to be fixed,"
            echo "       please remove $name.xfail"
            failures=`expr $failures + 1`
        fi
    elif [ $failed -ne 0 ]; then
        echo "FAIL $name ($reason)"
        failures=`expr $failures + 1`
        head -20 "$output" | sed 's/^/    | /'
    else
        echo "PASS $name"
    fi

    rm -f "$output"
done

echo ""
echo "$((total - failures - xfailed))/$((total - xfailed)) repl test cases passed, $xfailed known failures"

[ $failures -eq 0 ]
