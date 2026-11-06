#!/bin/bash
# preprocessor harness test driver:
#   builds preproc_test, then for every test_*.c compares the harness
#   output (whitespace-stripped) against gcc -E -P, using -nostdinc and a
#   stub include dir for <stdio.h>
set -u
cd "$(dirname "$0")"

mkdir -p preprocessed_results /tmp/opencode/stubinc
: > /tmp/opencode/stubinc/stdio.h

cc -I../../parser -DNO_DOT_PRINTER -DNO_SEMANTIC -DNO_FIBERS -DOLD_PARSER=1 \
   preproc_test.c -o preprocessed_results/preproc_test || exit 1

fail=0
for file in test_*.c; do
    name="${file%.c}"
    gcc -E -P -nostdinc -I. -I/tmp/opencode/stubinc "$file" \
        -o "preprocessed_results/$name.gcc" || { echo "gcc failed on $file"; fail=1; }

    ./preprocessed_results/preproc_test "$file" > "preprocessed_results/$name.metac" || { echo "harness errored on $file"; fail=1; }

    if diff <(tr -d '[:space:]' < "preprocessed_results/$name.gcc") \
            <(tr -d '[:space:]' < "preprocessed_results/$name.metac") > /dev/null; then
        echo "PASS $name"
    else
        echo "FAIL $name"
        fail=1
    fi
done

exit $fail
