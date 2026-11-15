#!/bin/bash
RETVAL=0

set -euo pipefail

./gen_code.sh &&
./run_tests.sh &&
./build_repl.sh &&
# the browser game of the MetaC subset (only if node is around)
if command -v node > /dev/null 2>&1; then
    node website/game/test.js
fi
