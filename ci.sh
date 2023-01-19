#!/bin/bash
RETVAL=0

set -euo pipefail

./gen_code.sh &&
./run_tests.sh &&
./build_repl.sh
