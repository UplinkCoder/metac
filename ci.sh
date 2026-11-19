#!/bin/bash
RETVAL=0

set -euo pipefail

./gen_code.sh &&
./build_repl.sh &&
./run_tests.sh
