#!/bin/bash

set -euo pipefail

./gen_code.sh
./run_tests.sh
./build_repl.sh
