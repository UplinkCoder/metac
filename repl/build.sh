#!/bin/bash
if [ -z $CC ]; then
    CC=cc
fi
pushd ..
./gen_code.sh
popd
if [ -z $ACCEL ]; then
    ACCEL=ACCEL_TABLE
fi

$CC repl.c ../metac_parser_obj.c ../metac_driver.c ../metac_semantic_obj.c \
    -I.. -DACCEL=$ACCEL \
    -g3 -O0 -lm -march=native -mtune=native -o repl  $@
