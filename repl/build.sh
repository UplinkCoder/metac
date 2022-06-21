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

$CC repl.c \
    -DACOSW_EXTERNAL_ASM ../3rd_party/libaco/acosw.S \
    -I.. -DACCEL=$ACCEL ../metac_coro.c ../metac_task.c \
    -g3 -O0 -march=native -mtune=native \
    -lm -lpthread \
    -o repl $@
# -mtune=core2 -march=core2
