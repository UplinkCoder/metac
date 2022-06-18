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
    -I.. -DACCEL=$ACCEL \
    -g3 -O0 -march=native -mtune=native \
    -lm -lpthread -lfiber -ldl \
    -L../3rd_party/libfiber/c \
    -o repl $@
# -mtune=core2 -march=core2
