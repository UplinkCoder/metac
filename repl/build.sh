#!/bin/sh
DIR=`pwd`

if [ -z $CC ]; then
    CC=cc
fi

cd ..
./gen_code.sh
cd $DIR

if [ -z $ACCEL ]; then
    ACCEL=ACCEL_TABLE
fi

$CC $CFLAGS linenoise_repl.c \
    -DACOSW_EXTERNAL_ASM ../3rd_party/libaco/acosw.S \
    -I.. -DACCEL=$ACCEL \
    -g3 -O0 \
    -mtune=core2 -march=core2 -mstackrealign \
    -lm -lpthread \
    -o repl $@
#-march=native -mtune=native \
