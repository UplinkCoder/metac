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

$CC $CFLAGS completion_trie_bench.c -g3 -O2 -o completion_trie_bench \
    -lm -lpthread &&

$CC $CFLAGS linenoise_repl.c \
    -I.. -DACCEL=$ACCEL \
    -mtune=core2 -march=core2 -mstackrealign \
    -lm -lpthread \
    -o repl \
    -Wno-incompatible-pointer-types \
     $@
#-march=native -mtune=native \
#    -g3 -O0 \

#    -DACOSW_EXTERNAL_ASM ../3rd_party/libaco/acosw.S \
