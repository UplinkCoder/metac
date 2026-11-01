#!/bin/sh

set -eu

if [ -z "${CC:-}" ]; then
    CC=cc
fi

DIR=$(pwd)
cd ..
./gen_code.sh
cd "$DIR"

$CC ${CFLAGS:-} web_repl.c \
    -I.. \
    -DDEBUG_SERVER=1 -DDEBUG_SERVER_LOOPBACK=1 \
    -lmicrohttpd -ljson-c -lm -lpthread \
    -o web_repl \
    "$@"