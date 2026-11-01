#!/bin/sh

set -eu

SCRIPT_DIR=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
ROOT_DIR=$(dirname "$SCRIPT_DIR")
LIGHTNING="lightning-${LIGHTNING_VERSION:-2.1.3}"
LIGHTNING_LIBS="$ROOT_DIR/3rd_party/lightning/$LIGHTNING/lib/.libs"
SO_FILE="$LIGHTNING_LIBS/liblightning.so"

if ! test -f "$SO_FILE"; then
    ARCHIVE="$SCRIPT_DIR/$LIGHTNING.tar.gz"
    if test -f "$ROOT_DIR/libinterpret/$LIGHTNING.tar.gz"; then
        ARCHIVE="$ROOT_DIR/libinterpret/$LIGHTNING.tar.gz"
    elif test -f "$ROOT_DIR/$LIGHTNING.tar.gz"; then
        ARCHIVE="$ROOT_DIR/$LIGHTNING.tar.gz"
    fi

    if ! test -f "$ARCHIVE"; then
        wget -O "$ARCHIVE" "https://ftp.gnu.org/gnu/lightning/$LIGHTNING.tar.gz"
    fi

    mkdir -p "$ROOT_DIR/3rd_party/lightning"
    tar -xf "$ARCHIVE" -C "$ROOT_DIR/3rd_party/lightning"

    cd "$ROOT_DIR/3rd_party/lightning/$LIGHTNING"
    CFLAGS="${CFLAGS:-} -g3" ./configure --enable-disassembler
    make

fi

cd "$SCRIPT_DIR"
./build.sh -DBC_LIGHTNING ../libinterpret/bc_lightning_backend.c \
    -L"$LIGHTNING_LIBS" \
    -llightning -DMETAC_COMPILER_INTERFACE \
    -I"$ROOT_DIR/3rd_party/lightning/$LIGHTNING/include" \
    -Wl,-rpath,"$LIGHTNING_LIBS" \
    "$@"
