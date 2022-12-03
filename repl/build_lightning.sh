#!/bin/sh

LIGHTNING="lightning-2.1.3"
LIGHTNING_LIBS="../3rd_party/lightning/$LIGHTNING/lib/.libs"
SO_FILE="$LIGHTNING_LIBS/liblightning.so"

if ! test -f $SO_FILE ; then

    if ! test -f $LIGHTNING.tar.gz ; then
        wget https://ftp.gnu.org/gnu/lightning/$LIGHTNING.tar.gz
    fi

    mkdir -p ../3rd_party/lightning
    tar -xf $LIGHTNING.tar.gz -C ../3rd_party/lightning

    cd ../3rd_party/lightning/$LIGHTNING
    CFLAGS="$CFLAGS -g3" ./configure --enable-disassembler && make
    cd ../../../repl

fi

./build.sh -DBC_LIGHTNING ../libinterpret/bc_lightning_backend.c \
    -L$LIGHTNING_LIBS \
    -llightning -DMETAC_COMPILER_INTERFACE \
    -I../3rd_party/lightning/include \
    -Wl,-rpath,./ \
    $@

LIB_NAME=$(ldd repl | grep liblightning | cut -d ' ' -f 1)

if test -f $LIB_NAME; then
    rm $LIB_NAME
fi

ln -s $SO_FILE $LIB_NAME
