#!/bin/sh
#
# Build the GTK3 front-end of the MetaC REPL.
#
#   ./build_gtk3.sh [extra compiler flags]
#
# Requirements: a GTK3 development package (libgtk-3-dev) and pkg-config.
#
# ---------------------------------------------------------------------------
# Building without root access
# ---------------------------------------------------------------------------
# On a machine where you cannot install packages you can unpack the
# development files into a local prefix:
#
#   mkdir -p /tmp/gtkdev && cd /tmp/gtkdev
#   apt-get download libgtk-3-dev libglib2.0-dev libpango1.0-dev \
#       libcairo2-dev libgdk-pixbuf-2.0-dev libatk1.0-dev libharfbuzz-dev \
#       libfreetype6-dev libfontconfig1-dev libx11-dev libxext-dev \
#       libxi-dev libxrender-dev libxcursor-dev libxinerama-dev \
#       libxfixes-dev libepoxy-dev libatspi2.0-dev libdbus-1-dev \
#       libfribidi-dev libpixman-1-dev uuid-dev libmount-dev libblkid-dev
#   for f in *.deb; do dpkg-deb -x "$f" root/; done
#
# then point pkg-config at it and rewrite the prefixes of the .pc files:
#
#   find root -name '*.pc' -exec sed -i \
#       -e 's|=/usr/lib/x86_64-linux-gnu|='"$PWD"'/root/usr/lib/x86_64-linux-gnu|g' \
#       -e 's|=/usr/include|='"$PWD"'/root/usr/include|g' \
#       -e 's|=/usr/share|='"$PWD"'/root/usr/share|g' {} \;
#   export PKG_CONFIG_PATH="$PWD/root/usr/lib/x86_64-linux-gnu/pkgconfig:$PWD/root/usr/share/pkgconfig"
#
# and finally build with the headers taken from the prefix but the shared
# libraries taken from the system:
#
#   ./build_gtk3.sh
#
# ---------------------------------------------------------------------------
# A note on the coroutine backend
# ---------------------------------------------------------------------------
# The build uses -DMETAC_CORO_FCONTEXT, which selects the boost.context
# based implementation of the coroutine layer (os/metac_coro_fcontext.c)
# instead of the default libaco one (3rd_party/libaco).
#
# The reason is a stack alignment problem in libaco: the stacks of the
# coroutines are not 16 byte aligned, so every call into a function that
# uses SSE instructions without an aligned stack (printf() for example)
# crashes with SIGSEGV on a current glibc.  The boost.context backend
# keeps the alignment and therefore works.
#
set -e

DIR=`pwd`
if [ -z "$CC" ]; then
    CC=cc
fi
if [ -z "$ACCEL" ]; then
    ACCEL=ACCEL_TABLE
fi

cd ../..
./gen_code.sh
cd "$DIR"

PKG_CFLAGS=`pkg-config --cflags gtk+-3.0`
PKG_LIBS=`pkg-config --libs gtk+-3.0`

# When the development files live in a local prefix the linker needs the
# plain ".so" symlinks.  They are usually in a directory called "link".
if [ -n "$GTK3_LIBDIR" ]; then
    PKG_LIBS="-L$GTK3_LIBDIR $PKG_LIBS"
fi

$CC gtk_repl.c \
    -I.. -I../../ \
    -DACCEL=$ACCEL \
    -DMETAC_COMPILER_INTERFACE \
    -DMETAC_CORO_FCONTEXT \
    $PKG_CFLAGS \
    -mtune=core2 -march=core2 -mstackrealign \
    -lm -lpthread \
    -o gtk_repl \
    -Wno-incompatible-pointer-types \
    $PKG_LIBS \
    $@

echo
echo "built $DIR/gtk_repl -- run it with ./gtk_repl"
echo
echo "A quick self test (it evaluates a small program and prints the"
echo "transcript):"
echo
echo "    ./gtk_repl --selftest"
echo
