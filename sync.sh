#!/bin/sh
DST="$1"
LINENOISE_DIR="3rd_party/linenoise"
if [ -d $DST ]; then
    cp README.md LICENSE \
       endian.h stdint_msvc.h compat.h \
       build.bat build.sh sync.sh run_tests.sh run_tests.bat \
       metal_lexer.c metal_lexer.h metal_parser.c \
       $DST

    chmod +x $DST/build.sh $DST/sync.sh $DST/run_tests.sh

    mkdir -p $DST/$LINENOISE_DIR
    cp  $LINENOISE_DIR/linenoise.c \
        $LINENOISE_DIR/linenoise.h \
        $LINENOISE_DIR/ocaml_community_mod.c \
        $DST/$LINENOISE_DIR

    mkdir -p $DST/cache
    cp cache/cached_tree.c cache/cached_tree.h cache/crc32.c $DST/cache
else
    echo "You need to give a target directory as argument"
fi

