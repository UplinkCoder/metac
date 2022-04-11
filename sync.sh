#!/bin/sh
DST="$1"
LINENOISE_DIR="3rd_party/linenoise"
if [ -d $DST ]; then
    cp README.md LICENSE \
       endian.h stdint_msvc.h compat.h int_to_str.c \
       build.bat build.sh sync.sh sync_from.sh run_tests.sh run_tests.bat \
       metac_lexer.c metac_lexer.h metac_parser.c metac_parser.h \
       metal.fpg \
       $DST

    chmod +x $DST/build.sh $DST/sync.sh $DST/run_tests.sh

    mkdir -p $DST/cache
    cp cache/cached_tree.c cache/cached_tree.h cache/crc32.c $DST/cache

    mkdir -p $DST/repl
    cp repl/repl.c repl/build.sh $DST/repl

    chmod +x $DST/repl/build.sh

    mkdir -p $DST/$LINENOISE_DIR
    cp  $LINENOISE_DIR/linenoise.c \
        $LINENOISE_DIR/linenoise.h \
        $LINENOISE_DIR/ocaml_community_mod.inl \
        $LINENOISE_DIR/win32_support_mod.inl \
        $DST/$LINENOISE_DIR

else
    echo "You need to give a target directory as argument"
fi
