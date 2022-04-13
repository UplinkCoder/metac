#!/bin/sh
DST="$1"
LINENOISE_DIR="3rd_party/linenoise"
if [ -d $DST ]; then
    cp README.md LICENSE \
       endian.h stdint_msvc.h compat.h int_to_str.c \
       sync.sh sync_from.sh \
       gen_code.bat gen_code.sh run_tests.sh run_tests.bat \
       metac_lexer.c metac_lexer.h metac_parser.c metac_parser.h \
       metac.fpg \
       $DST

    chmod +x $DST/gen_code.sh $DST/sync_from.sh $DST/sync.sh $DST/run_tests.sh

    mkdir -p $DST/cache
    cp cache/cached_tree.c cache/cached_tree.h cache/crc32.c cache/serialize_tree.c $DST/cache

    mkdir -p $DST/utils
    cp utils/gen_metac_match_keyword.c utils/metac_count_token_length.c \
       $DST/utils

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
