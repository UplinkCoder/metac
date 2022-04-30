#!/bin/sh
DST="$1"
LINENOISE_DIR="3rd_party/linenoise"
TRACY_DIR="3rd_party/tracy"
if [ -d $DST ]; then
    cp README.md LICENSE \
       metac_identifier_table.c metac_identifier_table.h \
       endian.h stdint_msvc.h compat.h int_to_str.c \
       sync.sh sync_from.sh \
       gen_code.bat gen_code.sh run_tests.sh run_tests.bat \
       metac_lexer.c metac_lexer.h metac_parser.c metac_parser.h \
       metac_printer.c metac_printer.h metac_cgen.c metac_cgen.h \
       metac_parser_obj.c \
       metac.fpg TODO \
       $DST

    chmod +x $DST/gen_code.sh $DST/sync_from.sh $DST/sync.sh $DST/run_tests.sh

    mkdir -p $DST/$TRACY_DIR
    cp $TRACY_DIR/TracyC.h \
       $DST/$TRACY_DIR

    mkdir -p $DST/$TRACY_DIR/common
    cp $TRACY_DIR/common/TracyApi.h \
       $DST/$TRACY_DIR/common

    mkdir -p $DST/$TRACY_DIR/client
    cp $TRACY_DIR/client/TracyCallstack.h \
       $DST/$TRACY_DIR/client

    mkdir -p $DST/cache
    cp cache/cached_tree.c cache/cached_tree.h \
       cache/crc32.c cache/serialize_tree.c \
       $DST/cache

    mkdir -p $DST/utils
    cp utils/gen_metac_match_keyword.c utils/metac_count_token_length.c \
       utils/identifier_hash.c utils/print_sizes.c utils/rec_bisect.c \
       utils/read_file.c utils/read_table.c \
       $DST/utils

    mkdir -p $DST/repl
    cp repl/repl.c repl/build.sh repl/exp_eval.c repl/exp_eval.h \
       $DST/repl

    mkdir -p $DST/libinterpret
    cp libinterpret/backend_interface_funcs.h \
       libinterpret/bc_common.h \
       libinterpret/bc_common.c \
       libinterpret/bc_interpreter_backend.h \
       libinterpret/bc_interpreter_backend.c \
       $DST/libinterpret


    mkdir -p $DST/package_serializer
    cp package_serializer/build.sh package_serializer/build.bat \
       package_serializer/package_serializer.c \
       $DST/package_serializer

    chmod +x $DST/repl/build.sh

    mkdir -p $DST/$LINENOISE_DIR
    cp $LINENOISE_DIR/linenoise.c \
       $LINENOISE_DIR/linenoise.h \
       $LINENOISE_DIR/ocaml_community_mod.inl \
       $LINENOISE_DIR/win32_support_mod.inl \
       $DST/$LINENOISE_DIR

else
    echo "You need to give a target directory as argument"
fi
