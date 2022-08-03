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
       crc32c.c crc32c.h \
       bsr.h bsf.h \
       os.h os.c \
       metac_node.h metac_node.c \
       metac_simd.h metac_atomic.h \
       metac_lexer.c metac_lexer.h metac_parser.c metac_parser.h \
       metac_alloc.c metac_alloc.h \
       metac_alloc_node.c metac_alloc_node.h \
       metac_parsetree.c metac_parsetree.h \
       metac_printer.c metac_printer.h metac_cgen.c metac_cgen.h \
       metac_dot_printer.c metac_dot_printer.h \
       metac_sematree.h \
       metac_type.c metac_type.h \
       metac_scope.c metac_scope.h \
       metac_semantic.c metac_semantic.h \
       metac_expr_semantic.c metac_expr_semantic.h \
       metac_type_semantic.c metac_type_semantic.h \
       metac_semantic_lru.h \
       metac_type_table.c metac_type_table.h \
       metac_coro.c metac_coro.h \
       metac_task.c metac_task.h \
       test_task_queue.c \
       metac_parser_obj.c metac_semantic_obj.c \
       metac_target_info.c metac_target_info.h \
       metac_default_target_info.h \
       metac_driver.c metac_driver.h \
       metac_file.c metac_file.h \
       metac_array.c metac_array.h \
       metac_preproc.c metac_preproc.h \
       metac_lpp.c metac_lpp.h \
       metac_compiler_interface.h \
       metac_codegen.h metac_codegen.c \
       metac.fpg TODO \
       $DST

    cp semantic/handoff.c semantic/node_alloc.c \
       $DST/semantic

    chmod +x $DST/gen_code.sh $DST/sync_from.sh $DST/sync.sh $DST/run_tests.sh

    mkdir -p $DST/3rd_party
    cp 3rd_party/bsr.c 3rd_party/bsf.c 3rd_party/rwlock.h \
       $DST/3rd_party

    mkdir -p $DST/3rd_party/tinycthread
    cp 3rd_party/tinycthread/tinycthread.c \
       3rd_party/tinycthread/tinycthread.h \
       $DST/3rd_party/tinycthread

    mkdir -p $DST/3rd_party/libaco
    cp 3rd_party/libaco/acosw.S \
       3rd_party/libaco/aco.h \
       3rd_party/libaco/aco_assert_override.h \
       3rd_party/libaco/aco.c \
       $DST/3rd_party/libaco

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
       $DST/cache

    mkdir -p $DST/utils
    cp utils/kw_macros.h utils/gen_metac_keyword_keys.c \
       utils/gen_metac_match_keyword.c utils/metac_count_token_length.c \
       utils/identifier_hash.c utils/print_sizes.c utils/rec_bisect.c \
       utils/read_file.c utils/read_table.c utils/gen_targetinfo.c \
       $DST/utils

    mkdir -p $DST/repl
    cp repl/repl.c repl/repl.h repl/build.sh repl/exp_eval.c repl/exp_eval.h \
       repl/linenoise_repl.c repl/curses.c \
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
