#!/bin/sh

if [ -z $CC ]; then
    CC=cc
fi

./gen_code.sh

$CC $CFLAGS os/test/test_arena.c os/metac_alloc.c os/os.c -g3 -o arena_test &&
./arena_test &&

$CC $CFLAGS os/test/test_arena_array.c os/os.c -g3 -o arena_array_test &&
./arena_array_test &&

$CC $CFLAGS parser/metac_alloc_node_test.c os/metac_alloc.c os/os.c -g3 -o node_alloc_test &&
./node_alloc_test &&

$CC $CFLAGS parser/metac_lexer.c hash/crc32c.c os/os.c os/metac_alloc.c -g3 -DTEST_LEXER -o lexer_test &&
./lexer_test &&

$CC $CFLAGS parser/metac_parser_obj.c driver/metac_lpp.c \
    -g3 -DNO_DOT_PRINTER -DTEST_PARSER -DNO_SEMANTIC -DNO_FIBERS -DNO_PREPROCESSOR -DOLD_PARSER=1 \
    -o parser_test &&
./parser_test &&

$CC $CFLAGS repl/completion_trie.c -g3 -DCOMP_TRIE_TEST_MAIN -o completion_trie_test &&
./completion_trie_test &&

$CC $CFLAGS repl/completion_trie_bench.c -g3 -O2 -o completion_trie_bench &&
./completion_trie_bench random 2000 2000 &&

# The repl tests need the repl binary.  It is built by repl/build.sh.
if [ -x ./repl/repl ]; then
    sh tests/repl/run_tests.sh || exit 1
fi

true
