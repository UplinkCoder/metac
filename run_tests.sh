#!/bin/sh

if [ -z $CC ]; then
    CC=cc
fi

./gen_code.sh

$CC $CFLAGS parser/metac_lexer.c hash/crc32c.c os/os.c os/metac_alloc.c -g3 -DTEST_LEXER -o lexer_test &&
./lexer_test &&

$CC $CFLAGS parser/metac_parser_obj.c driver/metac_lpp.c \
    -g3 -DNO_DOT_PRINTER -DTEST_PARSER -DNO_SEMANTIC -DNO_FIBERS -DNO_PREPROCESSOR -DOLD_PARSER=1 \
    -o parser_test &&
./parser_test &&

# the coroutine layer - once with libaco and once with boost.context
$CC $CFLAGS tests/coro_test.c -g3 -o coro_test &&
./coro_test &&

$CC $CFLAGS tests/coro_test.c -g3 -DMETAC_CORO_FCONTEXT -o coro_test_fcontext &&
./coro_test_fcontext &&

# the task system runs its tasks in coroutines - so it has to work with
# both of the coroutine backends as well
$CC $CFLAGS tests/task_system_test.c -g3 -I. -o task_system_test &&
./task_system_test &&

$CC $CFLAGS tests/task_system_test.c -g3 -I. -DMETAC_CORO_FCONTEXT -o task_system_test_fcontext &&
./task_system_test_fcontext &&

#$CC $CFLAGS repl/completion_trie.c -g3 -DCOMP_TRIE_TEST_MAIN -o completion_trie_test &&
#./completion_trie_test

true
