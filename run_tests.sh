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

$CC $CFLAGS repl/completion_trie.c os/os.c os/metac_alloc.c -g3 -DTEST_MAIN -o completion_trie_test &&
./completion_trie_test
