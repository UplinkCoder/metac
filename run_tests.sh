#!/bin/sh

if [ -z $CC ]; then
    CC=cc
fi

./gen_code.sh

$CC metac_lexer.c -g3 -DTEST_LEXER -o lexer_test
./lexer_test

$CC metac_parser.c metac_alloc_node.c \
    -g3 -DACCEL=ACCEL_TABLE -DNO_DOT_PRINTER -DTEST_PARSER -o parser_test
./parser_test
