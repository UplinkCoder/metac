#!/bin/sh

if [ -z $CC ]; then
    CC=cc
fi

./gen_code.sh

$CC metac_lexer.c crc32c.c -g3 -DTEST_LEXER -o lexer_test
./lexer_test

$CC metac_parser_obj.c \
    -g3 -DACCEL=ACCEL_TABLE -DNO_DOT_PRINTER -DTEST_PARSER -DNO_SEMANTIC -DNO_FIBERS -DNO_FILE -DNO_PREPROC \
    -o parser_test
./parser_test
