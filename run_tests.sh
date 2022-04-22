#!/bin/sh

if [ -z $CC ]; then
    CC=cc
fi

$CC metac_lexer.c -g3 -DTEST_LEXER -o lexer_test
./lexer_test

$CC metac_parser.c -g3 -DACCEL=ACCEL_TABLE -DTEST_PARSER -o parser_test
./parser_test
