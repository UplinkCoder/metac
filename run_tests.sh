#!/bin/sh

if [ -z $CC ]; then
    CC=cc
fi

$CC metac_lexer.c -g3 -DTEST_LEXER -o lexer_test
./lexer_test
