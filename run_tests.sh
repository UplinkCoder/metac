#!/bin/sh

if [ -z $CC ]; then
    CC=cc
fi

./gen_code.sh

$CC parser/metac_lexer.c hash/crc32c.c -g3 -DTEST_LEXER -o lexer_test
./lexer_test

$CC parser/metac_parser_obj.c driver/metac_lpp.c \
    -g3 -DNO_DOT_PRINTER -DTEST_PARSER -DNO_SEMANTIC -DNO_FIBERS -DNO_FILE -DNO_PREPROCESSOR \
    -o parser_test
./parser_test
