#!/bin/sh

if [ -z $CC ]; then
    CC=cc
fi

if ! [ -d generated ]; then
    mkdir generated
fi

$CC utils/gen_metac_keyword_keys.c -o gen_metac_keyword_keys
./gen_metac_keyword_keys > generated/metac_keyword_keys.h

$CC utils/gen_metac_match_keyword.c -o gen_metac_match_keyword
./gen_metac_match_keyword > generated/metac_match_keyword.inl
