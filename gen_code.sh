#!/bin/sh

if [ -z $CC ]; then
    CC=cc
fi

if ! [ -d generated ]; then
    mkdir generated
fi

$CC utils/gen_metac_match_keyword.c -o gen_metac_match_keyword
./gen_metac_match_keyword > generated/metac_match_keyword.inl

#$CC -I. utils/gen_targetinfo.c -o gen_target_info
#./gen_target_info > generated/target_info_`uname -m`
