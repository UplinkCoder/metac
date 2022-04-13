if [ -z $CC ]; then
    CC=cc
fi
pushd ..
./gen_code.sh
popd
$CC repl.c -I.. ../cache/cached_tree.c ../metac_lexer.c ../metac_parser.c \
    -g2 -Os -lm -march=native -mtune=native -o repl
