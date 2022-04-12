if [ -z $CC ]; then
    CC=cc
fi
$CC repl.c -I.. ../cache/cached_tree.c ../metac_lexer.c ../metac_parser.c \
    -g2 -Os -lm -o repl
