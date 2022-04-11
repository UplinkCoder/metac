if [ -z $CC ]; then
    CC=cc
fi
$CC repl.c -I.. ../cache/cached_tree.c ../metal_lexer.c ../metal_parser.c \
    ../metal_lexer_toChars.c \
    -g2 -Os -o repl -lm
