if [ -z $CC ]; then
    CC=cc
fi
pushd ..
./gen_code.sh
popd
$CC repl.c parser_obj.c -I.. \
    -g2 -O0 -lm -march=native -mtune=native -o repl
