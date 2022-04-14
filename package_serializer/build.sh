if [ -z $CC ]; then
    CC=cc
fi
pushd ..
./gen_code.sh
popd

$CC package_serializer.c -I.. \
    -g2 -Os -lm -march=native -mtune=native -o package_serializer
