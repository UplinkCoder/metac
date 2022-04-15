if [ -z $CC ]; then
    CC=cc
fi
pushd ..
./gen_code.sh
popd

$CC package_serializer.c ../metac_parser_obj.c -I.. \
    -g2 -O0 -lm -o package_serializer $@
