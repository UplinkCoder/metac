if [ -z $CC ]; then
    CC=cc
fi
pushd ..
./gen_code.sh
popd

$CC package_serializer.c -I.. \
    -g2 -O0 -march=native -mtune=native -o package_serializer $@

#g++ package_serializer.c ../metac_parser_obj.c ../3rd_party/tracy/TracyClientNoExit.o -I.. \
#    -g2 -O0 -march=native -mtune=native -lpthread -lm -ldl -o package_serializer $@
