if [ -z $CC ]; then
    CC=cc
fi
pushd ..
./gen_code.sh
popd

if [ -z $ACCEL ]; then
    ACCEL=ACCEL_TABLE
fi

#echo $CC package_serializer.c -DACCEL=$ACCEL -DWRITE_TABLE ../os/metac_task.c \
#    -g2 -O0 -march=native -mtune=native -o package_serializer $@

#$CC package_serializer.c \
#    -DACCEL=$ACCEL -DWRITE_TABLE ../os/metac_task.c \
#    -g2 -O0 -march=native -mtune=native -o package_serializer -lpthread $@

#    ../3rd_party/deboost.context/fcontext_aarch64.o \
#    -I../3rd_party/deboost.context/include/fcontext \
#    ../3rd_party/deboost.context/source/stack.c \

g++ package_serializer.c -DACCEL=$ACCEL -DWRITE_TABLE ../os/metac_task.c \
    ~/dev/tracy/TracyClientNoExit.o -I.. -DENABLE_TRACY=1 -DTRACY_ENABLE=1 \
    -g2 -O0 -march=native -mtune=native -lpthread -lm -ldl -o package_serializer $@
