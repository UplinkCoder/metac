pushd ..
call gen_code.bat
popd
cl /TP /I.. /DACCEL=ACCEL_TABLE package_serializer.c /DWRITE_TABLE /Od /I. /I.  /Z7 /Zi /MD /D_SILENCE_IVEC_C4799
