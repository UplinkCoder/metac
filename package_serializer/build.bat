pushd ..
call gen_code.bat
popd
cl /TP /I.. /DACCEL=ACCEL_TABLE package_serializer.c /Os /I.  
