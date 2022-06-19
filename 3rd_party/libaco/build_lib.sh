#!/bin/sh
makecc=cc
if [ "$CC" ]
then
    makecc="$CC"
fi
cc aco.c acosw.S -c $@
ar rcs libaco.a aco.o acosw.o
