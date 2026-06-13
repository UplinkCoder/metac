#!/bin/sh
CC=cc
$CC -o bc_dyncall.o bc_dyncall.c -I../3rdparty/libdyncall-1.4/ -ldyncall_s -L../3rdparty/libdyncall/1.4/libdyncall -c
