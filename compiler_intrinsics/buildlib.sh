#!/bin/sh

gcc metac_compiler_interface.c -fPIC -shared -Os -fno-exceptions -fno-unwind-tables -fno-asynchronous-unwind-tables -fno-stack-protector -o libcompiler.so
