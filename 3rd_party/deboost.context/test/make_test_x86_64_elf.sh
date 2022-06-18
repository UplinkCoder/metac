#!/bin/sh
cat ../asm/jump_x86_64_sysv_elf_gas.S ../asm/make_x86_64_sysv_elf_gas.S > x86_64_elf.S
gcc test_fcontext.c -I../include -I../include/fcontext ../source/stack.c -lm x86_64_elf.S
