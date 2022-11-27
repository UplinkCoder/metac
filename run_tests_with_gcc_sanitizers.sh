#!/bin/sh

CC=gcc CFLAGS='-fsanitize=address -fsanitize=leak -fsanitize=undefined -fno-sanitize=alignment' ./run_tests.sh
