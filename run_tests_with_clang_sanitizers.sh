#!/bin/sh

CC=clang CFLAGS='-fsanitize=memory' ./run_tests.sh
