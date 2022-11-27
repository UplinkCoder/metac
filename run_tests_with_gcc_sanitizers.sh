#!/bin/sh

CC=gcc CFLAGS='-fsanitize=address -fsanitize=leak -fsanitize=undefined' ./run_tests.sh
