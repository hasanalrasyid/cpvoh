#!/bin/bash
gfortran -fPIC -c foreign/test.f90 -o foreign/test.o
# gfortran -shared -o foreign/libtest.so foreign/test.o
stack build hascpvo:exe:test
