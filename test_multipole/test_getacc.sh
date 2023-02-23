#!/bin/sh

make test_getacc

./test_getacc > test_getacc.out; less -S test_getacc.out

#./coms.plt


