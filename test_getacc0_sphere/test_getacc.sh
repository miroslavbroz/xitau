#!/bin/sh

cd ../multipole; make test_getacc; cd ../test_getacc0_sphere

rm coms.out coms_.out

./test_getacc > test_getacc.out; less -S test_getacc.out

./coms.plt


