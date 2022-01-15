#!/bin/sh

cd ../multipole; make test_getacc; cd ../test_getacc1_216

#rm coms.out coms_.out

./test_getacc > test_getacc.out; cat test_getacc.out

#./coms.plt
#./coms2.plt


