#!/bin/sh

#cd ../../xvpl2el_laplac; make; cd ../test_3body_xitau3/xvpl2el_laplac/

./xvpl2el < out_JDATE_barycentric.dat > xvpl2el.out

#./xyz.plt

#./orbit.plt
./varpi2_BREITER2015.plt
./Omega2_BREITER2015.plt

