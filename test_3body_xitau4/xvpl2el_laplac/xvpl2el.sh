#!/bin/sh

#./xvpl2el < out_JDATE_heliocentric.dat > xvpl2el.out
./xvpl2el < out_JDATE_barycentric.dat > xvpl2el.out

#./orbit.plt
./varpi2_BREITER2015.plt
./Omega2_BREITER2015.plt

