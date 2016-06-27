#!/bin/sh

awk 'BEGIN{ print "# lambda [A] & irradiance [J s^-1 m^-2 m^-1]"; } !/^#/{ printf("%8.f  %.6e\n", $1*10., $2*1.e9+0.0); }' < NewGuey2003.txt > NewGuey2003.dat

