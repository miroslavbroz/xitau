#!/usr/bin/gnuplot

km = 1.e3
au = 1.496e11
deg = pi/180.
arcsec = deg/3600.
mas = 1.e-3*arcsec

d = 4.38264413834859*au
a = 20.*km
phi = a/d

print "phi = ", phi, " rad = ", phi/arcsec, " arcsec"


