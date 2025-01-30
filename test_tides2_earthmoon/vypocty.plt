#!/usr/bin/gnuplot

R_S = 6.957e8  # m; from IAU 2015
R_E = 6.378173e6  # m; WGS-82 http://wiki.gis.com/wiki/index.php/Reference_ellipsoid 
R_M = 1.7375e6  # m

print "R_E = ", R_E/R_S, " R_S"
print "R_M = ", R_M/R_S, " R_S"

day = 86400.

P1 = 1.*day
P2 = 27.33*day

omega1 = 2.*pi/P1
omega2 = 2.*pi/P2

print "omega1 = ", omega1 / (day**-1), " rad d^-1"
print "omega2 = ", omega2 / (day**-1), " rad d^-1"

R1 = R_E
R2 = R_M

v1 = 2.*pi*R1/P1
v2 = 2.*pi*R2/P2

print "v1 = ", v1/1.e3, " km s^-1"
print "v2 = ", v2/1.e3, " km s^-1"

