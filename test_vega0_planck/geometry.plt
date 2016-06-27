#!/usr/bin/gnuplot

deg = pi/180.
arcsec = deg/3600.
mas = arcsec/1.e3

AU = 1.49597870700e11  # m, from IAU 2012
pc = 648000.e0/pi*AU  # m, from IAU 2015
R_S = 6.957e8  # m, from IAU 2015

pi_hip = 128.93*mas
d = AU/tan(pi_hip)

print "d = ", d/pc, " pc"

R_equ = 2.78*R_S
R_pol = 2.26*R_S

R_mean = (R_equ+R_pol)/2.

print "R_mean = ", R_mean/R_S, " R_S"

T_pole = 10150.  # K



