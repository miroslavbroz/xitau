#!/usr/bin/gnuplot

AU = 1.49597870700e11  # m, from IAU 2012
pc = 648000.e0/pi*AU   # m, from IAU 2015
c = 2.99792458e8       # m/s
h = 6.626070040e-34    # J s, from 2014 CODATA
k_B = 1.38064852e-23   # J K^-1, from 2014 CODATA
R_S = 6.957e8          # m, from IAU 2015
nm = 1.e-9  # m
A = 1.e-10  # m

T_Vega = 9550.           # K, efektivni teplota Slunce
R_Vega = 2.78*R_S
d = 7.75614674629543*pc

B(lambda,T) = 2.*h*c**2/lambda**5 * 1 / (exp(h*c/(k_B*T*lambda)) - 1)

########################################################################

set term x11

set xl "lambda [A]"
set yl "irradiance [J s^-1 m^-2 m^-1]"

set xr [1000:20000]
set logscale x
set mxtics 10

p "alpha_lyr_stis_002.dat" w l,\
  4.*pi*R_Vega**2 * pi*B(x*A,T_Vega) / (4.*pi*d**2) ax x1y2 not w l lt 7

pa -1

set term png small
set out "alpha_lyr_stis_002.png"
rep


