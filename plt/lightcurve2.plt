#!/usr/bin/gnuplot

set term x11
set colors classic

set xl "alpha [deg]"
set yl "H0 [mag]"

set yr [:] reverse

p \
  "lightcurve2.dat_0.000" u 5:($6-0.0),\
  "lightcurve2.dat_1.276" u 5:($6+0.23),\
  "lightcurve2.dat_3.000" u 5:($6+0.47),\

pa -1

q

  "lightcurve2.dat" u 5:6 lc 'orange',\


