#!/usr/bin/gnuplot

set term x11
set colors classic

set xl "alpha [deg]"
set yl "H0 [mag]"
set cbl "JD-2400000"

set yr [:] reverse
set palette rgbformulae 33,13,10

p \
  "lightcurve2.dat" u 5:6 lc 'orange' t "synthetic",\
  "lightcurve2.dat" u 5:7:($1-2400000) lc palette z t "observed",\

pa -1

set term png small
set out "lightcurve2.png"
rep

q

  "lightcurve2.dat_0.000" u 5:($6-0.0),\
  "lightcurve2.dat_1.276" u 5:($6+0.23),\
  "lightcurve2.dat_3.000" u 5:($6+0.47),\


