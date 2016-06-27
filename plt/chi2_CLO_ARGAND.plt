#!/usr/bin/gnuplot

set term x11

deg = pi/180.

set xl "T3 real part"
set yl "T3 imaginary part"

set polar
set zeroaxis
set size ratio -1

tmp=1.5
set xr [-tmp:tmp]
set yr [-tmp:tmp]

p \
  "Closure.dat"      u ($10*deg):8:12  t "observed T3"  w p ps 0.75 lc var,\
  "closurephase.dat" u 8:7             t "synthetic T3" w p ps 0.75

pa -1

set term png small
set out "chi2_CLO_ARGAND.png"
rep


