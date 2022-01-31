#!/usr/bin/gnuplot

set xl "JD-2400000"
set yl "omega, Omega [arcsec]"

set zeroaxis
set key at graph 0.3,graph 0.7

load "T0.plt"
set arrow from T0-2400000,graph 0 rto 0,graph 1 nohead lt 0

f(x) = x>180.0 ? x-360.0 : x

p \
  "xvpl2el.out" u ($1-2400000):(f($7)*3600.) w lp t "omega",\
  "xvpl2el.out" u ($1-2400000):(f($6)*3600.) w lp t "Omega",\
  43.0 w l lt 0

pa -1

set term png small
set out "omega_PPN.png"
rep

