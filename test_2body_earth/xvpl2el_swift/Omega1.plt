#!/usr/bin/gnuplot

f(x) = x > 180. ? x - 360. : x

set xl "time [d]"
set yl "Omega_1 [deg]"

set zeroaxis
set key left

load "T0.plt"

p \
  "xvpl2el.out" u ($1-T0):(f($6)) w lp

pa -1

set term png small
set out "Omega1.png"
rep

q


