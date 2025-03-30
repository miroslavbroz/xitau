#!/usr/bin/gnuplot

f(x) = x > 180. ? x - 360. : x

set xl "time [yr]"
set yl "omega_1 [deg]"

set zeroaxis

load "T0.plt"

p \
  "xvpl2el.out" u ($1-T0):(f($7)) w lp

pa -1

set term png small
set out "omega1.png"
rep

q


