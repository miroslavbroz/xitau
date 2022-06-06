#!/usr/bin/gnuplot

set xl "iter"
set yl "theta (vector of free parameters)"
set cbl "walker"

set zeroaxis
set palette rgbformulae 33,13,10
set key left

p \
  "chain.tmp" u 1:4 w lp,\
  "chain.tmp" u 1:5 w lp,\
  "chain.tmp" u 1:6 w lp,\
  "chain.tmp" u 1:7 w lp,\
  "chain.tmp" u 1:8 w lp,\
  "chain.tmp" u 1:9 w lp,\

pa -1

set term png small
set out "chain.png"
rep

