#!/usr/bin/gnuplot

set colors classic
set term x11

set xl "phase"
set yl "mag"

set yr [:] reverse
set zeroaxis
set arrow from 0.5,graph 0 rto 0,graph 1 nohead lt 0

p \
  "lc.out"  u 2:3 w lp

pa -1

set term png small
set out "lc.png"
rep

q

