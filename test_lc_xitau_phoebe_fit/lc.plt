#!/usr/bin/gnuplot

set term x11

set xl "JD"
set yl "mag+K"

#set xr [56224.5:56225.0]
set yr [:] reverse

p \
  "lc.out"  u 1:3 w lp,\
  "../test_lc_old/lc.out" u 1:8 w lp,\
  "most.v4" u 1:2 w l,\

pa -1

set term png small
set out "lc.png"
rep

q

