#!/usr/bin/gnuplot

set term x11

set xl "JD"
set yl "mag+K"

set xr [56224.5:56224.9]
set yr [0.98:1.12] reverse

p \
  "lc.out"  u 1:3 w lp,\
  "../test_lc_old/lc.out" u 1:8 w lp,\
  "most.v4" u 1:2 w l,\

pa -1

set term png small
set out "lc_PRIMARY.png"
rep

q

