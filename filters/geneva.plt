#!/usr/bin/gnuplot

ang = 0.1

set colors classic
set term x11

set xl "lambda [nm]"
set yl "normalized response []"

#set xr [200:15000]
#set yr [-0.1:1.1]
#set logscale x
set zeroaxis
set key spacing 0.7

p \
  "geneva_1962_u.dat"  u ($1*ang):2 t "u"   w lp lt  4,\
  "geneva_1962_b.dat"  u ($1*ang):2 t "b"   w lp lt  3,\
  "geneva_1962_b1.dat" u ($1*ang):2 t "b1"  w l  lt  4,\
  "geneva_1962_b2.dat" u ($1*ang):2 t "b2"  w l  lt  5,\
  "geneva_1962_v.dat"  u ($1*ang):2 t "v"   w lp lt  7,\
  "geneva_1962_v1.dat" u ($1*ang):2 t "v1"  w l  lt  7,\
  "geneva_1962_g.dat"  u ($1*ang):2 t "g"   w lp lt  2,\
  1 not lt 0
pa -1

set term png small
set out "geneva.png"
rep

