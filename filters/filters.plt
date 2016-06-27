#!/usr/bin/gnuplot

ang = 0.1

set term x11

set xl "lambda [nm]"
set yl "normalized response []"

set xr [200:15000]
set yr [-0.1:1.1]
set logscale x
set zeroaxis
set key spacing 0.7

p \
  "johnson.U" u ($1*ang):2 t "U" w lp lt  4,\
  "johnson.B" u ($1*ang):2 t "B" w lp lt  3,\
  "johnson.V" u ($1*ang):2 t "V" w lp lt  2,\
  "johnson.R" u ($1*ang):2 t "R" w lp lt  1,\
  "cousins.R" u ($1*ang):2 not   w l  lt  1,\
  "johnson.I" u ($1*ang):2 t "I" w lp lt  5,\
  "cousins.I" u ($1*ang):2 not   w l  lt  5,\
  "johnson.J" u ($1*ang):2 t "J" w lp lt  6,\
  "johnson.K" u ($1*ang):2 t "K" w lp lt  7,\
  "johnson.L" u ($1*ang):2 t "L" w lp lt  8,\
  "johnson.M" u ($1*ang):2 t "M" w lp lt  9,\
  "johnson.N" u ($1*ang):2 t "N" w lp lt 10,\
  1 not lt 0
pa -1

set term png small
set out "filters.png"


