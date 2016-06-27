#!/usr/bin/gnuplot

set term x11

nm = 1.e-9  # m
ang = 1.e-10  # m
shift = 0.0

set xl "lambda [nm]"
set yl "F_lambda [erg s^-1 cm^-2 A^-1]"

set xr [200:3000]
set logscale xy

p "1.abs" u ($1*ang/nm):($2+0*shift) t "1" w l,\
  "2.abs" u ($1*ang/nm):($2+1*shift) t "2" w l,\
  "3.abs" u ($1*ang/nm):($2+2*shift) t "3" w l,\
  "4.abs" u ($1*ang/nm):($2+3*shift) t "4" w l
pa -1

set term png small
set out "abs.png"
rep


