#!/usr/bin/gnuplot

set term x11

set xl "log g [cgs]"
set yl "metallicity Z [M/H]"

p "limcof.dat" u 3:4 w p,\
  "<echo 4.25 -0.25" u 1:2 w p ps 2
pa -1


