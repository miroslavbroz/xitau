#!/usr/bin/gnuplot

set term x11

set xl "T_eff [K]"
set yl "log g [cgs]"

set yr [-1:]
set ytics 2

p "limcof.dat" u 2:3 w p,\
  "limcof_test.out2" u 1:(5) w p
pa -1


