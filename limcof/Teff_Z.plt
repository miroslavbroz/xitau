#!/usr/bin/gnuplot

set term x11

set xl "T_eff [K]"
set yl "metallicity [M/H]"

p "limcof.dat" u 2:4 w p
pa -1


