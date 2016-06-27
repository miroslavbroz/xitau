#!/usr/bin/gnuplot

set term x11

set xl "lambda [nm]"
set yl "T_eff [K]"
set zl "x_lin []"

set ticslevel 0

sp "limcof.dat" u 1:2:5 w d
pa -1


