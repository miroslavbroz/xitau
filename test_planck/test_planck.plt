#!/usr/bin/gnuplot

nm = 1.e-9

set term x11
set xl "lambda [nm]"
set yl "B_lambda [J s^-1 m^-2 sr^-1 m^-1]"

p "test_planck.out" u ($1/nm):2 w lp,\
  "test_planck.out" u ($1/nm):3 w lp,\
  "test_planck.out" u ($1/nm):4 w lp

pa -1

set term png small
set out "test_planck.png"
rep


