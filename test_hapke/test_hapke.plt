#!/usr/bin/gnuplot

set term x11

set xl "alpha [deg]"
set yl "f_{hapke} [sr^-1]"

set yr [0:]
set zeroaxis

p "test_hapke.out" u 3:6 w lp

pa -1

set term png small
set out "test_hapke.png"
rep

