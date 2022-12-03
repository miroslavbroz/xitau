#!/usr/bin/gnuplot

set term x11

set xl "alpha [deg]"
set yl "f_{hapke} [sr^-1]"
set cbl "A_w [1]"

set yr [0:]
set zeroaxis
set palette rgbformulae 33,13,10

p "test_hapke.out" u 3:6:5 w lp lc palette z

pa -1

set term png small
set out "test_hapke.png"
rep

