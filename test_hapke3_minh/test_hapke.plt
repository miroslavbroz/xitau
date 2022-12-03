#!/usr/bin/gnuplot

set term x11

set xl "alpha [deg]"
set yl "f_{hapke} [sr^-1]"
set cbl "minh [1]"

set yr [0:]
set zeroaxis
set palette rgbformulae 33,13,10

set arrow from 13,graph 0 rto 0,graph 1 nohead lt 0
set arrow from 20,graph 0 rto 0,graph 1 nohead lt 0

p "test_hapke.out" u 3:6:5 w lp lc palette z

pa -1

set term png small
set out "test_hapke.png"
rep

