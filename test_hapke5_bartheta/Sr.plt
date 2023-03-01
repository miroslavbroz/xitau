#!/usr/bin/gnuplot

set term x11

set xl "alpha [deg]"
set yl "Sr [1]"
set cbl "bartheta [deg]"

set yr [0:]
set zeroaxis
set palette rgbformulae 33,13,10

p "<awk '!/NaN/' test_hapke.out" u 3:7:5 w lp lc palette z

pa -1

set term png small
set out "Sr.png"
rep

