#!/usr/bin/gnuplot

set term x11

set xl "alpha [deg]"
set yl "mu_i' [1]"
set cbl "bartheta [deg]"

set yr [:]
set zeroaxis
set palette rgbformulae 33,13,10

set arrow from 90,graph 0 rto 0,graph 1 nohead lt 0

p "<awk '!/NaN/' test_hapke.out" u 3:8:5 w lp lc palette z

pa -1

set term png small
set out "mu_i_.png"
rep

