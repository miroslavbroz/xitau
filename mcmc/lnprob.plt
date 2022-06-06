#!/usr/bin/gnuplot

set xl "iter"
set yl "posterior ln p(theta|data)"
set cbl "walker"

set xr [-1:]
set zeroaxis
set palette rgbformulae 33,13,10

p "chain.tmp" u 1:3:2 lc palette z

pa -1

set term png small
set out "lnprob.png"
rep


