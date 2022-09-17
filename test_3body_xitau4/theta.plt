#!/usr/bin/gnuplot

deg = pi/180.

set xl "t [d]"
set yl "theta [deg]"

set xr [0:36.525]
#set xr [0:2.0]
set zeroaxis

p "<awk '/time =/{ t=$3; }/theta =/{ print t,$3; }' chi2.out" u 1:($2/deg) w p t "theta",\

pa -1


