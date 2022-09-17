#!/usr/bin/gnuplot

set xl "t [d]"
set yl "a_x, a_y, a_z [au d^-2]"

set xr [0:36.525]
#set xr [0:2.0]
set zeroaxis

p "<awk '/time =/{ t=$3; }/axyz =/{ print t,$3; }' chi2.out" u 1:2 w p t "a_x",\
  "<awk '/time =/{ t=$3; }/axyz =/{ print t,$4; }' chi2.out" u 1:2 w p t "a_y",\
  "<awk '/time =/{ t=$3; }/axyz =/{ print t,$5; }' chi2.out" u 1:2 w p t "a_z"

pa -1


