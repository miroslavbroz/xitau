#!/usr/bin/gnuplot

set xl "t [d]"
set yl "a_r, a_t, a_p [au d^-2]"

set xr [0:36.525]
#set xr [0:2.0]
set zeroaxis

p "<awk '/time =/{ t=$3; }/artp =/{ print t,$3; }' chi2.out" u 1:2 w p t "a_r",\
  "<awk '/time =/{ t=$3; }/artp =/{ print t,$4; }' chi2.out" u 1:2 w p t "a_t",\
  "<awk '/time =/{ t=$3; }/artp =/{ print t,$5; }' chi2.out" u 1:2 w p t "a_p"

pa -1


