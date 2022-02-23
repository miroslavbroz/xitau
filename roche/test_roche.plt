#!/usr/bin/gnuplot

set xl "Omega_{F} []"
set yl "R_{vol}, R_{L1} [a=1]"
set cbl "q []"

set xr [0:50]
set palette rgbformulae 33,13,10
set key at graph 0.95,graph 0.666

p "test_roche.out" u 6:8:1 w lp lc palette z,\
  "test_roche.out" u 6:4:1 w l lc 'gray',\
  "../test_roche/RocheLobe_q1.3.dat" u 10:16 w lp dt 2 lc 'gray',\
  0.5 w l lt 0 not

pa -1

