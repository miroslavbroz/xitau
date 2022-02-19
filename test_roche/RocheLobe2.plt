#!/usr/bin/gnuplot

set xl "Omega_{kopal} [a^{-1}]"
set yl "R [a=1]"

set zeroaxis
set key at graph 0.8,graph 0.5

p \
  "RocheLobe_q1.3.dat" u 10:4  w l  t "RL1",\
  "RocheLobe_q1.3.dat" u 10:11 w lp t "Rfr",\
  "RocheLobe_q1.3.dat" u 10:12 w lp t "Rbk",\
  "RocheLobe_q1.3.dat" u 10:13 w lp t "Ry",\
  "RocheLobe_q1.3.dat" u 10:14 w lp t "Rz",\
  "RocheLobe_q1.3.dat" u 10:16 w lp t "R_{vol}",\
  "RocheLobe_q1.3.dat" u 10:18 w l  dt 2 t "R_{area}",\

pa -1


