#!/usr/bin/gnuplot

set xl "R_{vol} [a=1]"
set yl "Omega_{kopal} [a^{-1}]"

set zeroaxis
set key left

p \
  "RocheLobe_q1.3.dat"   u 16:10      w lp,\
  "RocheLobe_q1.3.dat"   u  4:10      w l ,\
  "RocheLobe_q0.769.dat" u 16:10      w lp,\
  "RocheLobe_q0.769.dat" u  4:10      w l ,\

pa -1


