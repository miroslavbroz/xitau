#!/usr/bin/gnuplot

set xl "OmegaF []"
set yl "R [a=1]"

p "test_roche.out" u 5:7 w lp,\
  "../test_roche/RocheLobe_q1.3.dat" u 10:16 w lp

pa -1

