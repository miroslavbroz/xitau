#!/usr/bin/gnuplot

set xl "phase []"
set yl "flux []"

set zeroaxis
set arrow from 0.5,graph 0 rto 0,graph 1 nohead lt 0

p "forward_model.txt" u 1:2 w lp

pa -1


