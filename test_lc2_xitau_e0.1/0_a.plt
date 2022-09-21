#!/usr/bin/gnuplot

set xl "phase"
set yl "a"

set arrow from graph 0,first 40.085058040110539 rto graph 1,first 0 nohead lt 0

p "<awk '/gpha/{ gpha=$4; }/ A /{ print gpha,$4; }' chi2.out" u 1:2 w lp

pa -1


