#!/usr/bin/gnuplot

set xl "phase"
set yl "sema"

set arrow from graph 0,first 40.085058040110539 rto graph 1,first 0 nohead lt 0

p "<awk '/gpha/' chi2.out" u 4:16 w lp,\

pa -1


