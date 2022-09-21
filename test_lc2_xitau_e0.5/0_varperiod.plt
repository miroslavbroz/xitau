#!/usr/bin/gnuplot

set xl "phase"
set yl "varperiod"

set arrow from graph 0,first 5.732436 rto graph 1,first 0 nohead lt 0

p "<awk '/gpha/' chi2.out" u 4:19 w lp,\

pa -1


