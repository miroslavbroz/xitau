#!/usr/bin/gnuplot

set xl "phase"
set yl "xlg1, xlg2"

set arrow from graph 0,first 3.882 rto graph 1,first 0 nohead lt 0
set arrow from graph 0,first 4.279 rto graph 1,first 0 nohead lt 0

p "<awk '/gpha/{ gpha=$4; }/xlg1/{ print gpha,$4; }' chi2.out" u 1:2 w lp,\
  "<awk '/gpha/{ gpha=$4; }/xlg2/{ print gpha,$4; }' chi2.out" u 1:2 w lp,\

pa -1


