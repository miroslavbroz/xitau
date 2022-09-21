#!/usr/bin/gnuplot

set xl "phase"
set yl "sr1, sr2"

set arrow from graph 0,first 8.0 rto graph 1,first 0 nohead lt 0
set arrow from graph 0,first 3.5 rto graph 1,first 0 nohead lt 0

p "<awk '/gpha/{ gpha=$4; }/sr1/{ print gpha,$4; }' chi2.out" u 1:2 w lp,\
  "<awk '/gpha/{ gpha=$4; }/sr2/{ print gpha,$4; }' chi2.out" u 1:2 w lp,\

pa -1


