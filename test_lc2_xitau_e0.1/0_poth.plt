#!/usr/bin/gnuplot

set xl "phase"
set yl "poth, potc"

p "<awk '/gpha/' chi2.out" u 4:10 w lp,\
  "<awk '/gpha/' chi2.out" u 4:13 w lp,\

pa -1


