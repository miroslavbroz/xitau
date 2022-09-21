#!/usr/bin/gnuplot

set xl "phase"
set yl "dist"

p "<awk '/gpha/' chi2.out" u 4:7 w lp,\

pa -1


