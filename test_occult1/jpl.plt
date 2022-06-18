#!/usr/bin/gnuplot

set xl "RA [deg]"
set yl "DE [deg]"

p "jpl.out_1day" u 4:5 w lp,\
  "jpl.out_1hour" u 4:5 w lp

pa -1

