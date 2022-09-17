#!/usr/bin/gnuplot

set xl "JD-2400000"
set yl "Ltheta, Lphi"

p "angmom.tmp" u ($1-2400000):3,\

pa -1

q
  "angmom.tmp" u ($1-2400000):4,\


