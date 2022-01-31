#!/usr/bin/gnuplot

set xl "JD"
set yl "eps [deg]"

set zeroaxis

p "test_eps.out" u (($1-2451545.)/365.25):2,\
  24.2  w l lt 0,\
  23.45 w l lt 0,\
  22.5  w l lt 0

pa -1


