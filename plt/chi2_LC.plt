#!/usr/bin/gnuplot

set term x11

band = 7
shift = 0.2

set xl "JD - 2400000"
set yl "magnitude [mag]"

load "T0.plt"

set yr [:] reverse
#set ytics shift
set grid ytics
set key left

set arrow from T0-2400000,graph 0 to T0-2400000,graph 1 nohead lt 0

p \
  "chi2_LC.dat"    u ($1-2400000):($2+($4-band)*shift):3 t "residua"              w l lt 1 lw 3,\
  "Lc.dat"         u ($1-2400000):2:3                    not                      w l lt 3,\
  "Lc.dat"         u ($1-2400000):2:3                    t "observed lightcurve"  w err lt 3 pt 1 ps 0.5,\
  "Lc_U.dat"       u ($1-2400000):($2+(5-band)*shift):3 t "U" w err lt 4 pt 1 ps 0.5,\
  "Lc_B.dat"       u ($1-2400000):($2+(6-band)*shift):3 t "B" w err lt 5 pt 1 ps 0.5,\
  "Lc_V.dat"       u ($1-2400000):($2+(7-band)*shift):3 t "V" w err lt 2 pt 1 ps 0.5,\
  "lightcurve.dat" u ($1-2400000):($2+($3-band)*shift)   t "synthetic lightcurve" w lp pt 1 lt 7
pa -1

set term png small
set out "chi2_LC.png"
rep

q


