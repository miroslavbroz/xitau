#!/usr/bin/gnuplot

set colors classic
set term x11

band = 7
shift = 0.0

set xl "JD - 2400000"
set yl "magnitude [mag]"

load "T0.plt"

set yr [:] reverse
#set ytics shift
set grid ytics
set key right
set mouse format "%.6f"

set arrow from T0-2400000,graph 0 rto 0,graph 1 nohead lt 0 front

p \
  "../../xitau_20221206_ECLIPSE/test_polygon1_tinytriangle/lightcurve2.dat" u ($1-2400000):($2+($3-band)*shift) w lp pt 2 lc 'gray',\
  "lightcurve2.dat" u ($1-2400000):($2+($3-band)*shift) w lp pt 1 lt 7 t "synthetic",\

pa -1

set term png small size 2048,1024
set out "chi2_LC2.png"
rep

q

  30.450906610632611 w l lt 0
  "chi2_LC2.dat"    u ($1-2400000):($2+($4-band)*shift):3 w l lt 1 lw 3 t "residua",\

  "Lc.dat"         u ($1-2400000):2:3 w err lt 3 pt 1 ps 0.5 t "observed",\
  "Lc.dat"         u ($1-2400000):2:3 w l lt 3 t "observed",\
  "Lc_tess.dat"    u ($1-2400000):2:3 w l lt 3 not,\
  "Lc_U.dat"       u ($1-2400000):($2+(5-band)*shift):3 t "U" w err lt 4 pt 1 ps 0.5,\
  "Lc_B.dat"       u ($1-2400000):($2+(6-band)*shift):3 t "B" w err lt 5 pt 1 ps 0.5,\
  "Lc_V.dat"       u ($1-2400000):($2+(7-band)*shift):3 t "V" w err lt 2 pt 1 ps 0.5,\

