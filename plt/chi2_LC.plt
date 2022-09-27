#!/usr/bin/gnuplot

P = 5.998692
jd0 = 2449425.0844    # Mayer etal., Eq. (2)
jd1 = 2458573.048     # determination from LC, see ../data_20200208_tess/
jd2 = 2458573.064100  # Zasche
E = int((jd1-jd0)/P)
E = E+1
print "E = ", E

set colors classic
set term x11

band = 7
band = 54
shift = 0.2
shift = 0.0

set xl "JD - 2400000"
set yl "magnitude [mag]"

load "T0.plt"

set yr [:] reverse
#set ytics shift
set grid ytics
set key right

set arrow from T0-2400000,graph 0 rto 0,graph 1 nohead lt 0 front
set arrow from jd0+P*E-2400000,graph 0 rto 0,graph 1 nohead lt 0 front
set arrow from jd1-2400000,graph 0 rto 0,graph 1 nohead lt 1 lc 'cyan' front
set arrow from jd2-2400000,graph 0 rto 0,graph 1 nohead lt 1 lc 'green' front

p \
  "chi2_LC.dat"    u ($1-2400000):($2+($4-band)*shift):3 w l lt 1 lw 3 t "residua",\
  "Lc.dat"         u ($1-2400000):2:3 w l lt 3 t "observed",\
  "Lc_tess.dat"    u ($1-2400000):2:3 w l lt 3 not,\
  "Lc_U.dat"       u ($1-2400000):($2+(5-band)*shift):3 t "U" w err lt 4 pt 1 ps 0.5,\
  "Lc_B.dat"       u ($1-2400000):($2+(6-band)*shift):3 t "B" w err lt 5 pt 1 ps 0.5,\
  "Lc_V.dat"       u ($1-2400000):($2+(7-band)*shift):3 t "V" w err lt 2 pt 1 ps 0.5,\
  "lightcurve.dat" u ($1-2400000):($2+($3-band)*shift) w lp pt 1 lt 7 t "synthetic"

pa -1

set term png small size 1536,1024
set out "chi2_LC.png"
rep

q

  "Lc.dat"         u ($1-2400000):2:3 w err lt 3 pt 1 ps 0.5 t "observed",\

