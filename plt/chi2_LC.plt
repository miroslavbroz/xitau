#!/usr/bin/env gnuplot

load "T0.plt"

JD0 = T0
P = x_param5
P = 7.147596
P = 7.143630

jd0 = T0+0.5*P
jd1 = T0+1.0*P
jd2 = T0+1.5*P
jd3 = T0+2.0*P
E = int((jd1-jd0)/P)
E = E+1
print "E = ", E

set colors classic
#set term x11

band = 7
band = 54
shift = 0.2
shift = 0.0

set xl "JD - 2400000"
set yl "magnitude [mag]"
set cbl "JD - 2400000"

load "T0.plt"

#tmp=0.2; set xr [T0-2400000-tmp:T0-2400000+tmp]
set yr [:] reverse
#set ytics shift
set grid ytics
set key right
set mouse format "%.6f"
set palette rgbformulae 33,13,10

set arrow from T0-2400000,graph 0 rto 0,graph 1 nohead lt 0 front
#set arrow from jd0-2400000,graph 0 rto 0,graph 1 nohead lt 0 front
#set arrow from jd1-2400000,graph 0 rto 0,graph 1 nohead lt 0 front
#set arrow from jd2-2400000,graph 0 rto 0,graph 1 nohead lt 0 front
#set arrow from jd3-2400000,graph 0 rto 0,graph 1 nohead lt 0 front

# from Omc12.dat_tdb:
set arrow from 2456224.72474826-2400000,graph 0 rto 0,graph 1 nohead lt 0 front
set arrow from 2456228.30177255-2400000,graph 0 rto 0,graph 1 nohead lt 0 front
set arrow from 2456231.86899621-2400000,graph 0 rto 0,graph 1 nohead lt 0 front
set arrow from 2456235.44584543-2400000,graph 0 rto 0,graph 1 nohead lt 0 front
set arrow from 2459147.70172873-2400000,graph 0 rto 0,graph 1 nohead lt 0 front
set arrow from 2459151.27557915-2400000,graph 0 rto 0,graph 1 nohead lt 0 front
set arrow from 2459154.84385145-2400000,graph 0 rto 0,graph 1 nohead lt 0 front
set arrow from 2459161.98563354-2400000,graph 0 rto 0,graph 1 nohead lt 0 front
set arrow from 2459165.55942638-2400000,graph 0 rto 0,graph 1 nohead lt 0 front
set arrow from 2459169.12716193-2400000,graph 0 rto 0,graph 1 nohead lt 0 front

p \
  "Lc.dat"         u ($1-2400000):2:($1-2400000) w l lc palette z not,\
  "Lc.dat"         u ($1-2400000):2:3 w err lt 3 pt 1 ps 0.5 t "observed",\
  "data/LC_most12.dat_tdb"    u ($1-2400000):2:3 w err lt 3 pt 1 ps 0.5 not,\
  "data/LC_most17.dat_tdb"    u ($1-2400000):2:3 w err lt 3 pt 1 ps 0.5 not,\
  "data/LC_tess31.dat_tdb"    u ($1-2400000):2:3 w err lt 3 pt 1 ps 0.5 not,\
  "data/LC_tess42.dat_tdb"    u ($1-2400000):2:3 w err lt 3 pt 1 ps 0.5 not,\
  "data/LC_tess43.dat_tdb"    u ($1-2400000):2:3 w err lt 3 pt 1 ps 0.5 not,\
  "data/LC_tess44.dat_tdb"    u ($1-2400000):2:3 w err lt 3 pt 1 ps 0.5 not,\
  "data/LC_tess70.dat_tdb"    u ($1-2400000):2:3 w err lt 3 pt 1 ps 0.5 not,\
  "data/LC_tess71.dat_tdb"    u ($1-2400000):2:3 w err lt 3 pt 1 ps 0.5 not,\
  "Lc_U.dat"       u ($1-2400000):($2+(5-band)*shift):3 t "U" w err lt 4 pt 1 ps 0.5,\
  "Lc_B.dat"       u ($1-2400000):($2+(6-band)*shift):3 t "B" w err lt 5 pt 1 ps 0.5,\
  "Lc_V.dat"       u ($1-2400000):($2+(7-band)*shift):3 t "V" w err lt 2 pt 1 ps 0.5,\
  "lightcurve.dat" u ($1-2400000):($2+($3-band)*shift) w lp pt 1 lc 'orange' t "synthetic",\
  "chi2_LC.dat"    u ($1-2400000):($2+($4-band)*shift):3 w l lt 1 lw 3 t "residua",\

pa -1

set term png small size 2048,1024
set out "chi2_LC.png"
rep

q


  "Lc_tess.dat"    u ($1-2400000):2:3 w l lt 3 not,\

