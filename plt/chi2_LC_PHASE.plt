#!/usr/bin/env gnuplot

load "T0.plt"

JD0 = T0
P = x_param5
P = 7.147104
#P = 7.146604

f1(x) = x > 0.0 ? x : x+1.0
frac(x) = f1(x-int(x))
phase(jd) = frac((jd-JD0)/P)

set colors classic
#set term x11

band = 7
band = 54
shift = 0.0

set xl "JD - 2400000"
set yl "magnitude [mag]"
set cbl "JD - 2400000"

load "T0.plt"

set xr [-0.1:1.1]
set yr [:] reverse
set xtics 0.1
#set yr [1.12:0.98]
#set ytics shift
set grid ytics
set key right
set palette rgbformulae 33,13,10

set arrow from 0.0,graph 0 rto 0.0,graph 1 nohead lt 0
set arrow from 0.5,graph 0 rto 0.0,graph 1 nohead lt 0
set arrow from 1.0,graph 0 rto 0.0,graph 1 nohead lt 0

p \
  "chi2_LC.dat"    u (phase($1)):($2+($4-band)*shift):3 w l lt 1 lw 3 t "residua",\
  "<awk '($1-l>0.01){ print s; }{ print; l=$1; }' Lc.dat"         u (phase($1)):2:($1-2400000) w l lc palette z t "observed",\
  "<awk '($1-l>0.01){ print s; }{ print; l=$1; }' Lc_tess.dat"    u (phase($1)):2:3 w l lt 3 not,\
  "data/LC_most12.dat_tdb"         u (phase($1)):2:3 w err lt 3 pt 1 ps 0.5 not,\
  "data/LC_most17.dat_tdb"         u (phase($1)):2:3 w err lt 3 pt 1 ps 0.5 not,\
  "data/LC_tess31.dat_tdb"         u (phase($1)):2:3 w err lt 3 pt 1 ps 0.5 not,\
  "data/LC_tess42.dat_tdb"         u (phase($1)):2:3 w err lt 3 pt 1 ps 0.5 not,\
  "data/LC_tess43.dat_tdb"         u (phase($1)):2:3 w err lt 3 pt 1 ps 0.5 not,\
  "data/LC_tess44.dat_tdb"         u (phase($1)):2:3 w err lt 3 pt 1 ps 0.5 not,\
  "data/LC_tess70.dat_tdb"         u (phase($1)):2:3 w err lt 3 pt 1 ps 0.5 not,\
  "data/LC_tess71.dat_tdb"         u (phase($1)):2:3 w err lt 3 pt 1 ps 0.5 not,\
  "Lc_U.dat"       u (phase($1)):($2+(5-band)*shift):3 t "U" w err lt 4 pt 1 ps 0.5,\
  "Lc_B.dat"       u (phase($1)):($2+(6-band)*shift):3 t "B" w err lt 5 pt 1 ps 0.5,\
  "Lc_V.dat"       u (phase($1)):($2+(7-band)*shift):3 t "V" w err lt 2 pt 1 ps 0.5,\
  "<awk '($1-l>0.2){ print s; }{ print; l=$1; }' lightcurve.dat" u (phase($1)):($2+($3-band)*shift) w lp pt 1 lt 7 t "synthetic"

pa -1

set term png small size 2048,1024
set out "chi2_LC_PHASE.png"
rep

q


f(flux, calibration_flux) = -2.5*log10(flux/calibration_flux)

  "../data_20220205_tess/lightcurve_tess/Lc.dat" u ($1-2400000-4832.0-2.8):(f($2, 6.8e5)) w p ps 0.5 lc 'black' t "TESS"


