#!/usr/bin/gnuplot

load "T0.plt"

JD0 = 2436258.0000000000
P = x_param4
P = 0.17284167583333299

f1(x) = x > 0.0 ? x : x+1.0
frac(x) = f1(x-int(x))
phase(jd) = frac((jd-JD0)/P)

set colors classic
set term x11

band = 7
band = 54
shift = 0.0

set xl "JD - 2400000"
set yl "magnitude [mag]"

load "T0.plt"

set yr [:] reverse
#set ytics shift
set grid ytics
set key right

p \
  "chi2_LC2.dat"    u (phase($1)):($2+($4-band)*shift):3 w l lt 1 lw 3 t "residua",\
  "Lc.dat"         u (phase($1)):2:3 w err lt 3 pt 1 ps 0.5 t "observed",\
  "Lc_U.dat"       u (phase($1)):($2+(5-band)*shift):3 t "U" w err lt 4 pt 1 ps 0.5,\
  "Lc_B.dat"       u (phase($1)):($2+(6-band)*shift):3 t "B" w err lt 5 pt 1 ps 0.5,\
  "Lc_V.dat"       u (phase($1)):($2+(7-band)*shift):3 t "V" w err lt 2 pt 1 ps 0.5,\
  "<awk '($1-l>0.2){ print s; }{ print; l=$1; }' lightcurve2.dat" u (phase($1)):($2+($3-band)*shift) w p pt 1 lt 7 t "synthetic"

pa -1

set term png small size 2048,1024
set out "chi2_LC2_PHASE.png"
rep

q

  "Lc.dat"         u (phase($1)):2:3 w p lt 3 t "observed",\
  "<awk '($1-l>0.01){ print s; }{ print; l=$1; }' Lc_tess.dat"    u (phase($1)):2:3 w p lt 3 not,\

f(flux, calibration_flux) = -2.5*log10(flux/calibration_flux)

  "../data_20220205_tess/lightcurve_tess/Lc.dat" u ($1-2400000-4832.0-2.8):(f($2, 6.8e5)) w p ps 0.5 lc 'black' t "TESS"


