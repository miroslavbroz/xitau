#!/usr/bin/gnuplot

load "T0.plt"

JD0 = 2457733.83207
JD0 = 2458773.18865178
P = 5.732436
P = x_param4
P = x_param3

f1(x) = x > 0.0 ? x : x+1.0
frac(x) = f1(x-int(x))
phase(jd) = frac((jd-JD0)/P)

set colors classic
set term x11

band = 7
#band = 54
shift = 0.2

set xl "JD - 2400000"
set yl "magnitude [mag]"

load "T0.plt"

set yr [:] reverse
#set ytics shift
set grid ytics
set key right
set zeroaxis

set arrow from 0.5,graph 0 rto 0,graph 1 nohead lt 0
set arrow from 1.0,graph 0 rto 0,graph 1 nohead lt 0

p \
  "lightcurve.dat" u (phase($1)):($2+($3-band)*shift) w lp pt 1 lw 3 lc 'orange' t "synthetic",\
  "../test_lc1_wd_e0.0/lc.out" u 2:3 w lp pt 1 lc 'cyan' t "WD",\
  "../test_lc0_phoebe2_e0.0/forward_model.txt" u 1:(-2.5*log10($2/1.0)) w lp pt 1 lc 'green' t "Phoebe2",\

pa -1

set term png small
set out "chi2_LC_PHASE.png"
rep

q

  "chi2_LC.dat"    u (phase($1)):($2+($4-band)*shift):3 w l lt 1 lw 3 t "residua",\
  "Lc.dat"         u (phase($1)):2:3 w l lt 3 not,\
  "Lc.dat"         u (phase($1)):2:3 w err lt 3 pt 1 ps 0.5 t "observed",\
  "Lc_U.dat"       u (phase($1)):($2+(5-band)*shift):3 t "U" w err lt 4 pt 1 ps 0.5,\
  "Lc_B.dat"       u (phase($1)):($2+(6-band)*shift):3 t "B" w err lt 5 pt 1 ps 0.5,\
  "Lc_V.dat"       u (phase($1)):($2+(7-band)*shift):3 t "V" w err lt 2 pt 1 ps 0.5,\

f(flux, calibration_flux) = -2.5*log10(flux/calibration_flux)

  "../data_20220205_tess/lightcurve_tess/Lc.dat" u ($1-2400000-4832.0-2.8):(f($2, 6.8e5)) w p ps 0.5 lc 'black' t "TESS"


