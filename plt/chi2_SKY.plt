#!/usr/bin/gnuplot

set term x11

set xl "x [AU]"
set yl "y [AU]"
set size ratio -1

tmp=60.
#tmp=2.
set xr [-tmp:tmp]
set yr [-tmp:tmp]
set zeroaxis

load "T0.plt"

p \
  "<awk '($2==-1)' out_JDATE_photocentric.dat"  u 3:4 t "1" w l lt 1,\
  "<awk '($2==-2)' out_JDATE_photocentric.dat"  u 3:4 t "2" w l lt 2,\
  "<awk '($2==-3)' out_JDATE_photocentric.dat"  u 3:4 t "3" w l lt 3,\
  "<awk '($2==-4)' out_JDATE_photocentric3.dat" u 3:4 t "4" w l lt 4,\
  sprintf("<awk '($2==-1) && ($1==%.10f)' out_JDATE_photocentric.dat", T0)  u 3:4 t "T_0" w p lc 0 pt 1 ps 2,\
  sprintf("<awk '($2==-2) && ($1==%.10f)' out_JDATE_photocentric.dat", T0)  u 3:4 not     w p lc 0 pt 1 ps 2,\
  sprintf("<awk '($2==-3) && ($1==%.10f)' out_JDATE_photocentric.dat", T0)  u 3:4 not     w p lc 0 pt 1 ps 2,\
  sprintf("<awk '($2==-4) && ($1==%.10f)' out_JDATE_photocentric3.dat", T0) u 3:4 not     w p lc 0 pt 1 ps 2,\
  "arcsec_AU.dat" u 2:3 t "observ." w p lt 7 pt 1 ps 0.5,\
  "<./ellipses.awk arcsec_AU.dat" u 1:2 not w l lt 7,\
  "chi2_SKY.dat" u 2:3 t "residua" w l lt 1 lw 3
pa -1

set term png small
set out "chi2_SKY.png"
rep

q


