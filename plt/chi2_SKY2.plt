#!/usr/bin/gnuplot

deg = pi/180.
arcsec = deg/3600.
au = 1.49597870700e11  # m, from IAU 2012
pc = 648000.0/pi*au  # m, from IAU 2015

set colors classic
set term x11

set xl "u [arcsec]"
set yl "v [arcsec]"

tmp=0.035
set xr [-tmp:tmp]
set yr [-tmp:tmp]
set size ratio -1
set zeroaxis
set ang rad
set key left

load "T0.plt"

#d_pc = x_param44
d_pc = x_param45
f(x) = x*au/(d_pc*pc)/arcsec

p \
  "<awk '($2==-1){ x=$3; y=$4; }($2==-2){ print $3-x,$4-y; }' out_JDATE_heliocentric.dat" u (f($1)):(f($2)) t "2-1" w l lt 2,\
  "<awk '($2==-1){ x=$3; y=$4; }($2==-3){ print $3-x,$4-y; }' out_JDATE_heliocentric.dat" u (f($1)):(f($2)) t "3-1" w l lt 3,\
  sprintf("<awk '($2==-1){ x=$3; y=$4; }($2==-3) && ($1==%.10f){ print $3-x,$4-y; }' out_JDATE_heliocentric.dat", T0) u (f($1)):(f($2)) t "T_0" w p lc 0 pt 1 ps 2,\
  "<./ellipses.awk arcsec_AU2.dat" u (f($1)):(f($2)) not w l lt 7,\
  "chi2_SKY2.dat" u 2:3 t "residua" w l lw 3 lc 'red',\
  

pa -1

set term png small size 1024,1024
set out "chi2_SKY2.png"
rep

q


  "<awk '($2==-1){ x=$3; y=$4; }($2==-4){ print $3-x,$4-y; }' out_JDATE_heliocentric.dat" u (f($1)):(f($2)) t "4-1" w l lt 4,\


