#!/usr/bin/gnuplot

deg = pi/180.
arcsec = deg/3600.
au = 1.49597870700e11  # m, from IAU 2012
pc = 648000.0/pi*au  # m, from IAU 2015

set colors classic
set term post eps enh color dashed
set out "chi2_SKY2.eps"
set size 0.665,0.9

set xl "{/Helvetica-Oblique u} [arcsec]"
set yl "{/Helvetica-Oblique v} [arcsec]" offset 0.5,0

tmp=0.0325
set xr [-0.015:tmp]
set yr [-0.015:tmp]
set xtics 0.01
set ytics 0.01
set size ratio -1
set zeroaxis
set ang rad
set key left

load "T0.plt"

d_pc = x_param44
f(x) = x*au/(d_pc*pc)/arcsec

set lmargin 7.5
set rmargin 0.2
set bmargin 3.2
set tmargin 0.2

p \
  sprintf("<awk '($2==-1){ x=$3; y=$4; }($2==-3) && ($1==%.10f){ print $3-x,$4-y; }' out_JDATE_heliocentric.dat", T0) u (f($1)):(f($2)) t "T_0" w p lc 0 pt 1 ps 2,\
  "<awk '($2==-1){ x=$3; y=$4; }($2==-2){ print $3-x,$4-y; }' out_JDATE_heliocentric.dat" u (f($1)):(f($2)) t "2-1" w l lt 2,\
  "<awk '($2==-1){ x=$3; y=$4; }($2==-3){ print $3-x,$4-y; }' out_JDATE_heliocentric.dat" u (f($1)):(f($2)) t "3-1" w l lt 3,\
  "<./ellipses.awk arcsec_AU2.dat" u (f($1)):(f($2)) not w l lc 'orange',\
  "chi2_SKY2.dat" u 2:3 t "residua" w l lw 3 lc 'red',\
  
q


