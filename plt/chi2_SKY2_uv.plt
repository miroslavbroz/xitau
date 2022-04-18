#!/usr/bin/gnuplot

deg = pi/180.
arcsec = deg/3600.
au = 1.49597870700e11  # m, from IAU 2012
pc = 648000.0/pi*au  # m, from IAU 2015

set colors classic
set term x11

set tit "viewing geometry is changing..."
set xl "u [arcsec]"
set yl "v [arcsec]"

tmp=1.8
set xr [-tmp:tmp]
set yr [-tmp:tmp]
set size ratio -1
set zeroaxis
set ang rad

load "T0.plt"

#d_pc = x_param26
#d_pc = x_param29
#f(x) = x*arcsec*(d_pc*pc)/au
#f(x,d) = x*arcsec*d
f(x,d) = x/d/arcsec

p \
  "<awk '($2==-2){ x=$3; y=$4; }($2==-3){ print $3-x,$4-y,$6; }' out_JDATE_uvw.dat" u (f($1,$3)):(f($2,$3)) t "3-2" w l lt 3,\
  sprintf("<awk '($2==-2){ x=$3; y=$4; }($2==-3) && ($1==%.10f){ print $3-x,$4-y; }' out_JDATE_uvw.dat", T0) u (f($1,$3)):(f($2,$3)) t "T_0" w p lc 0 pt 1 ps 2,\
  "arcsec_AU2.dat" u (f($2,$7)):(f($3,$7)) t "observ." w p lt 7 pt 1 ps 0.5,\
  "arcsec_AU2.dat" u (f($2,$7)):(f($3,$7)):(sprintf("  %.0f", $1-2400000)) not w labels left,\
  "<./ellipses.awk arcsec_AU2.dat" u (f($1,$3)):(f($2,$3)) not w l lt 7,\
  "chi2_SKY2.dat" u 2:3 t "residua" w l lw 3 lc 'red',\
  

pa -1

set term png small
set out "chi2_SKY2_uv.png"
rep

q

  sprintf("<awk '($2==-2){ x=$3; y=$4; }($2==-3) && ($1==%.10f){ print $3-x,$4-y; }' out_JDATE_uvw.dat", 2457948.709022) u 1:2 not w p lc 'black' pt 6 ps 2,\
