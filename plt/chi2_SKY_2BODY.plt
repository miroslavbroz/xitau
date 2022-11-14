#!/usr/bin/gnuplot

deg = pi/180.
arcsec = deg/3600.
au = 1.49597870700e11  # m, from IAU 2012
pc = 648000.0/pi*au  # m, from IAU 2015

set colors classic
set term x11

set xl "x [arcsec]"
set yl "y [arcsec]"
set size ratio -1

tmp=0.3
set xr [-tmp:tmp]
set yr [-tmp:tmp]
set zeroaxis
set ang rad

load "T0.plt"

#d_pc = x_param32
#d_pc = x_param33
d_pc = x_param22
f(x) = x*arcsec*(d_pc*pc)/au
g(x) = x*au/(d_pc*pc)/arcsec

print "d = ", d_pc*pc/au, " au"

p \
  "<awk '($2==-1)' out_JDATE_heliocentric.dat"  u (g($3)):(g($4)) t "1" w l lt 1,\
  "<awk '($2==-2)' out_JDATE_heliocentric.dat"  u (g($3)):(g($4)) t "2" w l lt 2,\
  "<awk '($2==-3)' out_JDATE_heliocentric.dat"  u (g($3)):(g($4)) t "3" w l lt 3,\
  "<awk '($2==-4)' out_JDATE_heliocentric3.dat" u (g($3)):(g($4)) t "4" w l lt 4,\
  sprintf("<awk '($2==-1) && ($1==%.10f)' out_JDATE_heliocentric.dat", T0)  u (g($3)):(g($4)) t "T_0" w p lc 0 pt 1 ps 2,\
  sprintf("<awk '($2==-2) && ($1==%.10f)' out_JDATE_heliocentric.dat", T0)  u (g($3)):(g($4)) not     w p lc 0 pt 1 ps 2,\
  sprintf("<awk '($2==-3) && ($1==%.10f)' out_JDATE_heliocentric.dat", T0)  u (g($3)):(g($4)) not     w p lc 0 pt 1 ps 2,\
  sprintf("<awk '($2==-4) && ($1==%.10f)' out_JDATE_heliocentric3.dat", T0) u (g($3)):(g($4)) not     w p lc 0 pt 1 ps 2,\
  "arcsec_AU.dat" u (g($2)):(g($3)) t "observ." w p lt 7 pt 1 ps 0.5,\
  "<./ellipses.awk arcsec_AU.dat" u (g($1)):(g($2)) not w l lt 7,\
  "chi2_SKY.dat" u (-$2*sin($3*deg)):($2*cos($3*deg)) t "residua" w l lt 1 lw 3
pa -1

set term png small size 2048,2048
set out "chi2_SKY_2BODY.png"
rep

q


