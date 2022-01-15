#!/usr/bin/gnuplot

deg = pi/180.
arcsec = deg/3600.
au = 1.49597870700e11  # m, from IAU 2012
pc = 648000.0/pi*au  # m, from IAU 2015

set colors classic
set term x11
set size 0.66,1

set tit "viewing geometry is changing..."
set xl "u [AU]"
set yl "v [AU]"
set zl "w [AU]"
set size ratio -1

tmp=7.e-6
set xr [-tmp:tmp]
set yr [-tmp:tmp]
set zr [-tmp:tmp]
set zeroaxis
set ang rad
set xyplane 0
set view equal xyz
set view 0,0

unset colorbox
set palette gray
set cbr [1:]

load "T0.plt"

#d_pc = x_param26
#d_pc = x_param36
d_pc = x_param29
f(x) = x*arcsec*(d_pc*pc)/au

sp \
  "<awk '($2==-1)' out_JDATE_uvw.dat" u 3:4:5 t "1" w l lt 1,\
  "<awk '($2==-2)' out_JDATE_uvw.dat" u 3:4:5 t "2" w l lt 2,\
  "<awk '($2==-3)' out_JDATE_uvw.dat" u 3:4:5 t "3" w l lt 3,\
  "<awk '($2==-4)' out_JDATE_uvw.dat" u 3:4:5 t "4" w l lt 4,\
  sprintf("<awk '($2==-1) && ($1==%.10f)' out_JDATE_uvw.dat", T0) u 3:4:5 t "T_0" w p lc 0 pt 1 ps 2,\
  sprintf("<awk '($2==-2) && ($1==%.10f)' out_JDATE_uvw.dat", T0) u 3:4:5 not     w p lc 0 pt 1 ps 2,\
  sprintf("<awk '($2==-3) && ($1==%.10f)' out_JDATE_uvw.dat", T0) u 3:4:5 not     w p lc 0 pt 1 ps 2,\
  sprintf("<awk '($2==-4) && ($1==%.10f)' out_JDATE_uvw.dat", T0) u 3:4:5 not     w p lc 0 pt 1 ps 2,\
  "<awk '(FNR>1){ print $0,ARGIND; }' nodes001.dat" u (f($2)):(f($3)):(f($4)):5 t "shape" w d lc palette z

pa -1

set term png small
set out "chi2_SKY_uvw.png"
rep

q



