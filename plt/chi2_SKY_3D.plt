#!/usr/bin/gnuplot

deg = pi/180.
arcsec = deg/3600.
au = 1.49597870700e11  # m, from IAU 2012
pc = 648000.0/pi*au  # m, from IAU 2015

set colors classic
set term x11
set size 0.66,1

set xl "x [AU]"
set yl "y [AU]"
set zl "z [AU]"
set cbl "dataset"
set size ratio -1
set nokey

tmp=1.0e-5
tmp=0.05
set xr [-tmp:tmp]
set yr [-tmp:tmp]
set zr [-tmp:tmp]
set zeroaxis
set ang rad
set xyplane 0
set view equal xyz
set view 0,0
set view 81,194  # from hatw, eclipses
set colorbox
set palette defined (\
  0.0 '#000000',\
  1.0 '#eeeeee' \
)
set cbr [1:]

load "T0.plt"

d_pc = x_param19
#d_pc = x_param26
#d_pc = x_param36
f(x) = x*arcsec*(d_pc*pc)/au

sp \
  sprintf("<awk '($2==-1) && ($1>=%.10f) && ($1<=%.10f)' out_JDATE_heliocentric.dat", T0, T0+0.1) u 3:4:5 not w l lw 10 lc 'gray',\
  sprintf("<awk '($2==-2) && ($1>=%.10f) && ($1<=%.10f)' out_JDATE_heliocentric.dat", T0, T0+0.1) u 3:4:5 not w l lw 10 lc 'gray',\
  sprintf("<awk '($2==-3) && ($1>=%.10f) && ($1<=%.10f)' out_JDATE_heliocentric.dat", T0, T0+0.1) u 3:4:5 not w l lw 10 lc 'gray',\
  "<awk '($2==-1)' out_JDATE_heliocentric.dat"  u 3:4:5 t "1" w l lt 1,\
  "<awk '($2==-2)' out_JDATE_heliocentric.dat"  u 3:4:5 t "2" w l lt 2,\
  "<awk '($2==-3)' out_JDATE_heliocentric.dat"  u 3:4:5 t "3" w l lt 3,\
  "<awk '($2==-4)' out_JDATE_heliocentric3.dat" u 3:4:5 t "4" w l lt 4,\
  sprintf("<awk '($2==-1) && ($1==%.10f)' out_JDATE_heliocentric.dat", T0)  u 3:4:5 t "T_0" w p lc 0 pt 1 ps 2,\
  sprintf("<awk '($2==-2) && ($1==%.10f)' out_JDATE_heliocentric.dat", T0)  u 3:4:5 not     w p lc 0 pt 1 ps 2,\
  sprintf("<awk '($2==-3) && ($1==%.10f)' out_JDATE_heliocentric.dat", T0)  u 3:4:5 not     w p lc 0 pt 1 ps 2,\
  sprintf("<awk '($2==-4) && ($1==%.10f)' out_JDATE_heliocentric3.dat", T0) u 3:4:5 not     w p lc 0 pt 1 ps 2,\
  "<awk '(NF==0) || ($4==0) || ($4==1)' uvw.dat" u ($1*tmp):($2*tmp):($3*tmp) t "u" w l lc 'orange'  ,\
  "<awk '(NF==0) || ($4==0) || ($4==2)' uvw.dat" u ($1*tmp):($2*tmp):($3*tmp) t "v" w l lc 'yellow',\
  "<awk '(NF==0) || ($4==0) || ($4==3)' uvw.dat" u ($1*tmp):($2*tmp):($3*tmp) t "w" w l lc 'cyan' ,\
  "<awk '(FNR>1){ print $0,ARGIND; }' nodes*.ecl" u 2:3:4:5 t "shape" w d lc palette z

pa -1

set term png small
set out "chi2_SKY_3D.png"
rep

q

  "<awk '(FNR>1){ print $0,ARGIND; }' nodes*.tmp" u (g($2)):(g($3)):(g($4)):5 t "shape" w d lc palette z


