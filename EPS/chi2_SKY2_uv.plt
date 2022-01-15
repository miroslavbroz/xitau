#!/usr/bin/gnuplot

deg = pi/180.
arcsec = deg/3600.
au = 1.49597870700e11  # m, from IAU 2012
pc = 648000.0/pi*au  # m, from IAU 2015
km = 1.e3  # m

set term post eps enh color solid "Helvetica" 18
set out "chi2_SKY2_uv.eps"
set size 0.80,1.1

set colors classic

set tit "{/=12 viewing geometry is changing...}" offset 0,-0.5
set xl "{/Helvetica-Oblique u} [km]"
set yl "{/Helvetica-Oblique v} [km]" offset +1,0

tmp=1250.
set xr [-tmp:tmp]
set yr [-tmp:tmp]
#set xtics 4e-6
#set ytics 4e-6
set size ratio -1
set zeroaxis
set ang rad
set key at graph -0.00,0.98 left font "Helvetica,14" samplen 1.0 spacing 1.1
set grid noxtics noytics front

load "T0.plt"

d_pc = x_param26
#f(x) = x*arcsec*(d_pc*pc)/km
f(x,d) = x*arcsec*d*au/km
g(x) = x*au/km

set lmargin 8.5
set rmargin 0.0
set bmargin 3.0
set tmargin 1.2

p \
  "<awk '($2==-2){ x=$3; y=$4; }($2==-3){ print $3-x,$4-y; }' out_JDATE_uvw.dat" u (g($1)):(g($2)) t "3-2" w l lt 3 lc '#00eeee',\
  sprintf("<awk '($2==-2){ x=$3; y=$4; }($2==-3) && ($1==%.10f){ print $3-x,$4-y; }' out_JDATE_uvw.dat", T0) u (g($1)):(g($2)) t "{/Helvetica-Oblique T}_0" w p lc 0 pt 1 ps 2,\
  "arcsec_AU2.dat" u (g($2)):(g($3)) t "observ." w p lt 7 pt 1 ps 0.5,\
  "<./ellipses.awk arcsec_AU2.dat" u (g($1)):(g($2)) not w l lt 7,\
  "chi2_SKY2.dat" u (f($2,$7)):(f($3,$7)) t "residua" w l lw 3 lc 'red',\

q

  "arcsec_AU2.dat" u 2:3:(sprintf("  %.0f", $1-2400000)) not w labels left,\

  sprintf("<awk '($2==-2){ x=$3; y=$4; }($2==-3) && ($1==%.10f){ print $3-x,$4-y; }' out_JDATE_uvw.dat", 2457948.709022) u 1:2 not w p lc 'black' pt 6 ps 2,\

