#!/usr/bin/gnuplot

deg=pi/180.

set xl "x"
set yl "y"

tmp=0.225
set xr [-tmp:tmp]
set yr [-tmp:tmp]
set cbr [0.5:2.5]
set size ratio -1
set zeroaxis
set grid front
unset grid
set key left
set nocolorbox
set palette rgbformulae 33,13,10

# https://mathworld.wolfram.com/GnomonicProjection.html

lambda0 = 0.0*deg
phi0 = 47.5*deg
f(x) = abs(x*deg-lambda0) < 90*deg ? x*deg : NaN
g(x) = x*deg
fx(lambda,phi) = cos(g(phi))*sin(f(lambda)-lambda0)
fy(lambda,phi) = (cos(phi0)*sin(g(phi))-sin(phi0)*cos(g(phi))*cos(f(lambda)-lambda0))/(sin(phi0)*sin(g(phi))+cos(phi0)*cos(g(phi))*cos(f(lambda)-lambda0))

d = 1.0*deg*6378.
set label sprintf("  1 deg = %.0f km  ", d) at fx(-2,45.5),fy(-2,45.5) right front

p \
  "xitau/plt/world_50m.txt" u (fx($1,$2)):(fy($1,$2)) w filledcurves lc '#e3e3e3' not,\
  "xitau/plt/world_50m.txt" u (fx($1,$2)):(fy($1,$2)) w l lc 'black' not,\
  "xitau/plt/bound_50m.txt" u (fx($1,$2)):(fy($1,$2)) w l lc '#999999' not,\
  "<awk 'BEGIN{ FS=\",\"; }{ print $1,$2; }' green.dat" u (fx($1,$2)):(fy($1,$2)) w lp ls 1 t "Occult",\
  "sora_2022-10-23_02:07:40.320.tmp" u (fx($1,$2)):(fy($1,$2)) w lp ls 2 t "SORA",\
  "<awk '($4==1)' occultation.dat" u (fx($2,$3)):(fy($2,$3)):4 w lp pt 7 lc palette z t "Xitau",\
  "<awk '($4>=2)' occultation.dat" u (fx($2,$3)):(fy($2,$3)):4 w lp pt 7 lc palette z t "moon(s)",\
  "<awk 'BEGIN{ for (i=0;i<=1;i++){ print -2,45+1.0*i; }}'" u (fx($1,$2)):(fy($1,$2)) w l lc 'black' lw 3 not,\
  "<awk 'BEGIN{ for (i=-90;i<=90;i++){ print 0,i; }}'" u (fx($1,$2)):(fy($1,$2)) w l lc 'green' not,\
  "<awk 'BEGIN{ for (i=-180;i<=180;i++){ print i,51.477777; }}'" u (fx($1,$2)):(fy($1,$2)) w l lc 'cyan' not,\

pa -1

set term png small size 2048,2048
set out "occultation_GNOMONIC.png"
rep

q


