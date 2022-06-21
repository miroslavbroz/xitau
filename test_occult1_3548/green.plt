#!/usr/bin/gnuplot

deg=pi/180.

set xl "x"
set yl "y"
set zl "z"

set view equal xyz
set view 90,90
set xyplane 0.0
set isosamples 12+1
set hidden3d
set nokey

set parametric
set urange [0:360.0]
set vrange [-90.0:90.0]
fx(r,u,v) = r*cos(u*deg)*cos(v*deg)
fy(r,u,v) = r*sin(u*deg)*cos(v*deg)
fz(r,u,v) = r*sin(v*deg)

sp \
  "<awk 'BEGIN{ FS=\",\"; }{ print $1,$2; }' green.dat" u (fx(1,$1,$2)):(fy(1,$1,$2)):(fz(1,$1,$2)) w lp,\
  fx(1,u,v),fy(1,u,v),fz(1,u,v) w l lc 'gray',\
  "world_50m.txt" u (fx(1,$1,$2)):(fy(1,$1,$2)):(fz(1,$1,$2)) w l lc 'black',\
  "<awk 'BEGIN{ for (i=-90;i<=90;i++){ print 0,i; }}'" u (fx(1,$1,$2)):(fy(1,$1,$2)):(fz(1,$1,$2)) w l lc 'green',\
  "<awk 'BEGIN{ for (i=0;i<=360;i++){ print i,51.477777; }}'" u (fx(1,$1,$2)):(fy(1,$1,$2)):(fz(1,$1,$2)) w l lc 'cyan'

pa -1


