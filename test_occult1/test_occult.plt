#!/usr/bin/gnuplot

deg=pi/180.

set xl "x"
set yl "y"
set zl "z"

tmp=1.5
set xr [-tmp:tmp]
set yr [-tmp:tmp]
set zr [-tmp:tmp]
set view equal xyz
set xyplane 0.0
set view 90,90
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
  fx(1,u,v),fy(1,u,v),fz(1,u,v) lc 'gray',\
  "world_50m.txt" u (fx(1,$1,$2)):(fy(1,$1,$2)):(fz(1,$1,$2)) w l lc 'black',\
  "test_occult.out" u (fx(1,$2,$3)):(fy(1,$2,$3)):(fz(1,$2,$3)) w lp lc 'orange',\
  "<awk 'BEGIN{ for (i=-90;i<=90;i++){ print 0,i; }}'" u (fx(1,$1,$2)):(fy(1,$1,$2)):(fz(1,$1,$2)) w l lc 'green',\
  "<awk 'BEGIN{ for (i=0;i<=360;i++){ print i,51.477777; }}'" u (fx(1,$1,$2)):(fy(1,$1,$2)):(fz(1,$1,$2)) w l lc 'cyan',\
  "test_occult.tmp" u 2:3:4 w l

pa -1

q


