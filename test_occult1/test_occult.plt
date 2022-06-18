#!/usr/bin/gnuplot

deg=pi/180.

set xl "x"
set yl "y"
set zl "z"

tmp=1.2
set xr [-tmp:tmp]
set yr [-tmp:tmp]
set zr [-tmp:tmp]
set view equal xyz
set xyplane 0.0
set view 90,90,1.5
set isosamples 12+1
set hidden3d
set key

set parametric
set urange [0:360.0]
set vrange [-90.0:90.0]
fx(r,u,v) = r*cos(u*deg)*cos(v*deg)
fy(r,u,v) = r*sin(u*deg)*cos(v*deg)
fz(r,u,v) = r*sin(v*deg)

d = 1.*deg*6378.
set label sprintf(" %.0f km ", d) at fx(1,-2,45.5),fy(1,-2,45.5),fz(1,-1,45.5) right front

sp \
  "world_50m.txt" u (fx(1,$1,$2)):(fy(1,$1,$2)):(fz(1,$1,$2)) w l lc 'black' not,\
  fx(1,u,v),fy(1,u,v),fz(1,u,v) lc 'gray' not,\
  "<awk 'BEGIN{ FS=\",\"; }{ print $1,$2; }' green.dat" u (fx(1,$1,$2)):(fy(1,$1,$2)):(fz(1,$1,$2)) w lp ls 1 t "Occult",\
  "test_occult.out" u (fx(1,$2,$3)):(fy(1,$2,$3)):(fz(1,$2,$3)) w lp pt 7 lc 'orange' t "Xitau",\
  "<awk 'BEGIN{ for (i=45;i<=46;i++){ print -2,i; }}'" u (fx(1,$1,$2)):(fy(1,$1,$2)):(fz(1,$1,$2)) w l lc 'black' lw 3 not,\
  "<awk 'BEGIN{ for (i=-90;i<=90;i++){ print 0,i; }}'" u (fx(1,$1,$2)):(fy(1,$1,$2)):(fz(1,$1,$2)) w l lc 'green' not,\
  "<awk 'BEGIN{ for (i=0;i<=360;i++){ print i,51.477777; }}'" u (fx(1,$1,$2)):(fy(1,$1,$2)):(fz(1,$1,$2)) w l lc 'cyan' not,\
  "test_occult.tmp" u 2:3:4 w l not

pa -1

set term png small
set out "test_occult.png"
rep

q

  "../occult1_RADEJ2000_WORKING/test_occult.out" u (fx(1,$2,$3)):(fy(1,$2,$3)):(fz(1,$2,$3)) w lp lc 'cyan',\

