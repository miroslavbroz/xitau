#!/usr/bin/gnuplot

deg=pi/180.

set xl "x"
set yl "y"
set zl "z"

tmp=1.2
set xr [-tmp:tmp]
set yr [-tmp:tmp]
set zr [-tmp:tmp]
set cbr [1:3]
set view equal xyz
set xyplane 0.0
set view 90,90,1.5
set view 90,270,1.5
set isosamples 12+1
set hidden3d
set key
set nocolorbox
set palette defined (\
  0.0 '#ff6600',\
  0.5 '#00ff00',\
  1.0 '#0066ff' \
  )

set parametric
set urange [0:360.0]
set vrange [-90.0:90.0]
fx(r,u,v) = r*cos(u*deg)*cos(v*deg)
fy(r,u,v) = r*sin(u*deg)*cos(v*deg)
fz(r,u,v) = r*sin(v*deg)

d = 1.*deg*6378.
#set label sprintf(" %.0f km ", d) at fx(1,-2,45.5),fy(1,-2,45.5),fz(1,-1,45.5) right front

sp \
  "xitau/plt/world_50m.txt" u (fx(1,$1,$2)):(fy(1,$1,$2)):(fz(1,$1,$2)) w l lc 'black' not,\
  fx(1,u,v),fy(1,u,v),fz(1,u,v) lc 'gray' not,\
  "<awk 'BEGIN{ FS=\",\"; }{ print $1,$2; }' green.dat" u (fx(1,$1,$2)):(fy(1,$1,$2)):(fz(1,$1,$2)) w lp ls 1 t "Occult",\
  "<awk 'BEGIN{ FS=\",\"; }{ print $1,$2; }' green_Alexhelios.dat" u (fx(1,$1,$2)):(fy(1,$1,$2)):(fz(1,$1,$2)) w l lw 2 lc '#999999' t "outer moon (ERRONEOUS)",\
  "occultation3.dat" u (fx(1,$2,$3)):(fy(1,$2,$3)):(fz(1,$2,$3)) w l lw 2 lc 'black' not,\
  "<awk '($4==1)' occultation.dat" u (fx(1,$2,$3)):(fy(1,$2,$3)):(fz(1,$2,$3)):4 w lp pt 7 lc palette z t "Xitau",\
  "<awk '($4==2)' occultation.dat" u (fx(1,$2,$3)):(fy(1,$2,$3)):(fz(1,$2,$3)):4 w lp pt 7 lc palette z t "inner moon",\
  "<awk '($4==3)' occultation.dat" u (fx(1,$2,$3)):(fy(1,$2,$3)):(fz(1,$2,$3)):4 w lp pt 7 lc palette z t "outer moon",\
  "<awk 'BEGIN{ for (i=-90;i<=90;i++){ print 0,i; }}'" u (fx(1,$1,$2)):(fy(1,$1,$2)):(fz(1,$1,$2)) w l dt 2 lc '#666666' not

pa -1

set term png small size 2048,2048
set out "occultation.png"
rep

q

  "<awk 'BEGIN{ for (i=45;i<=46;i++){ print -2,i; }}'" u (fx(1,$1,$2)):(fy(1,$1,$2)):(fz(1,$1,$2)) w l lc 'black' lw 3 not,\

