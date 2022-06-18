#!/usr/bin/gnuplot

deg=pi/180.

set xl "lambda [deg]"
set yl "phi [deg]"

set xr [-7.5:7.5]
set yr [45-5:45+5]
set size ratio -1
set zeroaxis
set grid front
set key left

d = 1.0*deg*6378.
set label sprintf("  %.0f km  ", d) at -2,45.5 right front

p \
  "world_50m.txt" u 1:2 w filledcurves lc '#e3e3e3' not,\
  "world_50m.txt" u 1:2 w l lc 'black' not,\
  "bound_50m.txt" u 1:2 w l lc '#999999' not,\
  "<awk 'BEGIN{ FS=\",\"; }{ print $1,$2; }' green.dat" u 1:2 w lp ls 1 t "Occult",\
  "sora_2022-10-23_02:07:40.320.tmp" u 1:2 w lp ls 2 t "SORA",\
  "test_occult.out" u 2:3 w lp pt 7 lc 'orange' t "Xitau",\
  "<awk 'BEGIN{ for (i=0;i<=1;i++){ print -2,45+1.0*i; }}'" u 1:2 w l lc 'black' lw 3 not,\
  "<awk 'BEGIN{ for (i=-90;i<=90;i++){ print 0,i; }}'" u 1:2 w l lc 'green' not,\
  "<awk 'BEGIN{ for (i=-180;i<=180;i++){ print i,51.477777; }}'" u 1:2 w l lc 'cyan' not,\

pa -1

set term png small
set out "test_occult2.png"
rep

q


