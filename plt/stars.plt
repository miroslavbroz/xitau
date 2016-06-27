#!/usr/bin/gnuplot

set term x11

i_1 = 88.258900269300113

set xl "x [a = 1 units]"
set yl "y"
set zl "z"

tmp=0.2
set xr [-tmp:1+tmp]
set yr [-tmp-0.5:tmp+0.5]
set zr [-tmp-0.5:tmp+0.5]

set xyplane 0
set nokey
set grid
set zeroaxis
set size 0.666,1
set view i_1,90,1,1

set style line 1 lt 1
set style line 2 lt 7

sp \
   "star1.dat" u 1:2:3         w d ls 1,\
   "star1.dat" u 1:(-$2):(+$3) w d ls 1,\
   "star1.dat" u 1:(+$2):(-$3) w d ls 1,\
   "star1.dat" u 1:(-$2):(-$3) w d ls 1,\
   "star2.dat" u 1:2:3         w d ls 2,\
   "star2.dat" u 1:(-$2):(+$3) w d ls 2,\
   "star2.dat" u 1:(+$2):(-$3) w d ls 2,\
   "star2.dat" u 1:(-$2):(-$3) w d ls 2

pa -1


