#!/usr/bin/gnuplot

set term x11

i_1 = 88.258900269300113

set xl "x [a = 1 units]"
set yl "y"
set zl "z"
set cbl "T [K]"

tmp=0.5
set xr [-tmp:1+tmp]
set yr [-tmp-0.5:tmp+0.5]
set zr [-tmp-0.5:tmp+0.5]
c1=3000.
c2=40000.
set cbr [c1:c2]

set xyplane 0
set nokey
set grid
set zeroaxis
#set size 0.666,1
set view i_1,90,1.5,1.5
set view equal xyz
set colorbox

set style line 1 lt 1
set style line 2 lt 7

set palette defined (\
  ( 3000.-c2)/(c2-c1) "orange",\
  (10000.-c2)/(c2-c1) "white",\
  (30000.-c2)/(c2-c1) "#6666ff",\
  (40000.-c2)/(c2-c1) "magenta"\
  )

sp \
   "star1.dat" u 1:2:3:8         w d ls 1 lc palette z,\
   "star1.dat" u 1:(-$2):(+$3):8 w d ls 1 lc palette z,\
   "star1.dat" u 1:(+$2):(-$3):8 w d ls 1 lc palette z,\
   "star1.dat" u 1:(-$2):(-$3):8 w d ls 1 lc palette z,\
   "star2.dat" u 1:2:3:8         w d ls 2 lc palette z,\
   "star2.dat" u 1:(-$2):(+$3):8 w d ls 2 lc palette z,\
   "star2.dat" u 1:(+$2):(-$3):8 w d ls 2 lc palette z,\
   "star2.dat" u 1:(-$2):(-$3):8 w d ls 2 lc palette z

pa -1


