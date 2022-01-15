#!/usr/bin/gnuplot

set xl "x [m]"
set yl "y [m]"
set zl "z [m]"

tmp=150.e3
set xr [-tmp:tmp]
set yr [-tmp:tmp]
set zr [-tmp:tmp]
set cbr [0:5]

set xyplane relative 0.0
set view equal xyz
set view 90,0
set view 90,270
set view 0,0
set zeroaxis

sp \
  "coms2_.out" u 1:2:3 w p pt 1,\
  "r2_.out" u 1:2:3 w lp pt 1,\
  "<awk '(NR==1){ print $0; print 0,0,0; }' coms2_.out" u 1:2:3 w lp pt 1 lw 3 lc 'red',\

pa -1

q



