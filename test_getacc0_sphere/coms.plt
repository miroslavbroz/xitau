#!/usr/bin/gnuplot

set xl "x [m]"
set yl "y [m]"
set zl "z [m]"

tmp=150.e3
set xr [-tmp:tmp]
set yr [-tmp:tmp]
set zr [-tmp:tmp]
set cbr [0:5]

set xyplane 0.0
set view equal xyz
set view 90,0
set view 0,0
set zeroaxis

sp \
  "coms_.out" u 1:2:3:4 w p pt 1 ps 0.5 lc palette z,\

pa -1

q

  "coms.out" u 1:2:3 w p pt 1 ps 0.5,\


