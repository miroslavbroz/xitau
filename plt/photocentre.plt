#!/usr/bin/gnuplot

deg = pi/180.
arcsec = deg/3600.

set xl "u [arcsec]"
set yl "v [arcsec]"

tmp=0.10
set xr [-tmp:tmp]
set yr [-tmp:tmp]

set view 0,0,1.5
set view equal xyz
set xyplane 0.0
set zeroaxis
set palette gray
set surface hidden3d
set pm3d depthorder
set hidden3d front
set tics front

sp \
  "<./pm3d.awk output.arcsec.01 output.face.01 output.Phi_e.01" u 1:2:3:5 w pm3d not,\
  "photocentre.dat" u 2:3:(0.15) w l,\

pa -1

q

  "<./poly.awk output.poly5.01" u 4:5:6 w lp lw 1 lc 'green' not,\

