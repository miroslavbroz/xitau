#!/usr/bin/gnuplot

set term x11

set xl "x"
set yl "y"
set zl "z"

tmp=2.5
set xr [-tmp:tmp]
set yr [-tmp:tmp]
set zr [-tmp:tmp]

set view equal xyz
set xyplane 0.0
set surface hidden3d
set pm3d depthorder
set hidden3d front
unset colorbox
#set palette grayscale

sp \
  "<./pm2d.awk shape3.node shape3.face" u 1:2:3:($3) w pm3d not,\
  "<./face.awk shape3.node shape3.face" u 2:3:4 w lp lc 'green' not,\
  "<./face.awk shape2.node shape2.face" u 2:3:4 w lp lc 'gray' not,\


pa -1

q


