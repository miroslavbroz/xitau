#!/usr/bin/gnuplot

set term x11
load "output.gnu"

set xl "x"
set yl "y"
set zl "z"
set cbl "I_{lambda} [W m^{-2} sr^{-1} m^{-1}]" offset 3,0

set cbr [0:]

#set view 90,0,0.5
set view 0,0,1.5
set view equal xyz
set xyplane 0.0
set palette gray
set surface hidden3d
set pm3d depthorder
set hidden3d front

set arrow from 0,0,0 to s1__,s2__,s3__ front lc 'orange'
set arrow from 0+0.01,0,0 to o1__+0.01,o2__,o3__ front lc 'blue'

sp \
  "<./pm3d.awk output.node.50 output.face.50 output.I_lambda.50" u 1:2:3:5 w pm3d not,\
  "<./poly.awk output.poly5.50" u 4:5:6 w lp lw 1 lc 'green' not,\
  "<awk '(NR>1)' output.centre.50" u 2:3:4:1 w labels tc 'brown' not,\

pa -1

set term png small size 1024,1024
set out "output.I_lambda.50.png"
rep

q



