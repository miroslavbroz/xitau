#!/usr/bin/gnuplot

set term x11
load "output.gnu"

set xl "x"
set yl "y"
set zl "z"
set cbl "I_{lambda} [W m^{-2} sr^{-1} m^{-1}]" offset 3,0

tmp=1.5e5
set xr [-tmp:tmp]
set yr [-tmp:tmp]
set cbr [0:]

set view 90,0,0.5
set view 0,0,1.5
set view equal xyz
set xyplane 0.0
set zeroaxis
set palette gray
set surface hidden3d
set pm3d depthorder
set hidden3d front

scl=1.e5
set arrow from scl*(0+0.00),0,0 to scl*(s1__+0.00),scl*s2__,scl*s3__ front lc 'orange'
set arrow from scl*(0+0.01),0,0 to scl*(o1__+0.01),scl*o2__,scl*o3__ front lc 'blue'

do for [i = 1:99:1] {

  print i
  set term png small size 1024,1024
  set out sprintf("output.Phi_e.%02d.png", i)

  sp \
    sprintf("<./pm3d.awk output.node.%02d output.face.%02d output.Phi_e.%02d", i,i,i) u 1:2:3:5 w pm3d not,\
    sprintf("<./poly.awk output.poly5.%02d", i) u 4:5:6 w lp lw 1 lc 'green' not,\

}

q



