#!/usr/bin/gnuplot

set term x11
load "output.gnu"

set tit "s'', o'' vectors are misaligned..."
set xl "x"
set yl "y"
set zl "z"
set cbl "Phi_i [W m^{-2} m^{-1}]" offset 3,0

set cbr [0:]
#set cbr [0:1e7]
#set nocolorbox

set view 0,0,1.5
set view equal xyz
set xyplane 0.0
set zeroaxis
set palette gray
set surface hidden3d
set pm3d depthorder
set hidden3d front

#set xl tc 'white'
#set yl tc 'white'
#set zl tc 'white'
set cbl tc 'white'
#set xtics tc 'white'
#set ytics tc 'white'
#set ztics tc 'white'
set cbtics tc 'white'
#set border lc 'white'
set object rectangle from screen 0,0 to screen 1.01,1.01 behind fc 'black' fs solid noborder

#set arrow from 0,0,0 to s1__,s2__,s3__ front lc 'orange'
#set arrow from 0+0.01,0,0 to o1__+0.01,o2__,o3__ front lc 'blue'

tmp=300000

sp \
  "<./pm3d.awk output.node.01  output.face.01  output.Phi_i.01"  u ($1+ 0*tmp):2:3:5 w pm3d not,\

pa -1

set term png small size 1024,1024
set out "output.Phi_i.01.png"
rep

q

  "<./poly.awk output.poly5.01" u 4:5:6 w lp lw 1 lc 'green' not,\
  "<./pm3d.awk output.node.10  output.face.10  output.Phi_e.10"  u ($1+ 1*tmp):2:3:5 w pm3d not,\
  "<./pm3d.awk output.node.20  output.face.20  output.Phi_e.20"  u ($1+ 2*tmp):2:3:5 w pm3d not,\
  "<./pm3d.awk output.node.30  output.face.30  output.Phi_e.30"  u ($1+ 3*tmp):2:3:5 w pm3d not,\
  "<./pm3d.awk output.node.40  output.face.40  output.Phi_e.40"  u ($1+ 4*tmp):2:3:5 w pm3d not,\
  "<./pm3d.awk output.node.50  output.face.50  output.Phi_e.50"  u ($1+ 5*tmp):2:3:5 w pm3d not,\
  "<./pm3d.awk output.node.60  output.face.60  output.Phi_e.60"  u ($1+ 6*tmp):2:3:5 w pm3d not,\
  "<./pm3d.awk output.node.70  output.face.70  output.Phi_e.70"  u ($1+ 7*tmp):2:3:5 w pm3d not,\
  "<./pm3d.awk output.node.80  output.face.80  output.Phi_e.80"  u ($1+ 8*tmp):2:3:5 w pm3d not,\
  "<./pm3d.awk output.node.90  output.face.90  output.Phi_e.90"  u ($1+ 9*tmp):2:3:5 w pm3d not,\
  "<./pm3d.awk output.node.100 output.face.100 output.Phi_e.100" u ($1+10*tmp):2:3:5 w pm3d not,\


  "<awk '(NR>1)' output.centre.01" u 2:3:4:1 w labels tc 'brown' not,\


